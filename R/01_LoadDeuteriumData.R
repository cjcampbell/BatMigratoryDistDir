
# Setup ----------------------------------------------------------------

georef <- FALSE

if(georef == TRUE){
  library(ggmap)
  if(!has_google_key()){
    myAPIkey <- read.table("~/googleAPIkey.txt") %>% unlist %>% paste
    register_google(key = myAPIkey)
    if(!has_google_key()) stop("Google API Key not found. See '?ggmap::register_google`.")
  }
}

reload_museum <- FALSE
if(reload_museum == TRUE){
  library(rgbif)
}

# Functions ---------------------------------------------------------------

# Function to make first letter uppercase
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

adjust_OldSA.1_H_1 <- function(d2H) {
  # Function that serves as a wrapper for assignR::refTrans for a specific
  # standard scale.

  # Abritrary sd (currently not used in this wrapper).
  s <- data.frame("d2H" = d2H, "d2H.sd" = rep(2), "d2H_cal" = "OldEC.1_H_1")
  d1 <- assignR::refTrans(s)
  return(d1$data$d2H)
}

adjust_DEN_H_2 <- function(d2H) {
  s <- data.frame("d2H" = d2H, d2H.sd = rep(2), "d2H_cal" = "DEN_H_2")
  d1 <- assignR::refTrans(s)
  return(d1$data$d2H)
}

adjust_Denver_prior2004 <- function(d2H) {
  New_d2H  = (0.925247*d2H) - 19.79
  vsmow <- adjust_DEN_H_2(New_d2H)
  return(vsmow)

}

adjust_US_H_1 <- function(d2H) {
  s <- data.frame("d2H" = d2H, "d2H.sd" = rep(2), "d2H_cal" = "US_H_1")
  d1 <- assignR::refTrans(s)
  return(d1$data$d2H)
}
adjust_EC_H_5 <- function(d2H) {
  s <- data.frame("d2H" = d2H, "d2H.sd" = rep(2), "d2H_cal" = "EC_H_5")
  d1 <- assignR::refTrans(s)
  return(d1$data$d2H)
}



# Load CASIF-generated dataset. -------------------------------------------

CASIF_BatMigratoryStructure_dat <- read.csv(
  file.path(wd$data, "CASIF_BatMigratoryStructure_dat.csv"),
  stringsAsFactors = FALSE
) %>%
  dplyr::rename(
    Lat_dd = Lat,
    Lon_dd = Lon
  )

## Load data from literature -----------------------------------------------

## There are 4 data sources from the literature:
# Baerwald et al
# Cryan et al
# Fraser et al.
# Pylant et al. (a)
# Pylant et al. (b)

# Baerwald et al. data -----------------------------------------------------------

dat_BAERWALD <- list.files(
  pattern = "*Baerwald",
  path = wd$litdata, recursive = F, full.names = T
) %>%
  lapply( read.csv, stringsAsFactors = FALSE, na.strings=c("", "NA")) %>% # Load all dataframes matching this pattern...
  plyr::ldply() %>% # ... and combine.
  dplyr::rename_all(firstup) %>%
  dplyr::mutate(
    source_ID = paste0("Baerwald", row_number()), # Add individual ID specific to this source
    Lat_dd = if_else(Location == "Summerview", 49.5536552, as.numeric(NA)), # Mapped to specific facility.
    Lon_dd = if_else(Location == "Summerview", -113.9636443, as.numeric(NA)), # Mapped to specific facility.
    sampling_method = "TURBINE_CARCASS",
    Sourcefile = "Baerwald"
  ) %>%
  dplyr::rename(
    d2H = Dd
  ) %>%
  dplyr::select(Species, Sex, Age, Lat_dd, Lon_dd, Day, Month, Year, d2H, sampling_method, source_ID, Sourcefile)

# Cryan et al. data --------------------------------------------------------------

dat_CRYAN <- list.files(
  pattern = "*cryanData.csv",
  path = wd$litdata, recursive = F, full.names = T
) %>%
  lapply( read.csv, stringsAsFactors = FALSE, na.strings=c("", "NA")) %>%
  plyr::ldply() %>%
  dplyr::rename_all(firstup) %>%
  dplyr::mutate(turbine_killed = Turbine ) %>%
  dplyr::mutate(
    Sourcefile = "Cryan",
    Species = "LACI",
    sampling_method = if_else( turbine_killed == 1, "TURBINE_CARCASS", "LIVE_OR_MUSEUM_SPECIMEN"),
    Sex = Sex.age,
    Age = Sex.age,
    source_ID = paste0("CRYAN", New_id),
  ) %>%
  dplyr::rename(
    Lat_dd = Latitude,
    Lon_dd = Longitude,
    d2H = Dd_fur
  ) %>%
  dplyr::select(Species, Sex, Age, Lat_dd, Lon_dd, Day, Month, Year, d2H, sampling_method, source_ID, Sourcefile)


# Fraser et al. data -------------------------------------------------------------

# This one is trickier because there aren't metadata immediately available.
# I have to match the specimen.id to GBIF data!

frdat <- list.files(
  pattern = "*Fraser", path = wd$litdata, recursive = F, full.names = T
) %>%
  lapply( read.csv, stringsAsFactors = FALSE, na.strings=c("", "NA")) %>%
  plyr::ldply() %>%
  mutate( Species = as.character( plyr::revalue( Species, c( "LANO" = "Lasionycteris noctivagans") ) ) )

# Load data, scrape GBIF
if(reload_museum == TRUE){

  # Isolate institution codes and specimen ids
  frdat$InstitutionCode <- stringr::str_extract( frdat$Specimen.ID, "[A-Z]+" )
  frdat$specID <- stringr::str_extract(frdat$Specimen.ID, "\\d+")

  frdat2 <- lapply(1:nrow(frdat), function(z){
    l <- rgbif::occ_search(
      scientificName = frdat[z,"Species"],
      institutionCode = frdat[z, "InstitutionCode"],
      catalogNumber = frdat[z, "specID"],
      limit = 1)
    if(l$meta$count == 1){
      merge( frdat[z,], l$data )
    } else {
      return( frdat[z,] ) }
    print(paste("On record", z, "of", nrow(frdat)) )
  }) %>% plyr::ldply()

  # Remove entry for which there are no matching records.
  frdat2 <- frdat2 %>% filter(!is.na(name))

  write.csv(frdat2, file = file.path( wd$bin, "Fraserdata-museum.csv") )

}

# Georeference addresses.
if(georef == TRUE){

  frdat2 <- read.csv(
    file.path( wd$bin, "Fraserdata-museum.csv"),
    header = T, na.strings=c("", "NA"), stringsAsFactors = FALSE
  )

  # ID specimens without lat/lon information.
  targetSpecimens <- frdat2$X[which(is.na(frdat2$decimalLatitude))]
  # Clean lookup data for geocoding.
  targetAddresses <- lapply(1:length(targetSpecimens), function(i){
    tRow <- lapply(frdat2[which(frdat2$X == targetSpecimens[i]),], as.character)
    tAddress <- toString(c(
      if(!is.na(tRow$locality)) tRow$locality,
      if(!is.na(tRow$county)) paste(tRow$county, " County"),
      if(!is.na(tRow$stateProvince)) tRow$stateProvince,
      if(!is.na(tRow$countryCode)) tRow$countryCode ) )
    return(tAddress)
  })

  # Where necessary, modify targetAddresses manually.
  # A good tool to double-check how well addresses fit applies the following function.
  # googleMapLookup <- function(text){browseURL(paste0("https://www.google.com/maps/search/?api=1&query=", text)) }
  # lapply(unique(targetAddresses), googleMapLookup)

  changeKey <- as.data.frame(rbind(
    c("Squaw Shoals, Warrior River, Alabama, US", "33.464000, -87.351389"),
    c("Toccoa, 3 Mi N, Georgia, US", "Toccoa, Georgia, US"),
    c("Keiger Gorge, Steen Mountains, Oregon, US", "Kiger Gorge, OR"),
    c("Pisgah National Forest, Bent Creek, North Carolina, US", "Bent Creek, North Carolina, US"),
    c("Magnetic City, North Carolina, US", "Mitchell County, North Carolina, US"),
    c("Florence, 2 Mi SW, Montana, US", "Florence, Montana, US"),
    c("Mountain Lake, 5 Mi N, Headwaters Of Little Stony Creek, Giles County  County, Virginia, US", "Mountain Lake, Giles County, Virginia, US"),
    c("Roundtop Mountain, Washington County  County, Maryland, US", "Roundtop Hill, 5, Hancock, Washington County, MD"),
    c("Maryville, 4 Mi SE, Nodaway County  County, Missouri, US", "Maryville, Nodaway County  County, Missouri, US"),
    c("Jay, 5 Miles West, Escambia River Floodplains, Santa Rosa County  County, Florida, US", "Jay, Santa Rosa County, Florida, US"),
    c("Washington, National Museum of Natural History, 10th Street and Consitition Avenue, NW, found dying at west side of building., District of Columbia, US", "10th Street and Consitition Avenue, District of Columbia, US"),
    c("Washington, intersection of Independence Avenue NW 900 and L'Enfant Place SW 300 at James Forrestal Building, District of Columbia, US", "James Forrestal Building, District of Columbia, US"),
    c("Mount Thielson, E Of; West Sink Creek; E Base Cascade Mountains, Oregon, US", "Mount Thielsen, Oregon"),
    c("Glenwood, 4900 ft, Catron  County, New Mexico, US", "Glenwood, Catron County, New Mexico, US"),
    c("McKittrick Canyon, Guadalupe Mtns. Natl. Park, Culberson  County, Texas, US", "McKittrick Canyon Visitor Center, Salt Flat, TX"),
    c("Kanabownits Spring, Grand Canyon National Park,  8000 ft, Coconino  County, Arizona, US", "Kanabownits Spring"),
    c("Great Dismal Swamp National Wildlife Refuge, Intersection Of Jericho Lane And Hudnell Ditch, Suffolk  County, Virginia, US", "Great Dismal Swamp National Wildlife Refuge, Virginia, US"),
    c("Great Dismal Swamp National Wildlife Refuge, 1.7 Mi N Intersection Of West Ditch And Interior Ditch, Suffolk  County, Virginia, US", "Great Dismal Swamp National Wildlife Refuge, Virginia, US")
  ))

  for(n in 1:nrow(changeKey)) {
    targetAddresses[targetAddresses == changeKey$V1[n]] <- as.character(changeKey$V2[n])
  }

  addresses <- unique( targetAddresses)

  coded <- lapply(addresses, function(i){
    x <- geocode(
      i, output = "latlona", messaging = F,
      override_limit = T, force = T, source = "google"
    )
    while(is.na(x$lon)){
      Sys.sleep(10)
      x <- geocode(
        i, output = "latlona", messaging = F,
        override_limit = T, force = T, source = "google"
      )
    }
    Sys.sleep(1)
    tb <- cbind(ADDRESS = i, x)
    return(tb)
  } )

  geo <- plyr::ldply(coded)

  geoCoded <- left_join(
    data.frame(
      "X" = as.numeric(targetSpecimens),
      "ADDRESS" = as.character(targetAddresses)
    ),
    geo, by = "ADDRESS")

  frdat3 <- left_join(frdat2, geoCoded, by="X") %>%
    mutate(
      lon = ifelse( !is.na(decimalLongitude), decimalLongitude, ifelse(!is.na(lon), lon, NA) ),
      lat = ifelse( !is.na(decimalLatitude), decimalLatitude, ifelse(!is.na(lat), lat, NA) )
    )

  write.csv(frdat3, file = file.path(wd$bin, "Fraserdata-georeferenced.csv") )

}

dat_FRASER <- read.csv(
  file = file.path(wd$bin, "Fraserdata-georeferenced.csv"),
  header = T, na.strings=c("", "NA"),
  stringsAsFactors = FALSE
) %>%
  dplyr::select(-sex) %>%
  dplyr::mutate(
    Month = if_else( !is.na(month), as.numeric(month), as.numeric( chron::month.day.year(Collection.date..Julian.date.)$month )  ),
    Day = if_else( !is.na(day), as.numeric(day), as.numeric( chron::month.day.year(Collection.date..Julian.date.)$day ) ),
    Year = if_else( !is.na(year), as.numeric(year), as.numeric(Collection.year) ) ,
    source_ID = paste0("FRASER", X),
    Sourcefile = "Fraser",
    sampling_method = "MUSEUM_SPECIMEN",
    Age = NA
  ) %>%
  dplyr::rename(
    Sex = Specimen.sex,
    Lat_dd = lat,
    Lon_dd = lon,
    d2H = dDFur
  ) %>%
  dplyr::select(Species, Sex, Age, Lat_dd, Lon_dd, Day, Month, Year, d2H, sampling_method, source_ID, Sourcefile)


# Pylant et al. (a) LABO data --------------------------------------------------

dat_PYLANT_a <- list.files(
  pattern = "Pylant_LABO_dat",
  path = wd$litdata, recursive = F, full.names = T
) %>%
  lapply( read.csv, stringsAsFactors = FALSE, na.strings=c("", "NA")) %>%
  plyr::ldply() %>%
  dplyr::rename_all(firstup) %>%
  dplyr::mutate(
    Sourcefile = "Pylant_LABO",
    sampling_method = "MUSEUM_SPECIMEN",
    source_ID = paste0("Pylant_a_LABO_", row_number())
  ) %>%
  dplyr::rename(
    Lat_dd = Coords_n,
    Lon_dd = Coords_w,
    d2H = Dd.values,
    Species = Spp
  ) %>%
  dplyr::select(Species, Sex, Age, Lat_dd, Lon_dd, Day, Month, Year, d2H, sampling_method, source_ID, Sourcefile)


# Pylant et al. (b) data --------------------------------------------------

dat_PYLANT_b <- list.files(
  pattern = "Pylant data.csv",
  path = wd$litdata, recursive = F, full.names = T
) %>%
  lapply( read.csv, stringsAsFactors = FALSE, na.strings=c("", "NA")) %>%
  plyr::ldply() %>%
  dplyr::rename_all(firstup) %>%
  mutate( # Get coordinates for each site.
    Lat_dd = as.numeric(NA),
    Lon_dd = as.numeric(NA),

    # Coordinates available on wikipedia:
    # https://en.wikipedia.org/wiki/Mount_Storm_Wind_Farm
    # https://tools.wmflabs.org/geohack/geohack.php?pagename=Mount_Storm_Wind_Farm&params=39_13_28_N_79_12_15_W_type:landmark
    Lat_dd = if_else(Site.id == "MTSTORM_WV", 39.224444,  Lat_dd),
    Lon_dd = if_else(Site.id == "MTSTORM_WV", -79.204167, Lon_dd),

    # BKBN_MD is Criterion wind energy project
    # See description in Pylant et al.:
    # http://www.uvm.edu/~kellrlab/LabManuscripts/Pylant_et_al-2016-Ecological_Applications.pdf
    # Coordinates based on this site:
    # https://www.thewindpower.net/windfarm_en_11011_criterion.php
    # And clicking through to google maps to manually vet.
    Lat_dd = if_else(Site.id == "BKBN_MD", 39.3494305,  Lat_dd),
    Lon_dd = if_else(Site.id == "BKBN_MD", -79.3673039, Lon_dd),

    # Georeferenced to site with google maps, with address:
    # "Mountaineer Wind Energy Center, Tucker County, West Virginia"
    Lat_dd = if_else(Site.id == "MOUNTAINEER_WV", 39.177053, Lat_dd),
    Lon_dd = if_else(Site.id == "MOUNTAINEER_WV", -79.543071, Lon_dd),

    # Used the centroids (?) available on the maps here:
    # https://www.pgc.pa.gov/InformationResources/AboutUs/ContactInformation/Pages/default.aspx
    Lat_dd = if_else(Site.id == "PAGC_SW", 40.314369,  Lat_dd),
    Lon_dd = if_else(Site.id == "PAGC_SW", -79.148909, Lon_dd),
    Lat_dd = if_else(Site.id == "PAGC_SC", 40.494857,  Lat_dd),
    Lon_dd = if_else(Site.id == "PAGC_SC", -78.044987, Lon_dd),
    Lat_dd = if_else(Site.id == "PAGC_NE", 41.342326,  Lat_dd),
    Lon_dd = if_else(Site.id == "PAGC_NE", -75.991273, Lon_dd),

  ) %>%
  dplyr::rename(
    d2H = Dd,
    ID_CP = Id_cp
  ) %>%
  mutate(
    Sourcefile = "Pylant_b",
    sampling_method = "TURBINE_CARCASS",
    source_ID = paste0("ID_CP", row_number()),
    Month = if_else( grepl("_", ID_CP) & nchar(ID_CP) > 10, substr(ID_CP, 1, 2), as.character(NA) ),
    Day = if_else( grepl("_", ID_CP) & nchar(ID_CP) > 10, substr(ID_CP, 3, 4), as.character(NA) ),
    Year = if_else( grepl("_", ID_CP) & nchar(ID_CP) > 10, substr(ID_CP, 5, 6), as.character(NA) )
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Year = if_else(!is.na(Year), 2000 + Year, as.numeric(NA))
  ) %>%
  dplyr::select(Species, Sex, Age, Lat_dd, Lon_dd, Day, Month, Year, d2H, sampling_method, source_ID, Sourcefile)



## Combine and clean data --------------------------------------------------

combo_dat <- list(CASIF_BatMigratoryStructure_dat, dat_BAERWALD, dat_CRYAN, dat_FRASER, dat_PYLANT_a, dat_PYLANT_b) %>%
  plyr::join_all(type = "full") %>%
  dplyr::select(-X)

# Reclassify
alldat <- combo_dat %>%
  mutate_at(c("Lat_dd", "Lon_dd", "Day","Month", "Year", "d2H"), as.numeric) %>%
  dplyr::mutate(
    Species = dplyr::recode(
      Species,
      "ERBA" = "LABO", "Lasiurus borealis" = "LABO",
      "HOBA" = "LACI", "Lasiurus cinereus" = "LACI", "Hoary bat" = "LACI",
      "SHBA" = "LANO", "Lasionycteris noctivagans" = "LANO"
    ),
    Sex = toupper(Sex),
    Sex = dplyr::recode(
      Sex,
      "FA" = "F", "FJ" = "F", "FEMALE" = "F",
      "MA" = "M", "MJ" = "M", "MALE" = "M", "M?" = "M", "M " = "M",
      "U" = as.character(NA), "UJ" = as.character(NA), "UA" = as.character(NA), "UNKNOWN " = as.character(NA), "UNKNOWN" = as.character(NA)
    ),

    Age = toupper(Age),
    Age = dplyr::recode(
      Age,
      "ADULT" = "A", "FA" = "A", "MA" = "A", "UA" = "A",
      "JUVENILE" = "J", "FJ" = "J", "MJ" = "J", "UJ" = "J",
      "U" = as.character(NA), "UNKNOWN" = as.character(NA)
    )
  ) %>%
  # Add yday element.
  dplyr::mutate( yDay = yday ( as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d") ) ) %>%
  # Make sure all samples have d2H values! Some LANO from Pylant et al don't...
  dplyr::filter(!is.na(d2H)) %>%
  # Rename coordinates to DarwinCore standard.
  dplyr::rename(
    decimalLatitude = Lat_dd,
    decimalLongitude = Lon_dd
  ) %>%
  # Make sure all samples have useful ID codes
  dplyr::group_by(Sourcefile) %>%
  dplyr::arrange(source_ID, .by_group = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( ID = make.names( row_number() ) ) %>%
  dplyr::select(ID, everything()) %>%
  # Rename d2H in preparation for standard-transfer function application:
  dplyr::rename(d2H_raw = d2H) %>%
  # Add wind_killed column
  dplyr::mutate(wind_killed = as.factor( if_else(sampling_method %in% c("TURBINE_CARCASS", "turbine_carcass"), "yes", "no" ) ) )

# Convert to new projection -----------------------------------------------
# I'll retain decimal lat/lons for later convenience though.
alldat <- alldat %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>%
  st_transform(crs = myCRS) %>%
  st_coordinates() %>%
  as.data.frame %>%
  dplyr::rename(
    metersLatitude  = Y,
    metersLongitude = X
  ) %>%
  cbind(alldat) %>%
  dplyr::select(ID, Species, ends_with("tude"), everything() )


# Apply standard-transfer correction functions ---------------------------------------

mydata <- alldat %>%
  dplyr::mutate( d2H = case_when(
    Sourcefile == "Campbell"               ~ d2H_raw,
    Sourcefile == "Fraser"                 ~ adjust_OldSA.1_H_1(d2H_raw),
    Sourcefile == "Cryan" & Year <= 2004   ~ adjust_Denver_prior2004(d2H_raw),
    Sourcefile == "Cryan" & Year > 2004    ~ adjust_DEN_H_2(d2H_raw),
    Sourcefile == "Baerwald"               ~ adjust_EC_H_5(d2H_raw),
    Sourcefile == "Pylant_LABO"            ~ adjust_US_H_1(d2H_raw),
    Sourcefile == "Pylant_b"               ~ adjust_US_H_1(d2H_raw),
    TRUE ~ d2H_raw
  )) %>%
  dplyr::select(-d2H_raw)

# Save --------------------------------------------------------------------

write.csv(mydata, file = file.path(wd$data, "alldat.csv"))

