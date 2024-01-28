
# Setup -------------------------------------------------------------------
download_GADM <- FALSE

# This script assumes that there are candidate isoscapes downloaded from isoMAP
# somewhere in the wd$data directory.
reload_isoscapes <- FALSE

# This script assumes that there are IUCN rangemaps somewhere in the wd$iucn
# directory.
reload_IUCN_rangemaps <- FALSE

# Load GADM data ----------------------------------------------------------
if(download_GADM == TRUE){

  message("Loading GADM data...")

  # Get GADM data to state level.
  USA <- raster::getData('GADM', path = wd$bin, country='USA', level=0)
  MEX <- raster::getData('GADM', path = wd$bin, country='MEX', level=0)
  CAN <- raster::getData('GADM', path = wd$bin, country='CAN', level=0)
  GTM <- raster::getData('GADM', path = wd$bin, country='GTM', level=0)

  # Prepare to remove areas outside of desired extent.
  # ## Remove Hawaii
  USA_1 <- raster::getData('GADM', path = wd$bin, country='USA', level=1)
  hawaii <- USA_1[USA_1@data$NAME_1 == "Hawaii",] # Select Hawaii
  hawaii_simpl <- hawaii %>%
    sf::st_as_sf() %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5000) %>%
    st_buffer(dist = 1e6)
  # ## Remove water bodies.
  USA_2 <- raster::getData('GADM', path = wd$bin, country='USA', level=2)
  MEX_2 <- raster::getData('GADM', path = wd$bin, country='MEX', level=2)
  CAN_2 <- raster::getData('GADM', path = wd$bin, country='CAN', level=2)
  GTM_2 <- raster::getData('GADM', path = wd$bin, country='GTM', level=2)
  waterbodies <- lapply(list(USA_2,MEX_2,CAN_2,GTM_2), function(x){
    x[(x$ENGTYPE_2) == "Water body",]
  }) %>%
    do.call(rbind, .) %>%
    sf::st_as_sf() %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5000)

  # Combine into one polygon, convert to sf object.
  NoAm <- raster::bind(
    MEX, USA, CAN, GTM#, BLZ, SLV, HND, NIC
  )
  saveRDS(NoAm, file = file.path(wd$bin, "NoAm.rds"))

  NoAm_boundary_aea <- sf::st_as_sf(NoAm) %>%
    st_transform(crs = myCRS) %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 5e3) %>%
    st_difference(., hawaii_simpl) %>% # Remove Hawaii
    st_buffer(dist = 5e4) %>%
    rmapshaper::ms_erase(., waterbodies)  # Remove water bodies

  saveRDS(NoAm_boundary_aea, file = file.path(wd$bin, "NoAm_boundary_aea.rds"))

} else message("Not redownloading GADM Data...")


# Load isoscapes ----------------------------------------------------------

if(reload_isoscapes == TRUE){
  message("reloading isoscapes...")

  # Load NoAm boundary.
  NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )

  # Load then write assignR's isoscape. Could call directly, but this works okay.
  assignRpathPattern <- "assignR_d2h_world"
  GGS_H <- assignR::d2h_world
  if( !dir.exists(file.path(wd$data, assignRpathPattern)) ) {
    dir.create(file.path(wd$data, assignRpathPattern))
  }
  writeRaster(GGS_H[[1]], overwrite = TRUE,
              filename = file.path(wd$data, assignRpathPattern, "isoscape.tif"))
  writeRaster(GGS_H[[2]], overwrite = TRUE,
              filename = file.path(wd$data, assignRpathPattern, "sd.tif"))

  # Begin by preparing the candidate isoscapes to be stacked together.

  my.isoscapes.loaded <- mapply(
    FUN = function(path_pattern, isoscape_pattern,sd_pattern) {

      suppressWarnings( {
        # Load details for each isoscape
        l <- list()
        l$directory          <- directory
        l$path_pattern       <- path_pattern
        l$isoscape_pattern   <- isoscape_pattern
        l$sd_pattern         <- sd_pattern
        l$reference_isoscape_path <- reference_isoscape_path
        l$isoscape <- list.files( directory, recursive = TRUE,
                                  pattern = isoscape_pattern,
                                  full.names = TRUE ) %>%
          grep(path_pattern, ., value = TRUE) %>%
          raster::raster(.)
        l$sd <- list.files( directory, recursive = TRUE,
                            pattern = sd_pattern,
                            full.names = TRUE) %>%
          grep(path_pattern, ., value = TRUE) %>%
          raster::raster(.)
      }
      )

      return(l)
    },
    path_pattern = c("66098", "66100", assignRpathPattern),
    isoscape_pattern = c(rep("predkrig.tiff$", 2), "isoscape.tif$"),
    sd_pattern = c(rep("stdkrig.tiff$", 2), "sd.tif$"),
    SIMPLIFY = FALSE
  )

  refIsoscape <- my.isoscapes.loaded[[3]]$isoscape %>%
    raster::projectRaster(., crs = myCRS) %>%
    raster::extend( ., my_extent_aea ) %>%
    raster::crop( ., my_extent_aea ) %>%
    raster::mask( ., NoAm_boundary_aea  )

  my_isoscapes <- my.isoscapes.loaded
  for(i in 1:length(my.isoscapes.loaded) ) {
    my_isoscapes[[i]]$isoscape <- my_isoscapes[[i]]$isoscape %>%
      raster::projectRaster(., crs = myCRS) %>%
      raster::extend( ., my_extent_aea ) %>%
      raster::crop(   ., my_extent_aea ) %>%
      raster::resample(., refIsoscape) %>%
      raster::mask(., refIsoscape)
    my_isoscapes[[i]]$sd <- my_isoscapes[[i]]$sd %>%
      raster::projectRaster(., crs = myCRS) %>%
      raster::extend( ., my_extent_aea ) %>%
      raster::crop(   ., my_extent_aea ) %>%
      raster::resample(., refIsoscape) %>%
      raster::mask(., refIsoscape)
  }

  # Check if everything turned out okay.
  lapply(my_isoscapes, function(i) c( i$isoscape, i$sd) ) %>%
    unlist %>%
    lapply(., compareRaster, x = .[[1]] ) %>%
    unlist %>%
    all %>%
    {if(.!=TRUE) stop("Something is wrong! CompareRaster yeilds FALSE results.")}

  # Save.
  save(my_isoscapes, file = file.path(wd$bin, "my_isoscapes.RData"))
} else message("Not reloading isoscapes, loading saved version...")


# Load and buffer IUCN Rangemaps -----------------------------------------------------
if(reload_IUCN_rangemaps == TRUE){
  load(file.path(wd$bin, "my_isoscapes.RData"), verbose = TRUE)
  NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )

  # Load candidate rangemaps.
  ranges_NoAmBats <- rgdal::readOGR(dsn = wd$iucn, layer = "data_0")

  # Filter to species of interest.
  # This analysis is focused on extant species in Continental North America,
  # so filter out extinct and island populations.
  ranges_mySpecies <- ranges_NoAmBats[ ranges_NoAmBats$BINOMIAL %in% binoms , ] %>%
    .[.$LEGEND != "Extinct" , ] %>%
    .[ is.na(.$ISLAND) , ]

  # Then separate each rangemap by species.
  # Convert to simple features object and reproject to myCRS.
  ranges_bySpecies <- lapply(binoms, function(i){
    ranges_mySpecies[ ranges_mySpecies$BINOMIAL == i , ] %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = myCRS) %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 5000) %>%
      st_make_valid() %>%
      #st_crop(., st_bbox(NoAm_boundary_aea)) %>%
      st_intersection(., NoAm_boundary_aea) %>%
      st_combine()
    #st_mask(., st_buffer(NoAm_boundary_aea, dist = 100e3))
    # st_crop(., # Hackey way to remove South American poly's.
    #         raster::extent( -55e5, 26e5, -29e5, 53e5 )
    #)
  })

  # For each species, find distance of all points to its IUCN rangemap polygon.
  alldat <- read.csv( file.path(wd$data, "alldat.csv") )

  getDists <- function(spp, range){
    pts <- alldat %>%
      dplyr::filter( Species == spp ) %>%
      dplyr::select(metersLongitude, metersLatitude) %>%
      na.omit() %>%
      sp::SpatialPoints(., proj4string = crs(myCRS)) %>%
      st_as_sf()
    myDists <- sf::st_distance(range, pts)
    return( as.numeric(unlist(myDists) ) )
  }

  myDists <- mapply(FUN = getDists, spp = SoI, range = ranges_bySpecies )

  # Buffer by 50km until object myDists contains only 0's.
  # Crop to desired extent.
  # Remeasure distances.
  ranges_bySpecies_buffered <- ranges_bySpecies

  for(i in 1:3){
    print(paste("Working on", SoI[i]) )
    while( sum( myDists[[i]] ) > 0 ){
      ranges_bySpecies_buffered[[i]] <- ranges_bySpecies_buffered[[i]] %>%
        st_buffer(dist = 50e3) %>%
        st_crop(., my_extent_aea) %>%
        st_sf()
      newDists <- getDists(SoI[i], ranges_bySpecies_buffered[[i]])
      print(sum(newDists))
      myDists[[i]] <- newDists
    }
  }

  lapply(myDists, function(a) sum(a) == 0)

  # Convert buffered rangemaps to rasters with appropriate
  range_rasters <- lapply(1:3, function(i){
    ex_rast <- my_isoscapes[[1]]$isoscape
    ex_rast[] <- 1
    range_raster <- raster::mask(
      ex_rast, mask = ranges_bySpecies_buffered[[i]],
      updatevalue = NA
    ) %>%
      raster::mask(., NoAm_boundary_aea) %>%
      raster::crop(., my_extent_aea)

    l <- list()
    l$range_raster  <- range_raster
    l$spname        <- SoI[i]
    return(l)

  })

  save( range_rasters, file = file.path(wd$bin, "range_raster.Rdata" ) )
} else message("Not reloading IUCN rangemaps...")
