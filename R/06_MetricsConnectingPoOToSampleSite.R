# Setup -------------------------------------------------------------------

mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )
mydata_clustered <- readRDS( file.path(wd$bin, "mydata_clustered.rds") )

# Functions ----------------------------------------------------------------
# Function that extracts distance and bearing relating each potential cell of
# geographic origin with individual's sample site.

getDistanceDirection <- function(
  rowNumber, dataframe, ID, fromLat, toLat, fromLon, toLon, getDistance = TRUE,
  getDirection = TRUE, roundTo = 2
){
  p1 <- c( dataframe[ rowNumber, fromLon ], dataframe[ rowNumber, fromLat ] )
  p2 <- c( dataframe[ rowNumber, toLon ],   dataframe[ rowNumber, toLat ] )
  myResults <- list()
  if( getDistance == TRUE ){
    dist_km   <- round( geosphere::distGeo(p1, p2) / 1000 , roundTo) #Convert to km, round.
    myResults <- cbind(myResults, dist_km)
  }
  if( getDirection == TRUE ){
    theta_from_site   <- round( geosphere::bearing(p2, p1), roundTo)
    theta_from_origin <- round( geosphere::bearing(p1, p2), roundTo)
    myResults         <- cbind(myResults, theta_from_site, theta_from_origin)
  }
  return(myResults)

}

# Apply -------------------------------------------------------------------

# The coordinates of every origin map cell (across all species).
# coords presented in decimal degrees (`x_dd` and `y_dd`) and meters (`x` and `y`).
coords_all <- readRDS( file.path(wd$bin, "coords_all.rds") )

# Set to 'FALSE' is you don't want to run in parallel.
nclusters <- parallel::detectCores()

# New method is less computationally efficient, much more memory efficient:
wd$tmp <- file.path(wd$bin,"tmp")
wd$tmpDistDir <- file.path(wd$bin,"tmpDistDir")
if(!dir.exists(wd$tmpDistDir) ) dir.create(wd$tmpDistDir)

pbmcapply::pbmclapply(1:length(list.files(wd$tmp)), mc.cores = nclusters, function(i){

  originSurface <- readRDS( list.files(wd$tmp, full.names = T)[i] ) %>%
    as.data.frame()

  # Merge with coordinates to bring in converted decimal degree coordinates.
  origin_bothCoords <- data.table::merge.data.table(
    coords_all, originSurface,
    by=c("x", "y"))

  indivDeets1 <- data.table::merge.data.table(
    origin_bothCoords, dplyr::select(mydata_transformed, decimalLongitude, decimalLatitude, ID),
    by = "ID", all.x = TRUE
  )
  indivID <- unique(indivDeets1$ID)
  indivDeets <- indivDeets1 %>%
    dplyr::select(x, y, x_dd, y_dd, decimalLatitude, decimalLongitude) %>%
    distinct

  out <- lapply(
    FUN = getDistanceDirection,
    1:nrow(indivDeets),
    dataframe =  indivDeets,
    fromLat = "decimalLatitude", toLat = "y_dd", fromLon = "decimalLongitude", toLon = "x_dd",
    getDistance = TRUE, getDirection = TRUE
  ) %>%
    lapply(as.data.frame) %>%
    bind_rows() %>%
    bind_cols(indivDeets, . )

  saveRDS(out, file.path(wd$tmpDistDir, paste0("dist_dir_", indivID, ".rds") ) )

})

# Make quantile-simulation surfaces ---------------------------------------

probs_at_site_df <- readRDS( file.path(wd$bin, "probs_at_site_df.rds") )

referenceIndivs <- mydata_transformed %>%
  dplyr::filter(MoltStatus==1 & wind_killed == "no")

mydata_PoOatSampleSite <- probs_at_site_df %>%
  full_join(mydata_transformed, by = "ID") %>%
  tidyr::pivot_longer(cols = starts_with("probVal_"), names_to = "method", values_to = "valAtSampleSite")

knownOrigin_quantVals <- lapply(SoI, function(spp){
  quantvals <- mydata_PoOatSampleSite %>%
    dplyr::filter(method == "probVal_quant", Species == spp, MoltStatus == 1) %>%
    inner_join(referenceIndivs) %>%
    dplyr::select(valAtSampleSite) %>%
    unlist
  # Parametric bootstrap!
  set.seed(42)
  sampledQuantvals <- sample(quantvals, size = 1e5, replace = T)
  list(Species = spp, quantvals = quantvals)
})


# Conduct quantile-simulation to make surfaces using quantile probabilities of
# MoltStatus == 1 individuals.

list.files(wd$bin, pattern = "Combined.*grd", recursive = TRUE) %>%
  lapply(., function(o){
    spp <- dirname(o)
    stackMaps <- list.files(wd$bin, pattern = basename(o), full.names = T, recursive = TRUE) %>%
      grep(pattern = spp, value = T) %>%
      raster::stack()
    quantOrder <- knownOrigin_quantVals %>%
      lapply(function(x) x$Species) %>%
      unlist %>%
      grep(spp, .)
    myQuantVals <- knownOrigin_quantVals[[quantOrder]]$quantvals
    quantileSimulationStack <- pbmcapply::pbmclapply(1:nlayers(stackMaps), mc.cores = detectCores(), function(i){
      isocat::makeQuantileSimulationSurface(
        stackMaps[[i]],
        ValidationQuantiles = myQuantVals,
        rescale = TRUE
      )
    }) %>% raster::stack()
    writeRaster(
      quantileSimulationStack,
      filename = file.path(wd$bin, spp, "quantileSimulationSurfaces.grd"),
      overwrite = TRUE
    )
  })


# Then stack all the QS surfaces:
stackOfQSSurfaces <- list.files(
  path = wd$bin, pattern = "quantileSimulationSurfaces.grd",
  full.names = T, recursive = T
) %>%
  stack()

writeRaster(stackOfQSSurfaces, file = file.path(wd$bin, "stackOfQSSurfaces.grd"), overwrite = T)


# Estimate min distance traveled ------------------------------------------
if(!exists("stackOfQSSurfaces")) { stackOfQSSurfaces <- raster::stack( file.path(wd$bin, "stackOfQSSurfaces.grd")) }

# Location of distdir info:
wd$tmpDistDir <- file.path(wd$bin,"tmpDistDir")
if(!dir.exists(wd$tmpDistDir) ) error("Go back a couple steps and fill this tmp directory.")

whichQuants <- c(0,.01,.05)

minDistEstimates <- pbmcapply::pbmclapply(
  1:nlayers(stackOfQSSurfaces),
  mc.cores = detectCores(), function(i) {

    lapply(seq(0.05,0.95, by = 0.05), function(conf_threshold){

      # Read in dataframe with distance and direction from each cell to ID-linked sampling location
      coordFilename <- list.files(wd$tmpDistDir, full.names = T)[i]
      ID <- gsub("dist_dir_", "", gsub(".rds", "", basename(coordFilename) ) )
      coordFile <- readRDS( coordFilename ) %>%
        mutate_if(is.list, as.numeric)

      myDF <- stackOfQSSurfaces[[ID]] %>%
        raster::as.data.frame(xy = TRUE, long = FALSE, na.rm = FALSE) %>%
        tidyr::pivot_longer(-c("x", "y"), names_to = "layer", values_to = "value") %>%
        dplyr::filter(!is.na(value)) %>%
        as.data.frame()

      myDFwithCoords <- data.table::merge.data.table(myDF, coordFile, by = c("x", "y"))

      overThreshold <- myDFwithCoords[ myDFwithCoords$value >= conf_threshold,  ] %>%
        setorder(., dist_km)
      out <- overThreshold[floor(quantile(1:nrow(overThreshold), probs = whichQuants)),] %>%
        bind_cols(., data.frame(quantOrder = whichQuants,conf_threshold=conf_threshold ))
      return(out)

    }) %>% bind_rows()
  }) %>% bind_rows()

saveRDS(minDistEstimates, file = file.path(wd$bin, "minDistEstimates.rds"))


# Most likely direction estimates -----------------------------------------

if(!exists("stackOfQSSurfaces")) { stackOfQSSurfaces <- raster::stack( file.path(wd$bin, "stackOfQSSurfaces.grd")) }

i <- mydata_transformed$ID[1]
mydf <- stackOfQSSurfaces[[i]] %>%
  SDMetrics::surface2df()

if(!exists("stackOfQSSurfaces")) { stackOfQSSurfaces <- raster::stack( file.path(wd$bin, "stackOfQSSurfaces.grd")) }
# Location of distdir info:
wd$tmpDistDir <- file.path(wd$bin,"tmpDistDir")
if(!dir.exists(wd$tmpDistDir) ) error("Go back a couple steps and fill this tmp directory.")

mostLikelyDirection0 <- pbmcapply::pbmclapply(
  1:nlayers(stackOfQSSurfaces),
  mc.cores = detectCores(), function(i) {

    set.seed(69)

    coordFilename <- list.files(wd$tmpDistDir, full.names = T)[i]
    ID <- gsub("dist_dir_", "", gsub(".rds", "", basename(coordFilename) ) )
    coordFile <- readRDS( coordFilename ) %>%
      mutate_if(is.list, as.numeric)

    myDF <- stackOfQSSurfaces[[ID]] %>%
      raster::as.data.frame(xy = TRUE, long = FALSE, na.rm = FALSE) %>%
      tidyr::pivot_longer(-c("x", "y"), names_to = "layer", values_to = "value") %>%
      dplyr::filter(!is.na(value)) %>%
      as.data.frame()

    myDFwithCoords <- data.table::merge.data.table(myDF, coordFile, by = c("x", "y"))

    if(length(unique(myDFwithCoords$layer))!=1) stop("Something went wrong reading in layer-specific data!")

    # Make relativizing val/distance
    df <- myDFwithCoords %>%
      dplyr::mutate(
        dist_rescaled = 1-scales::rescale(dist_km),
        distVal = dist_rescaled*value )
    n <- 10000
    rownums <- sample(1:nrow(df), size = n, replace = TRUE, prob = unlist(df$distVal))

    # Generate a N and S column with relative probabilities of Northerly and Southerly origins

    df2 <- df[rownums,] %>% mutate(theta_binned_2 = cut(abs(theta_from_origin), breaks = c(-Inf,90,Inf))) %>%
      group_by(theta_binned_2) %>% dplyr::summarise(n = n()) %>% mutate(prop = n / sum(n)) %>%
      dplyr::mutate(NvS = case_when(theta_binned_2 == "(-Inf,90]" ~ "N", TRUE ~ "S")) %>%
      dplyr::select(prop, NvS) %>% pivot_wider(values_from = prop, names_from = NvS)

    return(data.frame(ID = ID, df2))
  })
mostLikelyDirections <- bind_rows(mostLikelyDirection0)

saveRDS(mostLikelyDirections, file = file.path(wd$bin, "mostLikelyDirections.rds"))



# Combine -----------------------------------------------------------------
mydata_clustered <- readRDS(file.path(wd$bin, "mydata_clustered.rds"))
minDistEstimates <- readRDS(file.path(wd$bin, "minDistEstimates.rds"))
mostLikelyDirections <- readRDS(file.path(wd$bin, "mostLikelyDirections.rds"))

mydata_minDistDir <- dplyr::rename(minDistEstimates, ID = layer) %>%
  dplyr::select(ID,value,x,y,ID,dist_km,theta_from_origin,quantOrder,conf_threshold) %>%
  left_join(., mydata_transformed) %>%
  left_join(., mostLikelyDirections) %>%
  dplyr::filter(!is.na(Species)) %>%
  dplyr::mutate(
    reference = if_else(MoltStatus==1&wind_killed=="no", 1, 0),
    dist = case_when(
      dist_km < 100 ~ "short",
      dist_km >= 100 & dist_km < 500 ~ "mid",
      dist_km > 500 ~ "long"
    ),
    dist = factor(dist, levels = c("short", "mid", "long"))
  ) %>%
  left_join(., mydata_clustered) %>%
  # Tidy.
  dplyr::filter(quantOrder == 0.00, conf_threshold == 0.25 ) %>%
  mutate(
    reference = factor(reference),
    dist = factor(dist, levels = rev(c("short", "mid", "long"))),
    Month = factor(Month, levels = paste(1:12)),
    commonName = case_when(
      Species == "LACI" ~ "Hoary",
      Species == "LABO" ~ "Eastern red",
      Species == "LANO" ~ "Silver-haired"
    ),
    commonName = factor(commonName, levels = mySpecies),
    didMove = case_when(
      dist  %in% c("mid", "long") ~ "Y",
      dist  %in% c("short")  ~ "N"
    ),
    dir = case_when(
      N >= 0.7 ~ "N",
      S >= 0.7 ~ "S",
      TRUE ~ "U"
    ),
    yday2 = case_when(
      yDay <= 150 ~ yDay + 365,
      TRUE ~ as.numeric(yDay)
    )
  )

saveRDS(mydata_minDistDir, file = file.path(wd$bin, "mydata_minDistDir.rds"))

