# Setup -------------------------------------------------------------------

mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )
load( file.path(wd$bin, "range_raster.Rdata" ) )
load( file.path( wd$bin, "my_isoscapes.RData") )

# Set to 'FALSE' is you don't want to run in parallel.
nclusters <- parallel::detectCores() - 1


# Create maps. -----------------------------------------------------------

lapply(SoI, function(spp){

  df <- mydata_transformed %>% dplyr::filter(Species == spp)
  myiso <- my_isoscapes[[ grep( paste0( "precip_val_", lapply(my_isoscapes, function(x) x$path_pattern)), pattern = unique(df$isoscape) ) ]]
  my_range <- range_rasters[[ grep( unlist( lapply(range_rasters, function(x) x$spname) ), pattern = unique(df$Species) ) ]]

  mypath <- file.path( wd$bin, spp)
  if(!dir.exists(mypath)) dir.create(mypath)

  isocat::isotopeAssignmentModel(
    ID               = df$ID,
    isotopeValue     = df$dDprecip,
    SD_indv          = df$sd_resid_bySource,
    precip_raster    = myiso$isoscape,
    precip_SD_raster = myiso$sd,
    savePath         = mypath,
    additionalModels = my_range$range_raster,
    nClusters = nclusters
  )

})


# Convert maps to data.frame format for later use -------------------------------

# Load maps.
maps_cropped_list <- lapply(SoI, function(spp){
  mypath <- file.path( wd$bin, spp)
  maps_cropped   <- list.files(
    mypath, pattern = "Combined.*.grd$", full.names = TRUE) %>%
    raster::stack()
  return(maps_cropped)
})

# Convert to a single stack.
# Normalized probs stack.
maps_cropped_stack <- raster::stack(maps_cropped_list)
writeRaster(maps_cropped_stack, filename = file.path(wd$bin, "normalizedProbabilityMaps.grd"), overwrite = TRUE)

# Data frame conversion:
wd$tmp <- file.path(wd$bin,"tmp")
if(!dir.exists(wd$tmp) ) dir.create(wd$tmp)

# Load cropped/normalized surfaces.
if(!exists("maps_cropped_stack")) maps_cropped_stack <- raster::stack(file.path(wd$bin, "normalizedProbabilityMaps.grd"))

maps_cropped_df_list <- pbmcapply::pbmclapply(
  1:nrow(mydata_transformed),
  mc.cores = detectCores(), function(i){
    # Function that iterates through each bat ID, finds the cropped/normalized probability rasterLayer in the rasterStack,
    # and then uses that to convert to quantile and odds surfaces.
    # Each of the three surfaces is fortified into a data.frame and saved in a subdirectory.

    myNormSurface <- maps_cropped_stack[[  mydata_transformed[i,"ID"]  ]]
    myQuantileSurface <- isocat::makeQuantileSurfaces(myNormSurface)
    myOddsSurface <- isocat::makeOddsSurfaces(myNormSurface)

    mystack <- stack( myNormSurface, myQuantileSurface, myOddsSurface )
    names(mystack) <- paste0(mydata_transformed[i,"ID"], c("_raw", "_quantile", "_odds") )

    mdf <- mystack %>%
      # So raster::as.data.frame has given me TWO big troubles here.
      # Some weird bug in raster::data.frame is messing up column names when long = TRUE.
      # AND if na.rm = TRUE, it seems to throw out *any* cell with an NA, not just a particular cell with an NA.
      # Very unhelpful if you have different ranges in your stack!
      raster::as.data.frame(xy = TRUE, long = FALSE, na.rm = FALSE) %>%
      # Hackey fixes:
      tidyr::pivot_longer(-c("x", "y"), names_to = "layer", values_to = "value") %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::separate(col = layer, into = c("ID", "method"))
    saveRDS(mdf, file = file.path(wd$tmp, paste0("df_list_", mydata_transformed[i,"ID"], ".rds")))
  } )


# Find probability of origin at sample site for all individuals. -------

if(!exists("maps_cropped_stack")) {
  assignmentRasts <- raster::stack(file.path(wd$bin, "normalizedProbabilityMaps.grd"))
} else {
  assignmentRasts <- maps_cropped_stack
}

probs_at_site_list <- pbmcapply::pbmclapply(
  1:nrow(mydata_transformed),
  FUN = function(i){
    thisRast <- assignmentRasts[[ mydata_transformed[i,"ID"] ]]
    pt <- SpatialPoints(coords = cbind( mydata_transformed[i,"metersLongitude"], mydata_transformed[i,"metersLatitude"] ) )

    probVal_raw <- raster::extract(thisRast, pt)
    probVal_OR <- isocat::oddsAtSamplingLocation(thisRast, Lat = pt@coords[[2]], Lon = pt@coords[[1]])
    probVal_quant <- isocat::quantileAtSamplingLocation(thisRast, Lat = pt@coords[[2]], Lon = pt@coords[[1]])

    data.frame(ID = mydata_transformed[i,"ID"], probVal_raw, probVal_OR, probVal_quant)
  },
  mc.cores = detectCores()
)

probs_at_site_df <- probs_at_site_list %>% bind_rows()
saveRDS(probs_at_site_df, file = file.path(wd$bin, "probs_at_site_df.rds"))
