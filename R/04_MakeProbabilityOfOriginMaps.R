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

  mySurfaces <- pbmcapply::pbmclapply(1:nrow(df), mc.cores = nclusters, function(i){
    df1 <- df[i,]
    mySurface <- isocat::isotopeAssignmentModel(
      ID               = df1$ID,
      isotopeValue     = df1$dDprecip,
      SD_indv          = df1$sd_resid_bySource,
      precip_raster    = myiso$isoscape,
      precip_SD_raster = myiso$sd,
      additionalModels = my_range$range_raster
    )
    myQuantileSurface <- isocat::makeQuantileSurfaces(mySurface)
    myOddsSurface <- isocat::makeOddsSurfaces(mySurface)

    mystack <- stack( mySurface, myQuantileSurface, myOddsSurface )
    names(mystack) <- paste0(df1[1,"ID"], c("_raw", "_quantile", "_odds") )

    mdf <- mystack %>%
      raster::as.data.frame(xy = TRUE, long = FALSE, na.rm = FALSE) %>%
      # Hackey fixes:
      tidyr::pivot_longer(-c("x", "y"), names_to = "layer", values_to = "value") %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::separate(col = layer, into = c("ID", "method"))
    saveRDS(mdf, file = file.path(wd$tmp, paste0("df_list_", df1[1,"ID"], ".rds")))

    terra::rast(mySurface) %>%
      terra::writeRaster(file.path(mypath, paste0("Combined_PoO_",df1[1,"ID"],".tif")), overwrite = T)
  } )

})




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
