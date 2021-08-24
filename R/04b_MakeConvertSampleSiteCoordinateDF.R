# Convert coordinate key --------------------------------------------

mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )

# Make a coonverted coordinate key for all points that have a value
# for any focal species:
coords_aea0 <- mydata_transformed %>%
  group_by(Species) %>%
  slice(1) %>%
  ungroup %>%
  dplyr::select(ID) %>%
  unlist %>%
  lapply(function(x) {
    file.path(wd$bin, "tmp", paste0("df_list_", x, ".rds") )
  } ) %>%
  purrr::map_df(., readRDS) %>%
  dplyr::select(x,y) %>%
  distinct

saveRDS(coords_aea0, file = file.path(wd$bin, "coords_aea0.rds"))

coords_all <- coords_aea %>%
  SpatialPointsDataFrame(coords = .[, 1:2], proj4string = CRS(myCRS)) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  st_coordinates() %>%
  as.data.frame %>%
  dplyr::rename(x_dd = X, y_dd = Y) %>%
  cbind(., coords_aea)

saveRDS(coords_all, file = file.path(wd$bin, "coords_all.rds"))

