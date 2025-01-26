# Setup -------------------------------------------------------------------

# Load required objects.
mydata <- read.csv( file.path(wd$data, "alldat.csv") ) %>%
  dplyr::mutate_if(is.factor, as.character)
load(file.path(wd$bin, "my_isoscapes.RData"))


# Check for individuals that don't fall on the isoscape.------------------------
# First, test extract values from ioscapes.
mydata_isoVals0 <- lapply(my_isoscapes, function(which_isoscape) {

  isoVals <- lapply(1:nrow(mydata), function(n){
    coords = mydata[ n, c("metersLongitude", "metersLatitude") ]
    raster::extract(which_isoscape$isoscape, coords)
  })

  test <- data.frame(mydata, rename_me = unlist(isoVals))
  names(test)[ncol(test)] <- paste( "precip_val", which_isoscape$path_pattern, sep = "_" )
  return(test)

} ) %>%
  purrr::reduce(left_join, by = names(mydata)) %>%
  dplyr::mutate_if(is.factor, as.character)

# Assuming they're close to candidate isoscapes, it's probably a small
# inevitable projection / masking issue.
# Here, as long as sampling locations fell within 50km of a cell centroid,
# I just moved them over to the nearest cell.
outaRange <- mydata_isoVals0 %>%
  filter_at(vars(starts_with("precip_val")), any_vars(is.na(.)))

if(nrow(outaRange) != 0){
  warning( "Some sampling locations fall outside the ranges of
           the candidate isoscapes!" )
}

if(nrow(outaRange) == 0 ) {
  mydata_isoVals <- mydata_isoVals0
} else{
  # Manual inspection shows that there are 6 individuals very close to a coast:

  my_isoscapes[[2]]$isoscape %>%
    raster::as.data.frame(xy = TRUE, na.rm = TRUE) %>%
    ggplot() +
    geom_tile(aes(x = x, y = y), fill = "grey50", na.rm = TRUE ) +
    geom_jitter(
      data = outaRange,
      aes(x = metersLongitude, y = metersLatitude),
      col = "red", width = 5e3, height = 5e3, alpha = 0.5) +
    theme_minimal()
  my_isoscapes[[2]]$isoscape %>%
    raster::as.data.frame(xy = TRUE, na.rm = TRUE) %>%
    ggplot() +
    geom_tile(aes(x = x, y = y), fill = "grey50", na.rm = TRUE ) +
    geom_jitter(
      data = outaRange,
      aes(x = metersLongitude, y = metersLatitude),
      col = "red", width = 5e3, height = 5e3, alpha = 0.5) +
    theme_minimal() +
    coord_sf(xlim = c(-24e5, -20e5), ylim = c(-2e5, 2e5))
  my_isoscapes[[2]]$isoscape %>%
    raster::as.data.frame(xy = TRUE, na.rm = TRUE) %>%
    ggplot() +
    geom_tile(aes(x = x, y = y), fill = "grey50", na.rm = TRUE) +
    geom_jitter(
      data = outaRange,
      aes(x = metersLongitude, y = metersLatitude),
      col = "red", width = 5e3, height = 5e3, alpha = 0.8) +
    theme_minimal() +
    coord_sf(xlim = c(0, 15e5), ylim = c(0, 10e5))

  # Just nudge em in to the nearest cell centroid.
  coordsToChange <- outaRange %>% dplyr::select(metersLongitude, metersLatitude) %>% distinct

  a <- my_isoscapes[[2]]$isoscape %>%
    raster::as.data.frame(xy = TRUE, na.rm = TRUE)
  changeTo <- lapply(1:nrow(coordsToChange), function(i){
    a$dist <- raster::pointDistance(p1 = a[,1:2], p2 = coordsToChange[i,], lonlat = FALSE) / 1000 #in km
    a[which.min(a$dist),] %>%
      dplyr::select(x,y) %>%
      data.frame(., coordsToChange[i, ] )
  }) %>% plyr::ldply()

  redo <- outaRange %>%
    dplyr::select(-starts_with("precip_val")) %>%
    left_join(changeTo, by = c("metersLongitude", "metersLatitude")) %>%
    dplyr::select(-c("metersLongitude", "metersLatitude")) %>%
    dplyr::rename(metersLongitude=x, metersLatitude = y)

  mydata_redo <- lapply(my_isoscapes, function(which_isoscape){

    isoVals <- lapply(1:nrow(redo), function(n){
      coords = redo[ n, c("metersLongitude", "metersLatitude") ]
      raster::extract(which_isoscape$isoscape, coords)
    })

    test <- data.frame(redo, rename_me = unlist(isoVals))
    names(test)[ncol(test)] <- paste( "precip_val", which_isoscape$path_pattern, sep = "_" )
    return(test)

  }) %>%
    purrr::reduce(left_join, by = names(redo)) %>%
    dplyr::mutate_if(is.factor, as.character)

  mydata_isoVals <- mydata_isoVals0 %>%
    dplyr::filter( !ID %in% outaRange$ID) %>%
    rbind( mydata_redo ) %>%
    arrange(X) %>% dplyr::select(-X)

  outaRange2 <- mydata_isoVals %>%
    filter_at(vars(starts_with("precip_val")), is.na)

  if(nrow(outaRange2) != 0){
    warning( "Some sampling locations fall outside the ranges of
           the candidate isoscapes!" )
  }
}

saveRDS(mydata_isoVals, file = file.path(wd$bin, "mydata_isoVals.rds"))
