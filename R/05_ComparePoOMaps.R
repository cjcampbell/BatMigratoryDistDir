
# Setup --------------------------------------------------------------------

mydata_transformed <- readRDS( file = file.path(wd$bin, "mydata_transformed.rds") )
load( file.path(wd$bin, "my_isoscapes.RData") )
load( file.path(wd$bin, "range_raster.Rdata" ) )

# Make "grouped" simmatrix ------------------------------------------------
# This approach find within-species similarity values for groups of individuals
# where each group representes a distinct combination of model parameter inputs,
# i.e., individuals with identical isotope values will be considered as one
# group for the model run then the simmatrix expanded to represent them
# individually in the next step.

do_par <- TRUE
how_many_cores <- detectCores() - 1        # Even with this optimization,
# it's a very resource-intensive process.

# Run.
for(spp in SoI){

  df <- mydata_transformed %>%
    dplyr::filter(Species == spp)

  message("Creating groups for species ", spp, ". Individuals: ", nrow(df))

  df$group <- paste0( "G", group_indices(df, dDprecip, sd_resid_bySource) )

  myiso <- my_isoscapes[[ grep( paste0( "precip_val_", lapply(my_isoscapes, function(x) x$path_pattern)), pattern = unique(df$isoscape) ) ]]
  my_range <- range_rasters[[ grep( unlist( lapply(range_rasters, function(x) x$spname) ), pattern = unique(df$Species) ) ]]

  bygroup <- df %>%
    dplyr::group_by(group) %>%
    dplyr::sample_n(1) %>%
    dplyr::ungroup()

  groupModels <- isocat::isotopeAssignmentModel(
    ID               = bygroup$group,
    isotopeValue     = bygroup$dDprecip,
    SD_indv          = bygroup$sd_resid_bySource,
    precip_raster    = myiso$isoscape,
    precip_SD_raster = myiso$sd,
    savePath         = FALSE,
    additionalModels  = my_range$range_raster,
    nClusters = how_many_cores
  )

  groupModels_quant <- lapply(1:nlayers(groupModels), function(x){
    .findProductThenNormalize(
      makeQuantileSurfaces(
        groupModels[[x]]
      )
    )
  }) %>%
    stack()

  y <- system.time(
    b <- isocat::simmatrixMaker(
      groupModels_quant,
      nClusters = how_many_cores,
      csvSavePath = FALSE
    )
  )
  save(b, file = file.path( wd$bin, spp, "quant_group_simmatrix.Rdata"))
  write.csv(
    cbind(spp = spp, individuals = nrow(df), y),
    file = file.path( wd$bin, spp, "log.csv")
  )
  message("Simmatrix completed for groups of ", spp)

}


# Populate simmatrix ------------------------------------------------------

for(spp in SoI){

  message("Populating simmatrix for species: ", spp)

  load( file.path( wd$bin, spp, "quant_group_simmatrix.Rdata") )
  df <- mydata_transformed %>%
    dplyr::filter(Species == spp)
  df$group <- paste0( "G", group_indices(df, d2H, sd_resid_bySource) )

  # Duplicate rows when converting from groups back to individuals.
  system.time({
    ids <- as.vector(df$ID)
    x <- matrix( as.numeric(NA), length(ids), length(ids), dimnames=list(ids,ids) )
    # x <- backup
    ut <- upper.tri(x, diag = TRUE)

    out <- parallel::mclapply(1:length(x),  x = x, ut = ut, mc.cores = how_many_cores, function(i, x, ut){
      if( ut[i] ){
        if( is.na( x[i]) ){
          rowgroup <- df[ which(df$ID ==  rownames(x)[ row(x)[i] ]), "group" ]
          colgroup <- df[ which(df$ID ==  colnames(x)[ col(x)[i] ]), "group" ]
          z <- b[rowgroup, colgroup]
        } else z <- x[i]
      } else z <- NA
      return( z )
    } )

    vals <- as.numeric(unlist(out))

    y <- matrix( vals, length(ids), length(ids), dimnames=list(ids,ids) )
  })
  simmatrix <- y

  mypath <- file.path( wd$bin, spp)
  if(!dir.exists(mypath)) dir.create(mypath)
  save(simmatrix, file = file.path(mypath, "quant_simmatrix.Rdata") )
  write.csv(simmatrix, file = file.path( mypath, "quant_simmatrix.csv"))

  message("Simmatrix completed for ", spp)
}


# Perform hierarchical clustering -----------------------------------------

# For each species, perform normal clustering based on similarity matrices. --
clustered_simmatrices <- lapply(SoI, function(spp){
  raw_simmatrix <- read.csv( file.path( wd$bin, spp, "quant_simmatrix.csv") )[, -1]
  rownames(raw_simmatrix) <- colnames(raw_simmatrix)
  simmatrix <- as.matrix( raw_simmatrix)
  simmatrix_dist <- dist( 1 - simmatrix, diag = TRUE, upper = TRUE )
  clustered_simmatrix <- hclust( simmatrix_dist, method = "ward.D2")
  return( list(spp = spp, tree = clustered_simmatrix) )
})
save(clustered_simmatrices, file = file.path(wd$bin, "quant_clustered_simmatrices.Rdata") )

#  Prepare to cut trees into groups of individuals with common origins ----------------

# This function will plot k-means.
wss <- lapply(SoI, function(spp){
  raw_simmatrix <- read.csv( file.path( wd$bin, spp, "quant_simmatrix.csv") )[, -1]
  rownames(raw_simmatrix) <- colnames(raw_simmatrix)
  simmatrix <- as.matrix( raw_simmatrix)
  simmatrix_dist <- dist( 1 - simmatrix, diag = TRUE, upper = TRUE )
  # k-means cluster number
  wss <- rep(NA, 15)
  for (i in 1:15) wss[i] <- sum(kmeans(simmatrix_dist, i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main = spp)
  return(wss)
})


# Plot trees.
load(file.path(wd$bin, "quant_clustered_simmatrices.Rdata"))
lapply(1:3, function(i){
  plot(clustered_simmatrices[[i]]$tree, cex = 0.6, main = SoI[i])
  rect.hclust(clustered_simmatrices[[i]]$tree, k = 5, border = 2:5)
})


# Estimate the latitude of highest probability of origin for each surface -----
mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )

ave_lat <- lapply(1:nrow(mydata_transformed), function(i){
  ex <- mydata_transformed[i, ]
  u1 <- list.files(
    file.path(wd$bin, "tmp"),
    pattern = paste0( "df_list_",  mydata_transformed[i, "ID"], ".rds" ) ,
    full.names = T
  ) %>%
    readRDS() %>%
    filter(method == "raw")
  lats <- base::sample(size = 1000, x = u1$y, replace = TRUE, prob = u1$value)
  mean(lats)
} ) %>%
  unlist

mydata_aveLat <- mydata_transformed %>%
  dplyr::mutate(ave_lat = ave_lat)

saveRDS(mydata_aveLat, file = file.path(wd$bin, "mydata_aveLat.rds"))

# Manually decide how many clusters to cut into. -----------------------
# This part requires manual input!
k_by_spp <- data.frame(species = SoI, k = c(5,5,5))
saveRDS(k_by_spp, file = file.path(wd$bin, "k_by_spp.rds"))

# Cut trees into groups ---------------------------------------------------
# Load data.
mydata_aveLat <- readRDS( file.path(wd$bin, "mydata_aveLat.rds") )
load(file.path(wd$bin, "quant_clustered_simmatrices.Rdata"))

# Apply.
mydata_clustered <- lapply(SoI, function(spp){

  mytree <- clustered_simmatrices %>%
    lapply(function(i) i$spp) %>%
    unlist %>%
    grep(., pattern = spp) %>%
    clustered_simmatrices[[.]] %>%
    .$tree

  rawCluster <- stats::cutree( mytree, k = k_by_spp[ k_by_spp$species == spp , "k"] )

  myClustDeets <- as.data.frame(rawCluster)
  myClustDeets$ID <- mytree$labels

  # Reorder cluster numbers with respect to descending latitude.
  myClustDeets0 <- mydata_aveLat %>%
    right_join(myClustDeets, by = "ID")
  lat_key <- myClustDeets0 %>%
    group_by(rawCluster) %>%
    dplyr::summarise(meanLat = mean(ave_lat, na.rm = TRUE)) %>%
    arrange(meanLat)
  lat_key$order <- 1:nrow(lat_key)

  save(lat_key, file = file.path( wd$bin, spp, "_recodedClusters.Rdata") )

  myClustDeets <- myClustDeets %>%
    dplyr::mutate(OriginCluster = plyr::mapvalues(
      rawCluster, lat_key$rawCluster, lat_key$order)
    ) %>%
    dplyr::select(ID, OriginCluster)

  myClustDeets$originColor <- viridis::viridis(
    length(unique(myClustDeets$OriginCluster)))[myClustDeets$OriginCluster]
  return(myClustDeets)

}) %>%
  plyr::ldply() %>%
  arrange(ID)

saveRDS(mydata_clustered, file = file.path(wd$bin, "mydata_clustered.rds"))
