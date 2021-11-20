
# Load libraries required for analyses.
library(tidyverse)
library(isocat)
library(sf)

# Load libraries relied on for a few analyses.
library(chron)
library(stringr)
library(lubridate)
library(ggmap)
library(rgbif)
library(smatr)
library(versions)
#library(devtools)
library(geosphere)
library(lwgeom)
library(tidyr)
library(rgdal)
library(boot)
library(parallel)
library(data.table)
library(assignR)

# Make an object to help navigate the subdirectories.
my_dir_path <- getwd()
print(paste("Working directory is", my_dir_path, ". Sounds correct?"))
wd <- list()
wd$R       <- file.path( my_dir_path, "R" )
wd$bin     <- file.path( my_dir_path, "bin" )
wd$data    <- file.path( my_dir_path, "data" )
wd$litdata <- file.path( my_dir_path, "data", "lit_data")
wd$iucn    <- file.path( my_dir_path, "data", "Chiroptera_NoAm_MesoAm_redlist_species_data_6437449e-d3a3-479e-8c18-169e1749e583")
wd$figs    <- file.path( my_dir_path, "figs" )
wd$results <- file.path( my_dir_path, "results" )

# Check for presence of subdirectories. Create if needed.
invisible({
  lapply(wd, function(i) if( dir.exists(i) != 1 ) dir.create(i) )
})

# Define species codes and binomials.
SoI <- c("LACI", "LABO", "LANO")
binoms <- c("Lasiurus cinereus", "Lasiurus borealis", "Lasionycteris noctivagans")

# Define extent of spatial analysis.
# Unit == meters
my_extent_aea <- raster::extent(
  -55e5, 34e5,
  -30e5, 53e5
)

# CRS for aea projection:
myCRS <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"

# Functions used in some analyses ---------

# Quick fun to return params to include in the next model.
topDredgeModelPredictors <- function(dredgeModelOutput) {
  dredgeModelOutput %>%
    # Find top-performing model
    dplyr::filter(delta < 2) %>% arrange(df) %>% slice(1) %>% as.data.frame %>%
    # Pull out predictors.
    dplyr::select(-c("(Intercept)", "df", "AICc", "delta", "weight", "logLik")) %>%
    # Make a df.
    t %>% as.data.frame() ->
    step1
  print(paste0("Drop: ", row.names(dplyr::filter(step1, is.na(V1))) ) )
  print(paste0("Keep: ", row.names(dplyr::filter(step1, !is.na(V1))) ) )
}

# Quick fun to return params with too-high VIF.
dropVIF <- function(vifOUT) {
  vifOUT %>%
    as.data.frame %>%
    dplyr::filter(`GVIF^(1/(2*Df))` >= 5) %>%
    row.names() -> p
  print(paste0("Consider dropping: ", p))
