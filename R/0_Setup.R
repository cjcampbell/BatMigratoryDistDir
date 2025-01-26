
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
wd$litdata <- file.path( my_dir_path, "data", "Lit_data")
wd$iucn    <- file.path(IUCNlocationData) # This  is in a separate directory, referenced it in the .Rprofile file.
wd$figs    <- file.path( my_dir_path, "figs" )

# Check for presence of subdirectories. Create if needed.
invisible({
  lapply(wd, function(i) if( dir.exists(i) != 1 ) dir.create(i) )
})

# Define species codes and binomials.
SoI <- c("LACI", "LABO", "LANO")
binoms <- c("Lasiurus cinereus", "Lasiurus borealis", "Lasionycteris noctivagans")
mySpecies <- c("Hoary", "Eastern red", "Silver-haired")

# Define extent of spatial analysis.
# Unit == meters
my_extent_aea <- raster::extent(
  -55e5, 34e5,
  -30e5, 53e5
)

# CRS for aea projection:
myCRS <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0"
