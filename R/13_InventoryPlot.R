
# Setup -------------------------------------------------------------------

NoAm <- readRDS( file.path("/Users/cjcampbell/BigZaddyData/NoAm_maps/NoAm.rds"))
mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )
if(!exists("sma_results")) sma_results <- readRDS(file = file.path(wd$bin, "sma_results.rds"))
sma_selected <- sma_results %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(duration = to-from) %>%
  dplyr::filter(duration >= 30) %>%
  group_by(Species) %>%
  dplyr::arrange(desc(mean_r2)) %>%
  slice(1)
load(file.path(wd$bin, "range_raster.Rdata" ), verbose = TRUE)
load(file.path(wd$bin, "my_isoscapes.RData"), verbose = TRUE)


# Plot ------------------------------------------------------------------

NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )


if(!exists("ranges_mySpecies")) {
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
    x <- ranges_mySpecies[ ranges_mySpecies$BINOMIAL == i , ]
    x$Species <- if_else(
      i == "Lasiurus cinereus", "LACI",
      if_else(i == "Lasionycteris noctivagans", "LANO",
              "LABO"
      )
    )
    x %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = myCRS) %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 5000) %>%
      st_make_valid() %>%
      st_intersection(., NoAm) %>%
      st_combine()

  })
}


lapply(SoI, function(spp) {

  library(cowplot)
  library(ggpubr)
  library(jpeg)

  tit <- case_when(
    spp == "LACI" ~ "Hoary" ,
    spp == "LABO" ~ "Eastern red" ,
    spp == "LANO" ~ "Silver-haired"
  )

  get(paste0("imgPath_", spp)) %>%
    readJPEG() ->
    mySppImage

  myRange <-
    case_when(
      spp == "LACI" ~ ranges_bySpecies[1] ,
      spp == "LABO" ~ ranges_bySpecies[2] ,
      spp == "LANO" ~ ranges_bySpecies[3]
    )

  mdf <- dplyr::filter(mydata_transformed, Species == spp)

  colorDeets <- list(
    scale_fill_viridis_c(
      "Sampling intensity (count individuals, log10 scale)",
      option = "viridis",
      begin=0.1,
      breaks = seq(0,2,by=1),
      labels = 10^seq(0,2,by=1),
      limits = c(0,2.7)
    )
  )

  samplingByTime <-
    mdf %>%
    dplyr::filter(!is.na(yDay)) %>%
    group_by(Month) %>%
    dplyr::summarise(n_log=log10(n())) %>%
    ggplot() +
    geom_col(
      aes(x=Month, y = n_log, fill = n_log)
    ) +
    scale_x_continuous(
      breaks = seq(1,12,1),
      labels = seq(1,12,1),
      limits = c(1,12)
    ) +
    scale_y_continuous(
      "n samples",
      expand = c(0,0),
      breaks = seq(0,6,1),
      labels = 10^seq(0,6,1),
      limits = c(0,3)
    ) +
    colorDeets +
    theme_bw() +
    theme(
      axis.line = element_line(color = "grey50"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = 3,color="grey50"),
      legend.position = "none"
    )

  myMap0 <- ggplot() +
    geom_sf(
      data = myRange[[1]], mapping = aes()
    ) +
    geom_hex(
      data = mdf,
      aes(
        x=metersLongitude, y = metersLatitude,
        fill=log10(..count..)
      ),
      binwidth = c(500e3,500e3)
    ) +
    geom_sf(
      data = NoAm,
      mapping = aes(),
      fill = NA
    ) +
    colorDeets +
    theme_bw() +
    guides(fill = guide_colorbar(title.position="top", title.hjust = 0.5)) +
    theme(
      legend.position = "bottom",
      legend.title.align=0.5,
      axis.title = element_blank(),
      panel.border = element_blank(),
      legend.key.width = unit(2,"cm")
    ) +
    coord_sf(
      ylim = c(-29.6e5, 30e5),
      xlim = c(-50e5, 26e5)
    )

  myLegend <- ggpubr::get_legend(myMap0)

  myMap <- myMap0 + theme(
    legend.position = "none"
  )

  big <- myMap +
    annotation_custom(
      grob = as_grob(samplingByTime),
      xmin = -Inf,
      xmax = -50e5 + (45e5+50e5)/3,
      ymin = -Inf,
      ymax = -29.6e5 + (30e5+29.6e5)/3
    )

  # Fix the width of the annotation raster.
    img_width <- (26e5 + 50e5)/3
    img_height <- img_width*dim(mySppImage)[1]/dim(mySppImage)[2]

  big2 <- big +
    annotation_raster(
      mySppImage,
      ymin = 33e5 - img_height,
      ymax = 33e5,
      xmin = -52e5,
      xmax = -52e5 + img_width
    ) +
    ggtitle(tit)

  return(list(big2, myLegend))

}) -> myPlots

p <- ggarrange(
  ncol = 1,
  # Plots
  myPlots[[1]][[1]],
  myPlots[[2]][[1]],
  myPlots[[3]][[1]],
  # Legend
  myPlots[[1]][[2]],
  heights = c(1,1,1, 0.2)
)

ggsave(p, filename = file.path(wd$figs, "inventoryFigs_new.png"),
       width = 6, height = 12)


