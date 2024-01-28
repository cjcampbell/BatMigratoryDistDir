
# Make Figure 2 - plots of sample acquisitions.

# Setup -------------------------------------------------------------------

NoAm_sf <- readRDS( file.path(wd$bin, "NoAm.rds")) %>%
  st_as_sf() %>%
  st_transform(myCRS) %>%
  st_simplify(dTolerance = 1e3)


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
NoAm_boundary_aea <- readRDS( file.path(wd$bin, "NoAm_boundary_aea.rds") )


# Plot ------------------------------------------------------------------

if(!exists("ranges_mySpecies")) {

  # Load candidate rangemaps.
  ranges_mySpecies <- file.path(wd$data, "iucn") %>%
    list.files(pattern = "data_0.shp", recursive = T, full.names = T) %>%
    lapply(function(x) {

      # This analysis is focused on extant species in Continental North America,
      # so filter out extinct and island populations.
      out <-
        st_read(x) %>%
        dplyr::mutate(Species = case_when(
          BINOMIAL == "Lasiurus cinereus" ~"LACI",
          BINOMIAL == "Lasionycteris noctivagans" ~ "LANO",
          BINOMIAL == "Lasiurus borealis" ~"LABO",
        )) %>%
        dplyr::filter(LEGEND != "Extinct") %>% # Keep only extant ranges
        st_transform(crs = myCRS) %>%
        st_simplify(preserveTopology = TRUE, dTolerance = 5000) %>%
        st_make_valid() %>%
        #st_union() %>%
        st_intersection(NoAm_boundary_aea)
      return(out)

    }) %>%
    bind_rows()

}


myPlots <- lapply(SoI[c(3,1,2)], function(spp) {

  library(cowplot)
  library(ggpubr)
  library(jpeg)

  tit <- case_when(
    spp == "LACI" ~ "Hoary" ,
    spp == "LABO" ~ "Eastern red" ,
    spp == "LANO" ~ "Silver-haired"
  )

  myRange <- dplyr::filter(ranges_mySpecies, Species == spp) %>%
    st_union()

  mdf <- dplyr::filter(mydata_transformed, Species == spp)
  mdf_sf <- st_as_sf(mdf, crs = myCRS, coords = c("metersLongitude", "metersLatitude"))

  colorDeets <- list(
    scale_fill_viridis_c(
      "Sampling intensity (count individuals, log10 scale)",
      option = "mako",
      begin=0,end = 0.8,
      breaks = seq(0,2,by=1),
      labels = 10^seq(0,2,by=1),
      limits = c(0,2.7),
      direction = -1
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

  # Assign points to grid, calculate counts.
  mygrid <- st_make_grid(NoAm_sf, cellsize =  c(500e3,500e3), square = F) %>%
    st_sf()
  mygrid$counts <- lengths(st_intersects(mygrid, mdf_sf))
  mygrid <- dplyr::filter(mygrid, counts != 0)

  myMap0 <- ggplot() +
    geom_sf( data = NoAm_sf, mapping = aes(),fill = "white") +
    geom_sf( data = myRange, mapping = aes() ) +
    geom_sf(data = mygrid, mapping = aes(fill = log10(counts)), alpha = 0.9) +
    colorDeets +
    geom_sf(data = NoAm_sf, mapping = aes(), fill = NA ) +
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
      xlim = c(-50e5, 26e5),
      label_graticule = "NE"
    ) +
    scale_x_continuous(breaks = seq(-140,-60,by=20))+
    scale_y_continuous(breaks = seq(20,60,by=10))

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

  # # Fix the width of the annotation raster.
  #   img_width <- (26e5 + 50e5)/3
  #   img_height <- img_width*dim(mySppImage)[1]/dim(mySppImage)[2]
  #
  #   get(paste0("imgPath_", spp)) %>%
  #     readJPEG() ->
  #     mySppImage
  # big2 <- big +
  #   annotation_raster(
  #     mySppImage,
  #     ymin = 33e5 - img_height,
  #     ymax = 33e5,
  #     xmin = -52e5,
  #     xmax = -52e5 + img_width
  #   ) +
  #   ggtitle(tit)
  # return(list(big2, myLegend))

  return(list(big, myLegend))

})

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

ggsave(p, filename = file.path(wd$figs, "inventoryFig_updated_noimg.png"),
       width = 6, height = 12)


