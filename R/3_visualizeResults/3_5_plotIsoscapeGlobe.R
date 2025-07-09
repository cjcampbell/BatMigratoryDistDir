
# Plot isoscape on globe used in conceptual figure (Fig 1).

library(rnaturalearth)
library(sf)
source("0_setup/0_1_setup.R")

world <- rnaturalearth::countries110 %>%
  st_as_sf()

# https://gis.stackexchange.com/questions/97589/creating-custom-projections-with-proj4

mycrs <- "+proj=tpers +h=15000000 +azi=5 +lon_0=-90 +lat_0=40"

world2 <- rnaturalearth::countries110 %>%
  st_as_sf() %>%
  st_transform(crs = mycrs)
world3 <- world2[st_is_valid(world2),]

ggplot() +
  geom_sf(world3, mapping = aes()) +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_line(color = "grey80")
  )

gs <- terra::rast("../BigZaddyData/isoscapes/GlobalPrecipGS/d2H_GS.tif") %>%
  terra::project(mycrs) %>%
  SDMetrics::surface2df()

mymap <- ggplot() +
  geom_sf(
    dplyr::filter(world3, continent == "North America"),
    mapping = aes(), fill = "grey50", color = NA) +
  geom_tile(gs, mapping = aes(x=x,y=y,fill=value,color=value)) +
  scale_fill_viridis_c( option = "plasma", end = 0.9) +
  scale_color_viridis_c(option = "plasma", end = 0.9) +
  scale_y_continuous(
    breaks = c(-23.43629, 0, 23.43629),
    minor_breaks = seq(-180, 180, by = 10)
    ) +
  scale_x_continuous(
    breaks = seq(-150,150, by = 50),
  ) +
  coord_sf() +
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "grey50"),
    panel.grid.minor = element_line(colour = "grey80")
    #panel.grid.minor= element_line(colour = 'transparent')
  )
ggsave(mymap, filename = file.path(wd$figs, "isoscape2.png"), width = 10, height = 10, bg = "transparent")



# Different color palette -------------------------------------------------
gs_NoAm <- terra::rast("../BigZaddyData/isoscapes/GlobalPrecipGS/d2H_GS.tif") %>%
  terra::crop(terra::ext(c(xmin = -180, xmax = 0, ymin = -10, ymax = 90))) %>%
  terra::project(mycrs) %>%
  SDMetrics::surface2df()

mymap2 <- ggplot() +
  geom_sf(
    dplyr::filter(world3, continent == "North America"),
    mapping = aes(), fill = "grey50", color = NA) +
  geom_tile(gs_NoAm, mapping = aes(x=x,y=y,fill=value,color=value)) +
  scale_color_gradientn(expression(paste(delta^2 ~ H[fur], " (", "\u2030", ", VSMOW)") ),  colors = c("#060215", "#150B38", viridis::viridis(10), "#FDE400"), na.value = "grey90" ) +
  scale_fill_gradientn( expression(paste(delta^2 ~ H[fur], " (", "\u2030", ", VSMOW)") ),  colors = c("#060215", "#150B38", viridis::viridis(10), "#FDE400"), na.value = "grey90" ) +
  scale_y_continuous(
    breaks = c(-23.43629, 0, 23.43629),
    minor_breaks = seq(-180, 180, by = 10)
  ) +
  scale_x_continuous(
    breaks = seq(-150,150, by = 50),
  ) +
  coord_sf() +
  theme(
    text = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "grey50"),
    panel.grid.minor = element_line(colour = "grey80")
  )
ggsave(mymap2, filename = file.path(wd$figs, "isoscape2-2.png"), width = 10, height = 10, bg = "transparent")

l <- get_legend(mymap2 + theme(legend.position = "bottom", text = element_text()) + guides(fill = guide_colorbar(title.position = 'top'), color =  guide_colorbar(title.position = 'top') ))
ggsave(l, filename = file.path(wd$figs, "isoscape2-legend.png"), width = 5, height = 5, bg = "transparent")

