
# Plotting model outputs from distance and direction of travel analyses.

library(ggplot2)
library(ggpubr)

theme_set(ggpubr::theme_pubclean(base_size = 10))

load(file.path(wd$bin, "distModelPlots.RData"))
load(file.path(wd$bin, "directionModel.RData"))
ps2 <- lapply(ps, function(x) {
  x +
    geom_hline(yintercept = 0, color = "grey60") +
    theme_bw() +
    labs(x = "Day of year") +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(0,1,0,0),
      axis.line = element_line(color = "grey60"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(color = "transparent", fill = "transparent")
    )
})

colParams <- list(
  scale_color_manual(
    "Species",
    breaks = mySpecies[c(3,1,2)],
    values = c("#E09F3E", "#A43828", "#335C67")[c(3,1,2)]
  ),
  scale_fill_manual(
    "Species",
    breaks = mySpecies[c(3,1,2)],
    values = c("#E09F3E", "#A43828", "#335C67")[c(3,1,2)]
  ),
  theme_bw(),
  theme(
    legend.position = "none",
    plot.margin = margin(0,1,0,0),
    axis.line = element_line(color = "grey60"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "transparent", fill = "transparent")
    )
)

# Plot of distance traveled results.
BigPlotBySpecies <-
  ggpubr::ggarrange(
    plotlist = list(
      lat_orig_spp1 + xlab ("Sampling latitude (°N)") + theme(legend.position = "none") + colParams,
      lat_orig_spp2 + xlab ("Sampling latitude (°N)") + theme(legend.position = "none") + colParams,
      clust_1       + theme(legend.position = "none") + colParams + scale_x_continuous("Relative summer latitude", breaks = c(1,4), labels = c("Low", "High")),
      clust_2       + theme(legend.position = "none") + colParams + scale_x_continuous("Relative summer latitude", breaks = c(1,4), labels = c("Low", "High"))
    ),
    #common.legend = F,
    labels = c(LETTERS[1:8]),
    #legend = "top",
    hjust=-0.2, vjust = 0,
    ncol = 2, nrow = 2
  ) +
  theme(plot.margin = margin(0.5,0,0,0, "cm"))


# Plot hypotheses -------------------------------------------------



theme_set(theme_pubr() + theme(text = element_text(size = 8)))
p1 <- ggplot() +
  geom_segment(aes(x = 1, xend = 6, y = 6, yend = 1)) +
  scale_x_continuous(
    "Sampling latitude",
    breaks = c(1,6),
    labels = c("Low", "High"),
  ) +
  scale_y_continuous(
    "Probability of movement\n from summer grounds",
    breaks = c(1,6),
    labels = c("Low", "High"),
    sec.axis = sec_axis(
      trans = ~.,
      name = "Distance traveled\n from summer grounds",
      breaks = c(1,6),
      labels = c("Low", "High"),
    )
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = "grey60"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "transparent", fill = "transparent")
  )
(p1)

p2 <- ggplot() +
  geom_segment(aes(x = 1, xend = 6, y = 1, yend = 6)) +
  scale_x_continuous(
    "Relative summer latitude",
    breaks = c(1,6),
    labels = c("Low", "High"),
  ) +
  scale_y_continuous(
    "Probability of movement\n from summer grounds",
    breaks = c(1,6),
    labels = c("Low", "High"),
    sec.axis = sec_axis(
      trans = ~.,
      name = "Distance traveled\n from summer grounds",
      breaks = c(1,6),
      labels = c("Low", "High"),
    )
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = "grey60"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "transparent", fill = "transparent")
  )
(p2)

p_ToNorth <- seq(-1, 1, length.out = 100) %>%
  data.frame(x = ., z = rep(0,100)) %>%
  dplyr::mutate(y = (x)^2 + 0.1) %>%
  ggplot() +
  geom_path( aes(x=x,y=z) ) +
  #geom_textpath(aes(x=x,y=z,label = "Migration north from summer grounds"), vjust = -0.1, hjust = 0.1, color = col_S, size = 2) +
  scale_x_continuous(
    "Day of year",
    breaks = seq(-1,1,length.out = 6),
    labels = c(seq(0,365,by=75), 365)
  ) +
  scale_y_continuous(
    #"Probability of movement\nto higher latitudes\nfrom summer grounds",
    "Probability of movement\nto higher latitudes",
    breaks = c(0,1),
    limits = c(0,1),
    labels = c("Low", "High")
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = "grey60"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )

p_ToSouth <- seq(-1, 1, length.out = 100) %>%
  data.frame(x = ., z = rep(0,100)) %>%
  dplyr::mutate(y = (x)^2 + 0.1) %>%
  ggplot() +
  geom_path( aes(x=x,y=y) , color = "black") +
 # geom_textpath(aes(x=x,y=y,label = "Migration south from summer grounds"), vjust = -0.1, hjust = 0.1, color = col_N, size = 2) +
  scale_x_continuous(
    "Day of year",
    breaks = seq(-1,1,length.out = 6),
    labels = c(seq(0,365,by=75), 365)
  ) +
  scale_y_continuous(
    #"Probability of movement\nto lower latitudes\n from summer grounds",
    "Probability of movement\nto lower latitudes",
    breaks = c(0,1),
    limits = c(0,1),
    labels = c("Low", "High")
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = "grey60"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )


library(patchwork)

exLegend <- clust_1 +
  scale_color_manual("", breaks = c("Silver-haired", "Hoary", "Eastern red"), values = c("#335C67","#E09F3E","#A43828")) +
  scale_fill_manual( "", breaks = c("Silver-haired", "Hoary", "Eastern red"), values = c("#335C67","#E09F3E","#A43828")) +
  theme(
    legend.background = element_rect(fill = "transparent", color = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
    legend.key = element_rect(fill = "transparent", colour = "transparent")
    )
myLegend <- get_legend(exLegend)


pp <- plot_spacer() +
  plot_spacer() +
  plot_spacer() +
  plot_spacer() +
  p2 +
  {clust_1       + theme(legend.position = "none") + colParams + scale_x_continuous("Relative latitude of summer habitat", breaks = c(1,4), labels = c("Low", "High"))} +
  {clust_2       + theme(legend.position = "none") + colParams + scale_x_continuous("Relative latitude of summer habitat", breaks = c(1,4), labels = c("Low", "High"))} +
  p1 +
  {lat_orig_spp1 + theme(legend.position = "none") + colParams + scale_x_continuous("Sampling latitude (°N)") } +
  {lat_orig_spp2 + theme(legend.position = "none") + colParams + scale_x_continuous("Sampling latitude (°N)") } +

  p_ToSouth +
  modColor(ps2[[6]], mycolor = "#335C67") +
  modColor(ps2[[2]], mycolor = "#E09F3E") +
  modColor(ps2[[4]], mycolor = "#A43828") +

   p_ToNorth +
  modColor(ps2[[5]], mycolor = "#335C67") +
  modColor(ps2[[1]], mycolor = "#E09F3E") +
  modColor(ps2[[3]], mycolor = "#A43828") +

  plot_layout(
    design =
      "ABBCCDD
       EFFFGGG
       HIIIJJJ
       KLLMMNN
       OPPQQRR
      ",
    widths = c(2.1,rep(1,6)),
    heights = c(0.1,rep(1,4))
  )

ggsave(plot = pp, filename = file.path(wd$figs, "BigComboPlot-nolabs.png"), width = 10, height = 10.5)

pp1 <- pp +
  plot_annotation(tag_levels = 'A')  &
  theme(plot.tag = element_text(size = 10, face = "bold", color = "grey20"))
ggsave(plot = pp1, filename = file.path(wd$figs, "BigComboPlot.png"), width = 10, height = 10.5)

