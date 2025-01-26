# Setup ------
library(tidyverse)
library(brms)
library(cmdstanr)
library(tidybayes)
library(lubridate)
library(splines)
library(patchwork)
load("bin/modelFits_2025-01-14.RData")
theme_set(
  theme_classic() +
    theme(
      strip.background = element_blank()
    ))
mySpecies <- c( "Silver-haired", "Hoary", "Eastern red")
speciesColors <- list(
  scale_color_manual(
    "Species",
    breaks = mySpecies,
    values = c( "#335C67", "#E09F3E", "#A43828")
  ),
  scale_fill_manual(
    "Species",
    breaks = mySpecies,
    values = c( "#335C67", "#E09F3E", "#A43828")
  )
)

firstsOfMonths1 <- ymd(paste( 2024, 1:12, 1, sep = "-")) %>%
  yday()
firstsOfMonths <- c(firstsOfMonths1, firstsOfMonths1 + 365)
monthDeets <- data.frame(yday = firstsOfMonths, month = month.abb[1:12], ydayLab = firstsOfMonths1)

monthBreaks1 <- list(
  scale_x_continuous(
    "Month\n(ordinal day of year, start Jan 1)",
    breaks = monthDeets$yday[1:12],
    labels = paste0(monthDeets$month, "\n",  monthDeets$ydayLab)[1:12],
    expand = c(0,0)
  )
)
monthBreaks2 <- list(
  scale_x_continuous(
    "Month",
    breaks = monthDeets$yday[1:12],
    labels = 1:12,
    expand = c(0,0),
    limits = c(1,365)
  )
)

predColor <- "grey50"
# dircol1 <- "#00C2D1"
# dircol2 <- "#C47AC0"
dircol1 <- "#00CC99"
dircol2 <- "#F374AE"
dirCols <- list(
  scale_color_manual(
    "Direction of movement",
    breaks = c("N", "S"),
    labels = c("To lower\nlatitude", "To higher\nlatitude"),
    values = c(dircol1, dircol2)
  ),
  scale_fill_manual(
    "Direction of movement",
    breaks = c("N", "S"),
    labels = c("To lower\nlatitude", "To higher\nlatitude"),
    values = c(dircol1, dircol2)
  )
)

# Load plots -----
## sampling lat ----
### Model predictions ----
p_dist_lat <- ggplot(pred_sam) +
  aes(x = decimalLatitude, y = estimate__, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = `lower__`, ymax = `upper__`), alpha = 0.1, color = NA) +
  geom_path() +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  scale_x_continuous("Sampling latitude (degrees)", expand = c(0,0)) +
  scale_y_continuous("Minimum distance traveled\nfrom summering grounds(km)", limits = c(0, NA), expand = c(0,0)) +
  speciesColors +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
p_dist_lat
ggsave(p_dist_lat, filename = "out/figs/modelPredictions/p_dist_lat.svg", width = 2, height = 2)

### Hypothesized -----
p1 <- ggplot() +
  geom_segment(aes(x = 1, xend = 6, y = 6, yend = 1), color = predColor) +
  scale_x_continuous(
    "Sampling latitude",
    breaks = c(1,6),
    labels = c("Low", "High"),
  ) +
  scale_y_continuous(
    "Distance traveled\n from summer grounds",
    breaks = c(1,6),
    labels = c("Low", "High")
  ) +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = predColor),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    text = element_text(color = predColor),
    axis.text = element_text(color = predColor)
  )
(p1)

p1_nolabs <- p1 +
  # annotate("text", x = 3.5, y =6, label = "Predicted", color = predColor, hjust = 0.5, vjust = -1) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = predColor),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
(p1_nolabs)
## summer origin ----
### Model prediction -----
p_dist_orig <- ggplot(pred_orig) +
  aes(x = OriginCluster, y = estimate__, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = `lower__`, ymax = `upper__`), alpha = 0.1, color = NA) +
  geom_path() +
  theme_minimal() +
  scale_x_continuous("Summer latitude", breaks = c(1, 4), labels = c("Low", "High"), expand = c(0,0)) +
  scale_y_continuous("Minimum distance traveled\nfrom summer grounds(km)", limits = c(0, 300), expand = c(0,0)) +
  speciesColors +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
p_dist_orig
ggsave(p_dist_orig, filename = "out/figs/modelPredictions/p_dist_orig.svg", width = 2, height = 2)

### Hypothesized ----
p2 <- ggplot() +
  geom_segment(aes(x = 1, xend = 6, y = 1, yend = 6), color = predColor) +
  scale_x_continuous(
    "Summer latitude",
    breaks = c(1,6),
    limits = c(1,6),
    labels = c("Low", "High"),
  ) +
  scale_y_continuous(
    "Distance traveled\n from summer grounds",
    breaks = c(1,6),
    limits = c(1,6),
    labels = c("Low", "High")
  ) +
  # coord_cartesian() +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(vjust = -5),
    axis.title.y.right = element_text(vjust = -5),
    axis.line = element_line(color = "grey60"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    text = element_text(color = predColor),
    axis.text = element_text(color = predColor)
  )
(p2)

p2_nolabs <- p2 +
  # annotate("text", x = 3.5, y =6, label = "Predicted", color = predColor, hjust = 0.5, vjust = -1) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = predColor),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
(p2_nolabs)

# Direction ---------------------------------------------------------------
### Model predictions ----
p_sppDirPlots <- lapply(mySpecies, function(spp) {
  pred_dir %>%
    dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>%
    dplyr::filter(commonName == spp) %>%
    ggplot() +
    aes(x = yDay, y = estimate__, color = dir, fill = dir, group = dir) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(color = dir), linewidth = 0.9) +
    dirCols +
    monthBreaks2 +
    scale_y_continuous(
      "Probability of movement\nin indicated direction",
      expand = c(0,0),
      breaks = seq(0,1,by=0.25),
      limits = c(0,1.1)
    ) +
    scale_linetype_discrete(
      "Direction of movement",
      breaks = c("N", "S", "U"),
      labels = c("To lower\nlatitude", "To higher\nlatitude", "Not\ndetermined")
    ) +
    facet_wrap(~commonName, scales = "free_y")  +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
})

### Hypothesized -----
df_pred_dir <- data.frame(
  x = rep(seq(-1, 1, length.out = 100), 2),
  dir = c(rep("N", 100), rep("S", 100))
) %>%
  dplyr::mutate( y = case_when(dir == "N" ~ (x)^2 + 0.1, TRUE ~ 0) )

p_dir_prediction <- df_pred_dir %>%
  ggplot() +
  geom_path( aes(x=x,y=y, color = dir), linewidth = 0.8 ) +
  dirCols +
  scale_x_continuous(
    "Month",
    breaks = seq(-1,1,length.out = 12),
    labels = 1:12
  ) +
  scale_y_continuous(
    "Probability of movement\n in indicated direction",
    breaks = c(0,1),
    limits = c(0,1),
    labels = c("Low", "High")
  ) +
  # annotate("text", x = 0, y = 1, label = "Predicted", color = predColor, hjust = 0.5) +
  coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    axis.line = element_line(color = "grey60"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    axis.text = element_text(color = predColor)
  ) +
  guides(linetype = "none")
(p_dir_prediction)

# Estimate peaks and other summaries
pred_dir %>%
  group_by(dir, commonName) %>%
  summarise(max = max(estimate__))

laci_thresh_0.9 <- pred_dir %>%
  dplyr::filter(commonName == "Hoary", dir == "S") %>%
  dplyr::summarise(q90 = quantile(estimate__, 0.90)) %>%
  unlist

pred_dir %>%
  dplyr::filter(commonName == "Hoary", dir == "S") %>%
  dplyr::filter(estimate__ >= laci_thresh_0.9) %>%
  dplyr::summarise(min = min(yDay), max = max(yDay))


# Schematic ---------------------------------------------------------------
## Setup prediction for schematic -----
dirCols <- list(
  scale_color_manual(
    "Direction of movement",
    breaks = c("N", "S"),
    labels = c("To lower\nlatitude", "To higher\nlatitude"),
    values = c(dircol1, dircol2)
  ),
  scale_fill_manual(
    "Direction of movement",
    breaks = c("N", "S"),
    labels = c("To lower\nlatitude", "To higher\nlatitude"),
    values = c(dircol1, dircol2)
  )
)

# Specify
n <- 8
yrange_top <- 5
ymed1 <- 17
yrange_bottom <- 5
ymed2 <- 27

xstart1 <- -140
xstart2 <- -90
xSpacing <- 1

set.seed(42)
df1 <- data.frame(
  y1 = runif(n, ymed1-yrange_bottom, ymed1+yrange_bottom),
  y2 = runif(n, ymed2-yrange_top, ymed2+yrange_top),
  direction = "S"
)
df2 <-   data.frame(
  y1 = df1$y2,
  y2 = df1$y1,
  direction = "N"
)

df_pell.mell <- lapply( 1:nrow(df1), function(i) {
  data.frame(
    y1 = df1[i,"y2"],
    y2 = df1[i,"y2"] + runif(1, min = 3, max = max(df1$y2)-df1[i,"y2"]+3),
    direction = "S"
  )
}) %>% bind_rows()

df_pell.mell_return <- data.frame(
  y1 = df_pell.mell$y2,
  y2 = df1$y1,
  direction = "N"
)

df_pell.mell_all <- lapply(1:n, function(i) {
  bind_rows(
    df_pell.mell[i,],
    df_pell.mell_return[i,]
  )
}) %>% bind_rows()

df_schem <- bind_rows(
  data.frame( df1, scenario = "Predicted", season = "Spring") ,
  data.frame( df2, scenario = "Predicted", season = "Fall"),
  data.frame( df1, scenario = "Pell-mell", season = "Spring") ,
  data.frame(df_pell.mell_all, scenario = "Pell-mell", season = "Fall")
)

# Change x:
df_schem <- df_schem %>%
  dplyr::filter(season == "Fall") %>%
  group_by(scenario) %>%
  dplyr::mutate(
    x = row_number(),
    x = case_when(
      scenario == "Predicted" ~ x + x - 0.75 ,
      scenario == "Pell-mell" & direction == "N" ~ x - 0.5,
      TRUE ~ x
    )
  ) %>%
  ungroup %>%
  dplyr::mutate(
    scenario = factor(scenario, levels = c("Predicted", "Pell-mell"))
  )


## Plot prediction for schematic -----
p_schematic3 <- lapply(c("Predicted", "Pell-mell"), function(d) {
  df_schem %>%
    dplyr::filter(scenario == d) %>%
    ggplot() +
    geom_segment(
      mapping = aes(x = x, xend = x, y = y1, yend = y2, color = direction),
      arrow = arrow(angle = 45,
                    ends = "last",
                    type = "open",
                    length = unit(4, "pt")),
      linewidth = 0.8
    ) +
    dirCols +
    scale_x_continuous("", breaks = 100, expand = c(0,0)) +
    scale_y_continuous("Latitude", breaks = seq(-200,200,by=5), expand = c(0,0)) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      #axis.title = element_text(size = 8),
      panel.grid.minor = element_blank(),
      text = element_text(color = predColor),
      axis.text = element_blank(),
      legend.position = "none",
      axis.line.y = element_blank()
    )
})


# Arrange -----------------------------------------------------------------

## Experiment with insets ----

p_sumLat_inset <- p_dist_orig +
  inset_element(
    p2_nolabs,
    left = 0, bottom = 0.72, right =  0.25, top = 0.93
    )


p_samLat_inset <- p_dist_lat +
  inset_element(
    p1_nolabs,
    left = 0, bottom = 0.72, right =  0.25, top = 0.93
  )

p_schematic_together <- {
  {p_schematic3[[1]] + annotate("text", x = 8, y =37, label = "Predicted autumn migration", color = predColor, hjust = 0.5) } /
  {p_schematic3[[2]] + annotate("text", x = 8, y =37, label = "'Pell-mell' autumn migration", color = predColor, hjust = 0.5)}
  }


### Combine ----

design4 <- "
  1#3#4
  1#3#4
  2#3#4
  ####4
"

bigCombo <-
  p_sumLat_inset +
  p_samLat_inset +
  p_dir_together +
  p_schematic_together +
  plot_layout(
    design = design4, guides = "collect",
    widths = c(1,0.1, 1.6, 0.1, 1),
    heights = c(1,0.1,1,0.1)
    ) &
  theme(legend.position='none')

ggsave(bigCombo, filename = "out/figs/modelPredictions/bigCombo.png", width = 12, height = 6)
ggsave(bigCombo, filename = "out/figs/modelPredictions/bigCombo.svg", width = 12, height = 6)

