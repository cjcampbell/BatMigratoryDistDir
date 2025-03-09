
# Setup -------------------------------------------------------------------
library(tidyverse)
library(brms)
library(cmdstanr)
library(tidybayes)
library(lubridate)
library(splines)
library(patchwork)
load("bin/modelFits.RData")
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

dircol1 <- "#0FB8C4"
dircol2 <- "#7FB069"
dircolU <- "grey50"
dirCols2 <- list(
  scale_color_manual(
    "Direction of movement",
    breaks = c("N", "S", "U"),
    labels = c("To lower\nlatitude", "To higher\nlatitude", "Not determined"),
    values = c(dircol1, dircol2, dircolU)
  ),
  scale_fill_manual(
    "Direction of movement",
    breaks = c("N", "S", "U"),
    labels = c("To lower\nlatitude", "To higher\nlatitude", "Not determined"),
    values = c(dircol1, dircol2, dircolU)
  )
)

filePath <- "out/figs/modelPredictions"


## Primary Wind model  ---------

### Dir -----
pred_wind1_3 <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano_cat_dir, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci_cat_dir, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo_cat_dir, effect = "dir")[[1]])
) %>% bind_rows()

pred_wind2_3 <-
  list(
    data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano_cat_dir, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci_cat_dir, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo_cat_dir, effect = "dir", prob = 0.5)[[1]])
  ) %>%
  bind_rows() %>%
  dplyr::rename(lower_50 = lower__, upper_50 = upper__) %>%
  full_join(.,  dplyr::rename(pred_wind1_3, lower_95 = lower__, upper_95 = upper__))

( p_wind_direction <-
    pred_wind2_3 %>%
    dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>%
    ggplot() +
    aes(x = dir, color = dir) +
    geom_segment(aes(y = lower_95, yend = upper_95)) +
    geom_segment(aes(y = lower_50, yend = upper_50), linewidth = 1) +
    geom_point(aes(y = estimate__)) +
    facet_wrap(~commonName, scales = "free_y") +
    dirCols2 +
    scale_y_continuous(
      "Probability of sampling\nat wind energy facility",
      limits = c(NA, NA),
      expand = expansion(add = c(0.1, 0.1)),
      breaks = seq(0,1,by=0.1)
    ) +
    scale_x_discrete(
      "Direction of movement",
      breaks = c("N", "S", "U"),
      labels = c("To lower\nlatitude", "To higher\nlatitude", "Not\ndetermined")
    ) +
    scale_linetype_discrete(
      "Direction of movement",
      breaks = c("N", "S", "U"),
      labels = c("To lower\nlatitude", "To higher\nlatitude", "Not\ndetermined")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
)


( p_wind_direction_noUnknowns <-
    pred_wind2_3 %>%
    dplyr::filter(effect1__ != "U") %>%
    dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>%
    ggplot() +
    aes(x = dir, color = dir) +
    geom_segment(aes(y = lower_95, yend = upper_95)) +
    geom_segment(aes(y = lower_50, yend = upper_50), linewidth = 1) +
    geom_point(aes(y = estimate__)) +
    facet_wrap(~commonName, scales = "free_y") +
    dirCols2 +
    scale_y_continuous(
      "Probability of sampling\nat wind energy facility",
      limits = c(NA, NA),
      expand = expansion(add = c(0.1, 0.1)),
      breaks = seq(0,1,by=0.1)
    ) +
    scale_x_discrete(
      "Direction of movement",
      breaks = c("N", "S", "U"),
      labels = c("To lower\nlatitude", "To higher\nlatitude", "Not\ndetermined")
    ) +
    scale_linetype_discrete(
      "Direction of movement",
      breaks = c("N", "S", "U"),
      labels = c("To lower\nlatitude", "To higher\nlatitude", "Not\ndetermined")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
)

### Dist -----

pred_wind_3_dist <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano_cat_dir, effect = "dist_km")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci_cat_dir, effect = "dist_km")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo_cat_dir, effect = "dist_km")[[1]])
) %>% bind_rows()

(p_wind_3_dist <- pred_wind_3_dist %>%
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>%
  ggplot() +
  aes(x = dist_km, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__)) +
  speciesColors +
  scale_y_continuous(
    "Probability of sampling\nat wind energy facility",
    breaks = seq(0,1,by=0.1),
    limits = c(NA,1), expand = expansion(add = c(0.1, 0))
  )  +
  scale_x_continuous(
    "Minimum distance traveled (km)",
    expand = c(0,0),
    limits = c(0, NA)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ))


### Summer Lat -----
pred_wind_3_lat <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano_cat_dir, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci_cat_dir, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo_cat_dir, effect = "OriginCluster")[[1]])
) %>% bind_rows()


p_wind_lat3 <- pred_wind_3_lat %>%
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>%
  ggplot() +
  aes(x = OriginCluster, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__)) +
  speciesColors +
  scale_y_continuous(
    "Probability of sampling\nat wind energy facility",
    breaks = seq(0,1,by=0.1),
    limits = c(NA,1), expand = expansion(add = c(0.1, 0))
  )  +
  scale_x_continuous("Summer latitude", breaks = c(1, 4), labels = c("Low", "High"))  +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p_wind_lat3


### Combine ----
(p_wind_distDir_combo <-
  p_wind_direction_noUnknowns +
   p_wind_lat3 +
   {p_wind_3_dist + theme(axis.title.y = element_blank())} +
  plot_layout(design =
  "
  11
  23
  ##",
  heights = c(1,1,0.1)
) # +
# + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")
)
ggsave(p_wind_distDir_combo, filename = file.path(filePath, "p_wind_distDir_combo.png"), width = 8, height = 5)
ggsave(p_wind_distDir_combo, filename = file.path(filePath, "p_wind_distDir_combo.svg"), width = 8, height = 5)



## Alternate wind model -----
# This propagates uncertainty in direction assignment (but loses some key relationships between direction and minimum distance traveled).
plotTheme <- list(
  theme_minimal(),
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ),
  scale_y_continuous(
    "Probability of sampling\nat wind energy facility",
    breaks = seq(0,1,by=0.1),
    expand = expansion(add = c(0.1, 0.1))
  )
)

altWindPlots <- lapply( list(m_wind_distDir_lano_cat_dir_cont,
                             m_wind_distDir_laci_cat_dir_cont,
                             m_wind_distDir_labo_cat_dir_cont), function(mod) {

  p1 <- conditional_effects(mod, effect = "S")[[1]] %>%
    ggplot() +
    aes(x = S) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(y = estimate__)) +
    scale_x_continuous(
      "Probability of movement to higher latitude",
      limits = c(0,1),
      expand = c(0,0)
    ) +
    plotTheme

  p3 <- conditional_effects(mod, effect = "dist_km")[[1]] %>%
    ggplot() +
    aes(x = dist_km) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(y = estimate__)) +
    scale_x_continuous(
      "Minimum distance traveled (km)",
      expand = c(0,0)
    ) +
    plotTheme

  p4 <- conditional_effects(mod, effect = "OriginCluster")[[1]] %>%
    ggplot() +
    aes(x = OriginCluster) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(y = estimate__)) +
    scale_x_continuous("Summer latitude", breaks = c(1, 4), labels = c("Low", "High"))  +
    plotTheme

  return(list(p1, p3, p4))

})

altWindPlots_combo <- unlist(altWindPlots, recursive = F) %>%
  wrap_plots(byrow = F, ncol = 3) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(altWindPlots_combo, filename = file.path(filePath, "altWindPlots.svg"), width = 12, height = 12)
ggsave(altWindPlots_combo, filename = file.path(filePath, "altWindPlots.png"), width = 12, height = 12)


# Demographic parameter data vis ------------------------------------------


### Age exploration ----
# Are juveniles more likely to be killed @ turbines, accounting for doy effects?
# Doy and turbine date are so correlated, hard to disentangle.

(p_wind_age_count <-
   df %>%
    count(commonName, wind_killed, Age) %>%
    mutate(Age = case_when(is.na(Age) ~ "U", TRUE ~ Age)) %>%
    ggplot() +
    geom_col(aes(x=wind_killed, y = n, fill = Age), position = "stack") +
    facet_wrap(~commonName) +
    scale_y_continuous(
      "Count",
      expand = c(0,0)
    ) +
   scale_x_discrete("Sampled at wind energy facility") +
   scale_fill_manual("Age",values = c("grey50", "black", "#BDD2BC"), labels = c("Adult", "Juvenile", "Unknown"))
 )

ggsave(p_wind_age_count, filename = file.path("out", "figs", "p_wind_age_count.png"), width = 12, height = 5, dpi = 600)

### Sex exploration -----

df %>%
  dplyr::filter(wind_killed == "no", !is.na(Sex)) %>%
  count(commonName, dir, Sex)

