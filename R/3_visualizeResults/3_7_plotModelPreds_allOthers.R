library(tidyverse)
library(brms)
library(cmdstanr)
library(tidybayes)
library(lubridate)
library(splines)
library(patchwork)
load("tmp/modelFits.RData")
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

# Plots re: sex ID -----
## Raw data ------
df %>% 
  count(commonName, wind_killed, Sex) %>% 
  ggplot() +
  geom_col(aes(x=wind_killed, y = n, fill = Sex), position = "stack") +
  facet_wrap(~commonName)

## Summer latitude results (reliable sex ID only) ----
df_sex_originClust <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_sex_lano, effects = "OriginCluster", cond = data.frame(yday2 = 153, decimalLatitude = 40) )[[1]] ),
  data.frame(commonName = mySpecies[2], conditional_effects(m_sex_laci, effects = "OriginCluster", cond = data.frame(yday2 = 153, decimalLatitude = 40) )[[1]] ),
  data.frame(commonName = mySpecies[3], conditional_effects(m_sex_labo, effects = "OriginCluster", cond = data.frame(yday2 = 153, decimalLatitude = 40) )[[1]] )
) %>% 
  bind_rows() %>% 
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies))

p_sex_originClust <- ggplot(df_sex_originClust) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey80") +
  aes(x = OriginCluster, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__)) +
  facet_wrap(~commonName) +
  scale_x_continuous("Latitude of summer origin", breaks = c(1, 4), labels = c("Low", "High")) +
  scale_y_continuous("Probability of identification as female") +
  speciesColors +
  theme(
    legend.position = "none"
  )
ggsave(p_sex_originClust, filename = "figs/batDistDir/p_sex_originClust.png", width = 12, height = 4)


df_sex_samplingLatitude <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_sex_lano, effects = "decimalLatitude", cond = data.frame(yday2 = c(153)) )[[1]] ),
  data.frame(commonName = mySpecies[2], conditional_effects(m_sex_laci, effects = "decimalLatitude", cond = data.frame(yday2 = c(153)) )[[1]] ),
  data.frame(commonName = mySpecies[3], conditional_effects(m_sex_labo, effects = "decimalLatitude", cond = data.frame(yday2 = c(153)) )[[1]] )
) %>% 
  bind_rows() %>% 
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies))

p_sex_samplingLatitude <- ggplot(df_sex_samplingLatitude) +
  aes(x = decimalLatitude, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__)) +
  facet_wrap(~commonName) +
  scale_x_continuous("Latitude of sampling") +
  scale_y_continuous("Probability of identification as female") +
  speciesColors +
  theme(
    legend.position = "none"
  )


## Sampling latitude results -----
df_decLat <- data.frame(decimalLatitude = c(25, 45))
preds_decLatYday <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_sex_lano, effects = "yday2", conditions = df_decLat)[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_sex_laci, effects = "yday2", conditions = df_decLat)[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_sex_labo, effects = "yday2", conditions = df_decLat)[[1]])
) %>% bind_rows() %>% 
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies))

p_sex_decLatYday <- ggplot(preds_decLatYday) +
  geom_hline(yintercept = 0.5, linetype = 2, color = "grey80") +
  aes(x = yday2, color = cond__, fill = cond__) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__), linewidth = 1) +
  scale_color_manual("Sampling Latitude", values = c("#2a9d8f", "#f4a261"), breaks = 1:nrow(df_decLat), labels = df_decLat$decimalLatitude) +
  scale_fill_manual( "Sampling Latitude", values = c("#2a9d8f", "#f4a261"), breaks = 1:nrow(df_decLat), labels = df_decLat$decimalLatitude) +
  scale_y_continuous("Probability of identification as female") +
  monthBreaks2 +
  facet_wrap(~commonName) 

## Combine origin cluster and sampling latitude data ------
p_sex_combo <- 
  p_sex_originClust / p_sex_decLatYday  +
  theme(legend.position='bottom')


# Age ---------------------------------------------------------------------
p_age_dataHistogram <- df_juvenile %>% 
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>% 
  ggplot() +
  geom_histogram(aes(x = yDay, group = isJ, fill = isJ)) +
  facet_wrap(~commonName) +
  scale_y_continuous("Count of samples with age specified", expand = c(0,0), breaks = seq(0,200,by=25)) +
  monthBreaks2 +
  scale_fill_manual("Age", values = c("grey50", "black"), labels = c("Adult", "Juvenile")) +
  theme(
    panel.grid.major.y = element_line()
  )
ggsave(p_age_dataHistogram, filename = file.path("figs/batDistDir/p_age_dataHistogram.png"), width = 12, height = 4)

df_wind_isJ <- conditional_effects(m_wind_isJ, effects = "isJ", prob = 0.95)[[1]] %>% 
  dplyr::rename(lower_95 = lower__, upper_95 = upper__) %>% 
  full_join(., {
    conditional_effects(m_wind_isJ, effects = "isJ", prob = 0.50)[[1]] %>% 
      dplyr::rename(lower_50 = lower__, upper_50 = upper__) 
  })

p_pred_age_wind <- df_wind_isJ %>% 
  ggplot() +
  aes(x = isJ) +
  geom_segment(aes(y = lower_95, yend = upper_95)) +
  geom_segment(aes(y = lower_50, yend = upper_50), size = 1) +
  geom_point(aes(y = estimate__)) +
  scale_x_discrete("Age", labels = c("Adult", "Juvenile")) +
  scale_y_continuous("Probability of sampling\nat wind energy facility", expand = c(0.1, 0.1))
p_pred_age_wind
ggsave(p_pred_age_wind, filename = file.path("figs/batDistDir/p_pred_age_wind.png"), width = 4, height = 4)



conditional_effects(m_wind_isJ, effects = "yDay", conditions = data.frame(isJ = c(0,1)))[[1]] %>% 
  ggplot() +
  aes(x = yDay, color = isJ, group = isJ) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__), linewidth = 1) +
  monthBreaks2 +
  scale_y_continuous("Probability of sampling\nat wind energy facility", expand = c(0,0), limits = c(0, NA))




# Wind --------------------------------------------------------------------

dircol1 <- "#00CC99"
dircol2 <- "#F374AE"
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

## Wind model 1 (direction only) -----

### Wind direction  ----
pred_wind1 <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_lano_1, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_laci_2, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_labo_2, effect = "dir")[[1]])
) %>% bind_rows()
pred_wind2 <- 
  list(
    data.frame(commonName = mySpecies[1], conditional_effects(m_wind_lano_1, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[2], conditional_effects(m_wind_laci_2, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[3], conditional_effects(m_wind_labo_2, effect = "dir", prob = 0.5)[[1]])
    ) %>% 
  bind_rows() %>% 
  dplyr::rename(lower_50 = lower__, upper_50 = upper__) %>% 
  full_join(.,  dplyr::rename(pred_wind1, lower_95 = lower__, upper_95 = upper__))

( p_wind_direction <-  
    pred_wind2 %>% 
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
ggsave(p_wind_direction, filename = "figs/batDistDir/p_wind_direction.svg")

p_wind_direction_vert <- p_wind_direction +
  facet_wrap(~commonName, scales = "free_y", ncol = 1) +
  scale_y_continuous(
    "Probability of sampling at wind energy facility",
    limits = c(NA, NA),
    expand = expansion(add = c(0.1, 0.1)),
    breaks = seq(0,1,by=0.1)
  )


### Wind lat -----

pred_wind1_OC <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_lano_1, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_laci_2, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_labo_2, effect = "OriginCluster")[[1]])
) %>% bind_rows()


p_wind_lat <- pred_wind1_OC %>% 
  dplyr::mutate(commonName = factor(commonName, levels= mySpecies)) %>% 
  ggplot() +
  aes(x = OriginCluster, color = commonName, fill = commonName) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
  geom_path(aes(y = estimate__)) +
  speciesColors +
  scale_y_continuous(
    "Probability of sampling\nat wind energy facility",
    breaks = seq(0,1,by=0.1)
  )  +
  scale_x_continuous("Summer latitude", breaks = c(1, 4), labels = c("Low", "High"))  +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
p_wind_lat
ggsave(p_wind_lat, filename = "figs/batDistDir/p_wind_lat.svg", width = 6, height = 6)

### Combine -----
#### Horizontal ----
wind_layout <- "
  111#
  1112
"
p_wind_combined <- 
  {p_wind_direction  + {p_wind_lat + theme(plot.margin = margin(10,0,0,0,unit="pt"))}} +
  plot_layout(
    design = wind_layout, heights = c(0.1,1)
  ) & theme(legend.position = "none")

ggsave(p_wind_combined, filename = "figs/batDistDir/p_wind_combined.png", width = 12, height = 4)
ggsave(p_wind_combined, filename = "figs/batDistDir/p_wind_combined.svg", width = 12, height = 4)


#### Vertical ----
wind_layout2 <- "
  1
  #
  2
"
p_wind_combined2 <- p_wind_direction_vert + p_wind_lat +
  plot_layout(
    design = wind_layout2,
    ncol = 1,
    heights = c(4,0.2,1))

ggsave(p_wind_combined2, filename = "figs/batDistDir/p_wind_combined2.png", width = 3, height = 8)
ggsave(p_wind_combined2, filename = "figs/batDistDir/p_wind_combined2.svg", width = 3, height = 8)



## Wind model 2 (direction probs, dist) --------
### Alternate wind model -----
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

altWindPlots <- lapply( list(m_wind_dist_lano, m_wind_dist_laci, m_wind_dist_labo), function(mod) {

  p1 <- conditional_effects(mod, effect = "N")[[1]] %>% 
    ggplot() +
    aes(x = N) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(y = estimate__)) +
    scale_x_continuous(
      "Probability of movement to lower latitude",
      limits = c(0,1),
      expand = c(0,0)
    ) +
    plotTheme
  
  p2 <- conditional_effects(mod, effect = "S")[[1]] %>% 
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
  
  p5 <- conditional_effects(mod, effect = "yDay")[[1]] %>% 
    ggplot() +
    aes(x = yDay) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.1, linewidth = 0.1) +
    geom_path(aes(y = estimate__)) +
    monthBreaks2 +
    plotTheme
  
  return(list(p1, p2, p3, p4, p5))
    
})

altWindPlots_combo <- unlist(altWindPlots, recursive = F) %>% 
  wrap_plots(byrow = F, ncol = 3) +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(altWindPlots_combo, filename = "figs/batDistDir/altWindPlots.svg", width = 12, height = 12)
ggsave(altWindPlots_combo, filename = "figs/batDistDir/altWindPlots.png", width = 12, height = 12)


## Wind model 3 ---------
# Direction + distance 

### Dir -----
pred_wind1_3 <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci, effect = "dir")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo, effect = "dir")[[1]])
) %>% bind_rows()

pred_wind2_3 <- 
  list(
    data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci, effect = "dir", prob = 0.5)[[1]]),
    data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo, effect = "dir", prob = 0.5)[[1]])
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

### Dist -----

pred_wind_3_dist <- list(
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano, effect = "dist_km")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci, effect = "dist_km")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo, effect = "dist_km")[[1]])
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
  data.frame(commonName = mySpecies[1], conditional_effects(m_wind_distDir_lano, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[2], conditional_effects(m_wind_distDir_laci, effect = "OriginCluster")[[1]]),
  data.frame(commonName = mySpecies[3], conditional_effects(m_wind_distDir_labo, effect = "OriginCluster")[[1]])
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
p_wind_distDir_combo <- 
  p_wind_direction / {p_wind_lat3 + p_wind_3_dist} +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")")

ggsave(p_wind_distDir_combo, filename = "figs/batDistDir/p_wind_distDir_combo.png", width = 8, height = 5)
ggsave(p_wind_distDir_combo, filename = "figs/batDistDir/p_wind_distDir_combo.svg", width = 8, height = 5)

