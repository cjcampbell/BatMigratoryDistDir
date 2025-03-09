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

# sex ID -----
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
ggsave(p_sex_originClust, filename = "out/figs/modelPredictions/p_sex_originClust.png", width = 12, height = 4)


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
ggsave(p_age_dataHistogram, filename = file.path("out/figs/p_age_dataHistogram.png"), width = 12, height = 4)

