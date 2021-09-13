
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggpubr)
library(gridExtra)
library(sjPlot)
library(MuMIn)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )
theme_set(ggpubr::theme_pubclean())
options(na.action = "na.fail")



# Exploratory plots -------------------------------------------------------

mydata_minDistDir %>%
  dplyr::filter(!is.na(Month)) %>%
  group_by(Species, Month, dir) %>%
  dplyr::summarise(n=n()) %>%
  mutate(p = n/sum(n)) %>%
  ggplot() +
  aes(y=n, x=Month, fill=dir) +
  geom_col(position = "stack") +
  facet_grid(~Species) +
  theme_bw()

mydata_minDistDir %>%
  ggplot() +
  aes(yday2) +
  geom_density(aes(fill = dir), alpha = 0.5) +
  facet_grid(dir~Species) +
  theme_bw()

mydata_minDistDir %>%
  dplyr::filter(!is.na(Month)) %>%
  group_by(Species,Month,dir) %>%
  dplyr::summarise(n=n()) %>%
  mutate(prop = n/sum(n)) %>%
  ggplot() +
  geom_col(aes(x=Month, y = prop, fill = dir)) +
  facet_grid(Species~dir) +
  theme_bw()

mydata_minDistDir %>%
  ggplot() +
  geom_violin(aes(x=yDay, y = dir, color = dir), fill = NA) +
  geom_jitter(aes(x=yDay, y = dir, color = dir), width = 0, height = 0.15) +
  geom_boxplot(aes(x=yDay, y = dir, color = dir), fill = NA) +
  facet_grid(~Species) +
  theme_bw()


# Fit models --------------------------------------------------------------

## For all indivs ----

# Even given increased sampling intensity, how do I predict which times of year correspond to increased activity in south-to-north movements?
mdf <- mydata_minDistDir %>%
  dplyr::filter(
    !is.na(dist_km),
    !is.na(OriginCluster),
    !is.na(yDay),
    !is.na(N),
    !is.na(wind_killed)
  ) %>%
  mutate(
    is_southerly = if_else(dir == "S", 1, 0),
    is_northerly = if_else(dir == "N", 1, 0),
    is_dir       = if_else(dir != "U", 1, 0)
  )

## southerly model ----
m_S1 <- glm(
  is_southerly ~
    commonName +
    poly(yday2, 2) +
    wind_killed +
    commonName:poly(yDay,2),
  data = mdf,
  family = "binomial"
)


d_S1 <- MuMIn::dredge(m_S1, beta = "none")
d_S1 %>% topDredgeModelPredictors
m_S2 <- glm(
  is_southerly ~
    commonName +
    wind_killed +
    commonName:poly(yDay,2),
  data = mdf,
  family = "binomial"
)
d_S2 <- MuMIn::dredge(m_S2, beta = "none")
d_S2 %>% topDredgeModelPredictors
dropVIF(car::vif(m_S2))

sjPlot::plot_model(m_S2, "pred", terms = c("commonName"))
sjPlot::plot_model(m_S2, "pred", terms = c("yDay"))
sjPlot::plot_model(m_S2, "pred", terms = c("wind_killed"))
sjPlot::plot_model(m_S2, "pred", terms = c("yDay", "commonName"))


### southern Model performance. ----
summary(m_S2)
gtsummary::tbl_regression(m_S2, exponentiate = F) %>% add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(m_S2)
anova(m_S2)
caret::varImp(m_S2) %>% arrange(desc(Overall))





## northerly model ----
m_N1 <- glm(
  is_northerly ~
    commonName +
    poly(yday2, 2) +
    wind_killed +
    commonName:poly(yDay,2),
  data = mdf,
  family = "binomial"
)

d_N1 <- MuMIn::dredge(m_N1, beta = "none")
d_N1 %>% topDredgeModelPredictors
m_N2 <- glm(
  is_northerly ~
    commonName +
    poly(yday2, 2) +
    commonName:poly(yDay,2),
  data = mdf,
  family = "binomial"
)
d_N2 <- MuMIn::dredge(m_N2, beta = "none")
d_N2 %>% topDredgeModelPredictors
dropVIF(car::vif(m_N2))


sjPlot::plot_model(m_N2, "pred", terms = c("commonName"))
sjPlot::plot_model(m_N2, "pred", terms = c("yDay"))
sjPlot::plot_model(m_N2, "pred", terms = c("yDay", "commonName"))


pN <-sjPlot::plot_model(m_N2, "pred", terms = c("yDay","commonName"))
pN


### Northern Model performance. ----
summary(m_N2)
gtsummary::tbl_regression(m_N2, exponentiate = F) %>% add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(m_N2)
anova(m_N2)
caret::varImp(m_N2) %>% arrange(desc(Overall))



# Combine preds -----

col_N <- "#e76f51"
col_S <- "#023047"


df_N <- ggplot_build(pN)$plot$data %>% as.data.frame %>% mutate(mod = "N")
df_S <- ggplot_build(pS)$plot$data %>% as.data.frame %>% mutate(mod = "S")
df_wide <- rbind(df_N, df_S) %>%
  dplyr::rename(yDay = x, Species = group) %>%
  mutate(Species = factor(Species, levels = c("Hoary", "Eastern red", "Silver-haired")))

rug_df <- mdf %>%
  dplyr::select(-c(x, Species)) %>%
  dplyr::rename(Species = commonName) %>%
  mutate(Species = factor(Species, levels = c("Hoary", "Eastern red", "Silver-haired")))

# Crop plotted projection to within a certain window of any observations.
windowToPlot <- 30
## By species and direction:
dplyr::rug_df %>%
  dplyr::filter(dir != "U") %>%
  group_by(Species, dir) %>%
  dplyr::summarise(min = min(yDay), max= max(yDay)) %>%
  ungroup %>%
  mutate(start = min - windowToPlot, end = max + windowToPlot) %>%
  rename(mod = dir) %>%
  dplyr::select(Species, mod, start, end) %>%
  right_join(df_wide, by = c("Species", "mod")) %>%
  dplyr::filter(yDay >= start & yDay <= end) ->
  df_wide_filtered
## By species only:
rug_df %>%
  dplyr::filter(dir != "U") %>%
  group_by(Species) %>%
  dplyr::summarise(min = min(yDay), max= max(yDay)) %>%
  ungroup %>%
  mutate(start = min - windowToPlot, end = max + windowToPlot) %>%
  dplyr::select(Species, start, end) %>%
  right_join(df_wide, by = c("Species")) %>%
  dplyr::filter(yDay >= start & yDay <= end) ->
  df_wide_filtered


# Plot --------------------------------------------------------------------



myPlot <- ggplot() +
  # Plot CI's
  geom_ribbon(
    data = df_wide_filtered,
    aes(x=yDay, ymin = conf.low, ymax = conf.high, fill = mod, group = interaction(Species, mod)),
    alpha = 0.1
  ) +
  geom_path(
    data = df_wide_filtered,
    aes(x=yDay, y = conf.low, color = mod, group = interaction(Species, mod) ),
    linetype = 2
  ) +
  geom_path(
    data = df_wide_filtered,
    aes(x=yDay, y = conf.high, color = mod, group = interaction(Species, mod) ),
    linetype = 2
  ) +
  # Plot model predictions.
  geom_path(
    data = df_wide_filtered,
    aes(x=yDay, y = predicted, color = mod, group = interaction(Species, mod) ),
    linetype = 1
  ) +
  # Plot rug plots
  geom_rug(
    data =  dplyr::filter(rug_df, is_northerly == 1) ,
    aes(x=yDay),
    color = col_N,
    sides = "t"
  )  +
  geom_rug(
    data = dplyr::filter(rug_df, is_southerly == 1) ,
    aes(x=yDay),
    color = col_S,
    sides = "b"
  )  +
  # Facet.
  facet_wrap(~Species) +
  # Appearance
  scale_fill_manual(  breaks = c("N", "S"), values = c(col_N, col_S) ) +
  scale_color_manual( breaks = c("N", "S"), values = c(col_N, col_S) ) +
  xlab("Day of Year") +
  ylab("Probability of movement in specified direction") +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "none"
  )


label_df <-
  data.frame(
    label = c("North-to-south", "South-to-north"),
    Species = rep("Hoary", 2),
    x=c(100,00),
    y=c(0.9, 0.2),
    ax1 = c(110, 100),
    ax2 = c(50,120),
    ay1 = c(0.88,0.18),
    ay2 = c(0.8, 0.1)
  ) %>%
  dplyr::mutate(
    Species = factor(Species, levels = c("Hoary", "Eastern red", "Silver-haired"))
  )


(myPlot_labs <-
    myPlot +
    geom_text(
      data = label_df,
      aes(label = label,x=x,y=y), hjust = 0, size = 3
    ) +
    geom_segment(
      data = label_df,
      aes(x=ax1, xend = ax2, y=ay1, yend = ay2)
    )
)

ggsave(
  myPlot_labs,
  filename = file.path(wd$figs, "directionOfMovementByYday.png"),
  width = 8, height = 4
  )
