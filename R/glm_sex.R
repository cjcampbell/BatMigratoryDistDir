## ----setup --------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )

# Find latitude of a near-ish point hitting a particular threshold:
coords_in_dd <- readRDS( file.path(wd$bin, "coords_all.rds") ) %>%
  dplyr::select(x,y,y_dd) %>%
  distinct
latDeets <- mydata_minDistDir %>%
  filter(quantOrder == 0.05, conf_threshold == 0.25 ) %>%
  dplyr::select(ID, x, y) %>%
  left_join(., coords_in_dd, by = c("x", "y")) %>%
  rename(latOf5thPercentileClosestLikelyPt = y_dd) %>%
  dplyr::select(ID,latOf5thPercentileClosestLikelyPt)

# Specify working df.
mdf <- mydata_minDistDir

## ---- simple count by wind killed y/n figure ---------------------------------

mdf %>%
  filter(!is.na(Sex)) %>%
  ggplot() +
  aes(x = OriginCluster, fill = Sex) %>%
  geom_bar(stat ="count", position=position_dodge()) +
  facet_grid(wind_killed~Species) +
  theme_bw() +
  labs( title = "Sampling Counts")


## ----proportion female at wind killed y/n figure -----------------------------

mdf %>%
  filter(!is.na(Sex)) %>%
  group_by(Species, OriginCluster, wind_killed, Sex) %>% summarise(n=n()) %>%
  group_by(Species, OriginCluster,wind_killed) %>% mutate(prop = n/sum(n)) %>%
  filter(Sex == "F") %>%
  ggplot() +
  geom_hline(yintercept=0.5, linetype = 2) +
  aes(x = OriginCluster, y = prop, fill = wind_killed) +
  geom_col(position = position_dodge(), color = "grey50") +
  scale_fill_viridis_d(option = 5) +
  facet_wrap(~Species) +
  facet_grid(wind_killed~Species) +
  theme_bw() +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(hjust=0)
  ) +
  ggtitle("Proportion of female bats sampled live vs. at wind turbines")


## ----fit logistic model ------------------------------------------------------

mdf2 <- mdf %>%
  filter(!is.na(Sex)) %>%
  mutate(sex_01 = case_when(Sex == "F" ~ 1, Sex == "M" ~ 0, TRUE ~ as.numeric(NA)))

# logistic regression to do that.
m_sex <- glm( sex_01 ~ OriginCluster + wind_killed + Species:OriginCluster:wind_killed + Species:OriginCluster + Species:wind_killed,
  data = mdf2, na.action = "na.fail",
  family = binomial(link = "logit")
  )

car::vif(m_sex)
d_sex <- MuMIn::dredge(m_sex)


#tab_model(m_sex)
m_sum <- summary(m_sex)
m_sum$coefficients %>%
  as.data.frame %>%
  mutate(sig = if_else(`Pr(>|z|)` <= 0.05, "*", "")) %>%
  mutate_if(is.numeric, signif, digits = 2) %>%
  knitr::kable()

pp <- sjPlot::plot_model(m_sex, type = "pred", terms = c("OriginCluster", "wind_killed", "Species"))

pp +
  ggpubr::theme_pubr() +
  scale_color_viridis_d(option = 5) +
  scale_fill_viridis_d( option = 5) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  ylab("Likelihood of individual being FEMALE") +
  ggtitle("Predicted probability of sex:female") +
  theme(
    legend.position = "right"
  )

