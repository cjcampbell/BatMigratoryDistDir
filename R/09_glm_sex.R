
# Model drivers of probability of sex ID.

## ----setup --------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(caret)
library(ggpubr)
library(gtsummary)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") ) %>%
  dplyr::mutate(commonName = factor(commonName, levels = mySpecies[c(3,1,2)]))

# Find latitude of a near-ish point hitting a particular threshold:
coords_in_dd <- readRDS( file.path(wd$bin, "coords_all.rds") ) %>%
  dplyr::select(x,y,y_dd) %>%
  distinct
latDeets <- mydata_minDistDir %>%
  dplyr::filter(quantOrder == 0.05, conf_threshold == 0.25 ) %>%
  dplyr::select(ID, x, y) %>%
  left_join(., coords_in_dd, by = c("x", "y")) %>%
  rename(latOf5thPercentileClosestLikelyPt = y_dd) %>%
  dplyr::select(ID,latOf5thPercentileClosestLikelyPt)

# Specify working df.
mdf <- mydata_minDistDir

## ---- simple count by wind killed y/n figure ---------------------------------

mdf %>%
  dplyr::filter(!is.na(Sex)) %>%
  ggplot() +
  aes(x = OriginCluster, fill = Sex) %>%
  geom_bar(stat ="count", position=position_dodge()) +
  facet_grid(wind_killed~commonName) +
  theme_bw() +
  labs( title = "Sampling Counts")


## ----proportion female at wind killed y/n figure -----------------------------

p_sex_rawdata <- mdf %>%
  dplyr::filter(!is.na(Sex)) %>%
  group_by(commonName, OriginCluster, wind_killed, Sex) %>% summarise(n=n()) %>%
  group_by(commonName, OriginCluster, wind_killed) %>% mutate(prop = n/sum(n)) %>%
  dplyr::filter(Sex == "F") %>%
  ggplot() +
  geom_hline(yintercept=0.5, linetype = 2) +
  aes(x = OriginCluster, y = prop, fill = wind_killed) +
  geom_col(position = position_dodge(), color = "grey50") +
  scale_y_continuous("Proportion female", expand = expansion(c(0,0.1))) +
  scale_fill_viridis_d(option = 5) +
  facet_wrap(~commonName) +
  facet_grid(wind_killed~commonName) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust=0)
  ) +
  ggtitle("Proportion of female bats sampled live vs. at wind turbines")
(p_sex_rawdata)
ggsave(p_sex_rawdata, filename = file.path(wd$figs, "p_sex_rawdata.png"))


## ----fit logistic model ------------------------------------------------------

mdf2 <- mdf %>%
  dplyr::filter(!is.na(Sex)) %>%
  mutate(sex_01 = case_when(Sex == "F" ~ 1, Sex == "M" ~ 0, TRUE ~ as.numeric(NA)))

# logistic regression to do that.
m_sex1 <- glm(
  sex_01 ~
    commonName +
    OriginCluster +
    wind_killed +
    commonName:OriginCluster +
    commonName:wind_killed +
    OriginCluster:wind_killed +
    commonName:OriginCluster:wind_killed
   ,
  data = mdf2, na.action = "na.fail",
  family = binomial(link = "logit")
  )

d_sex1 <- MuMIn::dredge(m_sex1)
topDredgeModelPredictors(d_sex1)

m_sex2 <- glm(
  sex_01 ~
    commonName +
    OriginCluster +
    wind_killed +
    commonName:OriginCluster #+
    #commonName:wind_killed +
    #OriginCluster:wind_killed +
    #commonName:OriginCluster:wind_killed
  ,
  data = mdf2, na.action = "na.fail",
  family = binomial(link = "logit")
)
d_sex2 <- MuMIn::dredge(m_sex2)
topDredgeModelPredictors(d_sex2)
car::vif(m_sex2)
# Top model!

sjPlot::plot_model(m_sex2)

## Model performance. ----
summary(m_sex2)
gtsummary::tbl_regression(m_sex2, intercept = TRUE) %>%
  add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(m_sex2)
anova(m_sex2)
caret::varImp(m_sex2) %>% arrange(desc(Overall))

# Plot results ------------------------------------------------------------

## All responses ---

list(
  "commonName" ,
  "OriginCluster"   ,
  "wind_killed"                    ,
  c("OriginCluster","commonName")
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(m_sex2, type = "pred", terms = x)
  } ) %>%
  {gridExtra::grid.arrange(grobs = .)}

## Main plot ----

# New plot with only good sampling ----------------------------------------
speciesColors <- list(
  scale_color_manual(
    "Species",
    breaks = mySpecies,
    values = c("#E09F3E", "#A43828", "#335C67")
  ),
  scale_fill_manual(
    "Species",
    breaks = mySpecies,
    values = c("#E09F3E", "#A43828", "#335C67")
  )
)
myPlot_sex <- sjPlot::plot_model(m_sex2, type = "pred", terms = c("OriginCluster","wind_killed", "commonName"))
myPlot_sex4 <- ggplot_build(myPlot_sex)$plot$data %>%
   as.data.frame %>%
   dplyr::rename(wind_killed = group_col, Species = facet) %>%
  dplyr::filter(wind_killed == "no") %>%
   ggplot() +
  facet_grid(~Species) +
   geom_hline(yintercept = 0.5, linetype = 1, color = "grey80") +
   geom_ribbon( aes(x=x, ymin = conf.low, ymax = conf.high, fill = Species, color = Species), alpha = 0.2, linetype = 1, size = 0.15) +
   geom_path( aes(x=x, y=predicted, color = Species) , size = 1) +
    speciesColors +
   scale_y_continuous(
     "Probability of identification as female",
     limits = c(0,1),
     expand = c(0,0)
   ) +
   scale_x_continuous(
     "Relative latitude of summer origin",
     breaks = 1:4,
     expand = c(0.1,0.1)
   ) +
   theme(
     axis.line = element_line(),
     strip.background = element_rect(fill = NA),
     plot.background = element_rect(fill = "white"),
     panel.background = element_rect(fill = "white"),
     legend.position = "none"
   )  +
  geom_text(
    data = data.frame(
      label = c("Southerly\nsummer\norigin", "Northerly\nsummer\norigin"),
      x=c(1+0.2, 4-0.2),
      y=rep(0.2),
      Species = rep(factor("Silver-haired", levels = levels(myPlot_sex$data$facet)))
    ),
    aes(x=x,y=y,label=label),
    hjust = 0.5, vjust = 1,
    size = 3
  ) +
  geom_segment(
    data = data.frame(
      xstart = 1.85,
      xend = 3.15,
      y=rep(0.15),
      Species = rep(factor("Silver-haired", levels = levels(myPlot_sex$data$facet)))
    ),
    aes(x=xstart, xend = xend, y= y, yend = y),
    lineend = "round", linejoin = "mitre",
    size = 1, arrow = arrow(length = unit(0.12, "inches"))
  ) +
  geom_segment(
    data = data.frame(
      xstart = 1.85,
      xend = 3.15,
      y=rep(0.15),
      Species = rep(factor("Silver-haired", levels = levels(myPlot_sex$data$facet)))
    ),
    aes(xend=xstart, x = xend, y= y, yend = y),
    lineend = "round", linejoin = "mitre",
    size = 1, arrow = arrow(length = unit(0.12, "inches"))
  )
(myPlot_sex4)
saveRDS(myPlot_sex4, file = file.path(wd$bin, "myPlot_sex4.rds"))
ggsave(myPlot_sex4, filename = file.path(wd$figs, "myPlot_sex4.png"), width = 6, height = 3.5)
