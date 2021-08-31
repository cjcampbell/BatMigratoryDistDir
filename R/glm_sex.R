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
  facet_grid(wind_killed~commonName) +
  theme_bw() +
  labs( title = "Sampling Counts")


## ----proportion female at wind killed y/n figure -----------------------------

mdf %>%
  filter(!is.na(Sex)) %>%
  group_by(commonName, OriginCluster, wind_killed, Sex) %>% summarise(n=n()) %>%
  group_by(commonName, OriginCluster,wind_killed) %>% mutate(prop = n/sum(n)) %>%
  filter(Sex == "F") %>%
  ggplot() +
  geom_hline(yintercept=0.5, linetype = 2) +
  aes(x = OriginCluster, y = prop, fill = wind_killed) +
  geom_col(position = position_dodge(), color = "grey50") +
  scale_fill_viridis_d(option = 5) +
  facet_wrap(~commonName) +
  facet_grid(wind_killed~commonName) +
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


#tab_model(m_sex)
m_sum <- summary(d_sex2)
m_sum$coefficients %>%
  as.data.frame %>%
  mutate(sig = if_else(`Pr(>|z|)` <= 0.05, "*", "")) %>%
  mutate_if(is.numeric, signif, digits = 2) %>%
  knitr::kable()


list(
  "commonName"                     ,
  "OriginCluster"                  ,
  "wind_killed"                    ,
  c("OriginCluster", "commonName")
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(m_sex2, type = "pred", terms = x) + theme_bw()
  } ) -> sexPlots
gridExtra::grid.arrange(grobs = sexPlots)

pp <- sjPlot::plot_model(m_sex, type = "pred", terms = c("OriginCluster", "wind_killed", "commonName"))

(myPlot_sex <- pp +
  ggpubr::theme_pubr() +
    scale_color_manual(
      breaks = c("yes", "no"),
      values = c("#e76f51", "#2a9d8f")
    ) +
    scale_fill_manual(
      breaks = c("yes", "no"),
      values = c("#e76f51", "#2a9d8f")
    ) +
  geom_hline(yintercept = 0.5, linetype = 2) +
  ylab("Likelihood of individual identified as female") +
  ggtitle("Predicted probability of sex:female") +
  theme(
    legend.position = "right"
  )
  )

ggsave(myPlot_sex, filename = file.path(wd$figs, "model_sex_ouput.png"))



# New plot ----------------------------------------------------------------

(ggplot_build(myPlot_sex)$plot$data %>%
  as.data.frame %>%
  dplyr::rename(wind_killed = group_col, Species = facet) %>%
  ggplot() +
  facet_wrap(~Species) +
  geom_hline(yintercept = 0.5, linetype = 1, color = "grey80") +
  geom_ribbon( aes(x=x, ymin = conf.low, ymax = conf.high, fill = wind_killed), alpha = 0.25 ) +
  geom_path( aes(x=x, y=conf.low , color = wind_killed) , linetype = 2 ) +
  geom_path( aes(x=x, y=conf.high, color = wind_killed) , linetype = 2 ) +
  geom_path( aes(x=x, y=predicted, color = wind_killed) ) +
  scale_y_continuous(
    "Probability of identification as female",
    limits = c(0,1),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    "OriginCluster",
    breaks = 1:4,
    expand = c(0.1,0.1)
  ) +
  scale_color_manual(
    breaks = c("no", "yes"),
    labels = c("Live-caught, other sampling methods", "Wind carcass salvage"),
    values = c("#2a9d8f","#e76f51")
  ) +
  scale_fill_manual(
    breaks = c("no", "yes"),
    labels = c("Live-caught, other sampling methods", "Wind carcass salvage"),
    values = c("#2a9d8f","#e76f51")
  ) +
  theme(
    axis.line = element_line(),
    strip.background = element_rect(fill = NA)
  ) ->
  myPlot_sex2)

myPlot_sex2 +
  theme(
    legend.position = c(0.01,1),
    legend.justification = c("left", "top"),
    legend.title = element_blank()
    ) +
  geom_text(
    data = data.frame(
      label = c("Southerly\nsummer\norigin", "Northerly\nsummer\norigin"),
      x=c(1+0.2, 4-0.2),
      y=rep(0.15),
      Species = rep(factor("Hoary", levels = levels(myPlot_sex$data$facet)))
      ),
    aes(x=x,y=y,label=label),
    hjust = 0.5, vjust = 1,
    size = 3
  ) +
  geom_segment(
    data = data.frame(
      xstart = 1.85,
      xend = 3.15,
      y=rep(0.1),
      Species = rep(factor("Hoary", levels = levels(myPlot_sex$data$facet)))
    ),
    aes(x=xstart, xend = xend, y= y, yend = y),
    lineend = "round", linejoin = "mitre",
    size = 1.5, arrow = arrow(length = unit(0.12, "inches"))
  ) +
  geom_segment(
    data = data.frame(
      xstart = 1.85,
      xend = 3.15,
      y=rep(0.1),
      Species = rep(factor("Hoary", levels = levels(myPlot_sex$data$facet)))
    ),
    aes(xend=xstart, x = xend, y= y, yend = y),
    lineend = "round", linejoin = "mitre",
    size = 1.5, arrow = arrow(length = unit(0.12, "inches"))
  ) ->
  myPlot_sex3


ggsave(myPlot_sex3, filename = file.path(wd$figs, "model_sex_ouput2.png"),
       width = 6, height = 4)
