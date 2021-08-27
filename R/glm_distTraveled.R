## ----setup --------------------------------------------------------
library(tidyverse)
library(sf)
library(ggpubr)
library(gridExtra)
library(sjPlot)

library(car)
library(MuMIn)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )

theme_set(ggpubr::theme_pubclean())
options(na.action = "na.fail")


## ----transformVariables----------------------------------------------------------

mdf <- mydata_minDistDir %>%
  filter(
    !is.na(dist_km),
    !is.na(OriginCluster),
    !is.na(yDay)
    ) %>%
  dplyr::mutate(
  didMove = case_when(
    dist  %in% c("mid", "long") ~ 1,
    dist  %in% c("short")  ~ 0
    )
  )

## ----plotPosSkew-----------------------------------------------------------------

ggarrange(
  {
    mdf %>%
      ggplot() +
      aes(didMove) +
      geom_histogram() +
      xlab("Movement > 100km observed")
  }, {
    mdf %>%
      filter(didMove == 1) %>%
    ggplot() +
    aes(dist_km) +
    geom_histogram() +
    xlab("Distance when movement observed")
  }
)


## ----prep df, echo = F, message=F, results=FALSE---------------------------------
m1 <- glm(
  didMove ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf,
  na.action = "na.fail",
  family = binomial(link = "logit")
)

d1 <- MuMIn::dredge(m1)
d1 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View

m2 <- glm(
  didMove ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    #commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster #+
  #commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf,
  na.action = "na.fail",
  family = binomial(link = "logit")
)

d2 <- MuMIn::dredge(m2)
d2 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(m2)
m2$coefficients

m3 <- glm(
  didMove ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    #commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster #+
  #commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf,
  na.action = "na.fail",
  family = binomial(link = "logit")
)

d3 <- MuMIn::dredge(m3)
d3 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(m3)

list(
  "commonName",
  "yday2"                          ,
  "OriginCluster"                  ,
  "wind_killed"                    ,
  "decimalLatitude",
  c("OriginCluster", "commonName"),
  c("wind_killed", "commonName"),
  c("yday2", "OriginCluster")
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(m3, type = "pred", terms = x)
  } ) -> modPlots1
grid.arrange(grobs = modPlots1)

#### Part 2. #####
# For bats that *did* move, what predicted how far they would move?
mdf2 <- filter(mdf, didMove == 1)

mdf2$dist_km %>% hist

r1 <- glm(
  dist_km ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf2,
  family = Gamma
)
e1 <- MuMIn::dredge(r1)
e1 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(r1)
r1$coefficients

r2 <- glm(
  dist_km ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf2,
  family = Gamma
)
e2 <- MuMIn::dredge(r2)
e2 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(r2)
r2$coefficients

r3 <- glm(
  dist_km ~
    #commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf2,
  family = Gamma
)
e3 <- MuMIn::dredge(r3)
e3 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(r3)
r3$coefficients

r4 <- glm(
  dist_km ~
    #commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    #poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf2,
  family = Gamma
)
e4 <- MuMIn::dredge(r4)
e4 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(r4)
#commonName, yday2, commonName:OriginCluster, or poly(yday2, 2):OriginCluster
r4$coefficients

r5 <- glm(
  dist_km ~
    #commonName +
    #poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    #poly(yday2, 2):OriginCluster +
    commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf2,
  family = Gamma
)
e5 <- MuMIn::dredge(r5)
e5 %>% filter(delta < 2) %>% arrange(df) %>% slice(1) %>% View
car::vif(r5)

list(
  "OriginCluster"                  ,
  "wind_killed"                    ,
  "decimalLatitude",
  c("yday2", "commonName"),
  c("OriginCluster", "commonName"),
  c("wind_killed", "commonName"),
  c("yday2", "OriginCluster")
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(r5, type = "pred", terms = x) + ylim(0,750)
  } ) -> modPlots2
grid.arrange(grobs = modPlots2)

