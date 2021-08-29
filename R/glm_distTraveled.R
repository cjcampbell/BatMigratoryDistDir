## ----setup --------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(sjPlot)
library(car)
library(MuMIn)
library(caret)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )

theme_set(ggpubr::theme_pubclean())
options(na.action = "na.fail")


# Quick fun to return params to include in the next model.
topDredgeModelPredictors <- function(dredgeModelOutput) {
  dredgeModelOutput %>%
    # Find top-performing model
    filter(delta < 2) %>% arrange(df) %>% slice(1) %>% as.data.frame %>%
    # Pull out predictors.
    dplyr::select(-c("(Intercept)", "df", "AICc", "delta", "weight", "logLik")) %>%
    # Make a df.
    t %>% as.data.frame() ->
    step1
  print(paste0("Drop: ", row.names(filter(step1, is.na(V1))) ) )
  print(paste0("Keep: ", row.names(filter(step1, !is.na(V1))) ) )
}

# Quick fun to return params with too-high VIF.
dropVIF <- function(vifOUT) {
  vifOUT %>%
    as.data.frame %>%
    filter(`GVIF^(1/(2*Df))` >= 5) %>%
    row.names() -> p
  print(paste0("Consider dropping: ", p))
}

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


# Model selection ---------------------------------------------------------


## Model A -- didMove ----
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

d1 <- MuMIn::dredge(m1, beta = "none")
d1 %>% topDredgeModelPredictors

m2 <- glm(
  didMove ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    # commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster #+
  # commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf,
  na.action = "na.fail",
  family = binomial(link = "logit")
)

d2 <- MuMIn::dredge(m2)
d2 %>% topDredgeModelPredictors
# Stable model.
dropVIF(car::vif(m2))

# https://stats.stackexchange.com/questions/106344/how-to-quantify-the-relative-variable-importance-in-logistic-regression-in-terms
library(caret)
varImp(m2, scale = FALSE) %>% arrange(desc(Overall))
# Drop common name.

m3 <- glm(
  didMove ~
    # commonName +
    poly(yday2, 2) +
    OriginCluster +
    wind_killed +
    decimalLatitude +
    # commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster #+
  # commonName:poly(yday2, 2):OriginCluster
  ,
  data = mdf,
  na.action = "na.fail",
  family = binomial(link = "logit")
)

d3 <- MuMIn::dredge(m3)
d3 %>% topDredgeModelPredictors
# Stable again.
dropVIF(car::vif(m3))

# Lookin good!
summary(m3)
plot(m3)


list(
  "yday2"                          ,
  "OriginCluster"                  ,
  "wind_killed"                    ,
  "decimalLatitude"                ,
  c("commonName", "OriginCluster") ,
  c("wind_killed", "commonName") ,
  c("decimalLatitude", "commonName") ,
  c("yday2", "OriginCluster" )
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(m3, type = "pred", terms = x) + theme_bw()
  } ) -> modPlots1
gridExtra::grid.arrange(grobs = modPlots1)

## Model B -- how far? ----

mdf2 <- mdf %>% filter(didMove == 1)

gl1 <-
  glm(
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
    na.action = "na.fail",
    family = Gamma(link = "log")
  )
dgl1 <- MuMIn::dredge(gl1, beta = "none")
d1 %>% topDredgeModelPredictors

gl2 <-
  glm(
    dist_km ~
      commonName +
      poly(yday2, 2) +
      OriginCluster +
      wind_killed +
      decimalLatitude +
      #commonName:poly(yday2, 2) +
      commonName:OriginCluster +
      commonName:wind_killed +
      commonName:decimalLatitude +
      poly(yday2, 2):OriginCluster# +
    #commonName:poly(yday2, 2):OriginCluster
    ,
    data = mdf2,
    na.action = "na.fail",
    family = Gamma(link = "log")
  )
dgl2 <- MuMIn::dredge(gl2, beta = "none")
d2 %>% topDredgeModelPredictors

# Stable model.
dropVIF(car::vif(gl2))
varImp(gl2, scale = FALSE) %>% arrange(desc(Overall))
# Drop common name.

gl3 <-
  glm(
    dist_km ~
      #commonName +
      poly(yday2, 2) +
      OriginCluster +
      wind_killed +
      decimalLatitude +
      #commonName:poly(yday2, 2) +
      commonName:OriginCluster +
      commonName:wind_killed +
      commonName:decimalLatitude +
      poly(yday2, 2):OriginCluster# +
    #commonName:poly(yday2, 2):OriginCluster
    ,
    data = mdf2,
    na.action = "na.fail",
    family = Gamma(link = "log")
  )
dgl3 <- MuMIn::dredge(gl3, beta = "none")
d3 %>% topDredgeModelPredictors

dropVIF(car::vif(gl3))
car::vif(gl3)
# A stable model! Woo!!

plot(gl3)
summary(gl3)



list(
  "yday2"                          ,
  "OriginCluster"                  ,
  "wind_killed"                    ,
  "decimalLatitude"                ,
  c("commonName", "OriginCluster") ,
  c("wind_killed", "commonName") ,
  c("decimalLatitude", "commonName") ,
  c("yday2", "OriginCluster" )
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(gl3, type = "pred", terms = x) + theme_bw()
  } ) -> modPlots2
gridExtra::grid.arrange(grobs = modPlots2)

grid.arrange(
  arrangeGrob( grobs = modPlots1, ncol = 1 ),
  arrangeGrob( grobs = modPlots2, ncol = 1 ),
  ncol = 2
)
