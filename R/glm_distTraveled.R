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
    !is.na(yDay),
    !is.na(N)
    ) %>%
  mutate( d = log(1+dist_km) )



## ----plotPosSkew-----------------------------------------------------------------

ggarrange(
  {
    mdf %>%
      ggplot() +
      aes(dist_km) +
      geom_histogram() +
      xlab("Minimum distance traveled (km)")
  }, {
    mdf %>%
    ggplot() +
    aes(d) +
    geom_histogram() +
    xlab("ln( Minimum distance traveled [km] + 1 )")
  }
)


## ----prep df, echo = F, message=F, results=FALSE---------------------------------


# First model
m1 <- glm(
  d ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    poly(N, 2) +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:poly(N, 2) +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster + ## CPNSIDER  DROPPINGS
    poly(yday2, 2):poly(N, 2) +
    commonName:poly(yday2, 2):OriginCluster +
    commonName:poly(N, 2):poly(yday2, 2),
  data = mdf
  )

d1 <- MuMIn::dredge(m1)
View(d1)

subset(d1, delta == 0)

# Second model.
m2 <- glm(
  d ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    poly(N, 2) +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:poly(N, 2) +
    commonName:wind_killed +
    commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    poly(yday2, 2):poly(N, 2) +
    commonName:poly(yday2, 2):OriginCluster# +
    #commonName:poly(N, 2):poly(yday2, 2)
    ,
  data = mdf
  )
car::vif(m2)

# Third model.
# Remove ONE predictor for which GVIF^(1/(2*Df)) > 5,THE BIGGEST
m3 <- glm(
  d ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    poly(N, 2) +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:poly(N, 2) +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    poly(yday2, 2):poly(N, 2) +
    commonName:poly(yday2, 2):OriginCluster# +
    #commonName:poly(N, 2):poly(yday2, 2)
    ,
  data = mdf
  )
car::vif(m3)
# And again
m4 <- glm(
  d ~
    commonName +
    poly(yday2, 2) +
    OriginCluster +
    poly(N, 2) +
    wind_killed +
    decimalLatitude +
    commonName:poly(yday2, 2) +
    commonName:OriginCluster +
    commonName:poly(N, 2) +
    commonName:wind_killed +
    #commonName:decimalLatitude +
    poly(yday2, 2):OriginCluster +
    poly(yday2, 2):poly(N, 2) #+
    #commonName:poly(yday2, 2):OriginCluster# +
    #commonName:poly(N, 2):poly(yday2, 2)
    ,
  data = mdf
  )
car::vif(m4)

# Dredge again to see if we are at a stable model
d4 <- dredge(m4)
#View(d4)

# NICE.


## ----modelPerformance------------------------------------------------------------
car::vif(m4)%>% knitr::kable()
summary(m4)
#summary(m4)$coefficients %>% knitr::kable()
car::Anova(m4)

plot(m4)



## ----modelTable------------------------------------------------------------------
library(gtsummary)
m4 %>% gtsummary::tbl_regression(.)
# tab_model(m4, CSS = list(css.depvarhead = '+color: red;'))


# Model out ---------------------------------------------------------------

o <- sjPlot::plot_model(m4)

o +
  geom_hline(yintercept = 0, linetype = 2)


## ----modelPredictions------------------------------------------------------------

raiseTo <- function(x) { round(exp(x)-1 )}

universalPreds <- list(
  labs(
    title = "Predicted value of min. distance traveled"
  ),
  scale_y_continuous(
    "Min. distance Traveled (km)",
    breaks = scales::breaks_extended(5),
    labels = raiseTo
  )
)

dayOfYear_x <- list(
  scale_x_continuous(
    "Day of Year",
    breaks = c(seq(150,365, 50), 365, seq(415, 600, 50)),
    labels = c(c(150, 200, 250, 300, 350), c( 365,  415, 465, 515, 565)-365)
  )
)

fillColorLegend <- list(
  guides(color=guide_legend(title="Probability of northerly origin", title.position = "top", title.hjust = 0.5), fill=guide_legend(title="Probability of northerly origin", title.position = "top", title.hjust = 0.5))
)

list(
   "commonName"                        ,
   "yday2"                          ,
   "OriginCluster"                  ,
   "N"                              ,
   "wind_killed"                    ,
   "decimalLatitude"                ,
   c( "yday2","commonName")            ,
   c("commonName", "OriginCluster" )   ,
   c("N", "commonName")                ,
   c("commonName", "wind_killed"   )   ,
   c("yday2", "OriginCluster" )     ,
   c("yday2" , "N")
) %>%
  lapply(., function(x) {
    sjPlot::plot_model(m4, type = "pred", terms = x)  + universalPreds
  } ) -> modPlots


sjPlot::plot_model(m4,  show.values = TRUE, value.offset = .3)
modPlots[[1]] + xlab("Species")
modPlots[[2]] + dayOfYear_x
modPlots[[3]]
modPlots[[4]] + scale_x_continuous("Probability of summering north of sampling location")
modPlots[[5]] + xlab("Wind turbine sampled")
modPlots[[6]] + scale_x_continuous("Decimal latitude of sample site")
modPlots[[7]] + dayOfYear_x
modPlots[[8]] + xlab("Species")
modPlots[[9]] + scale_x_continuous("Probability of summering north of sampling location")
modPlots[[10]] + guides(color=guide_legend(title="Wind turbine sampled", title.position = "top", title.hjust = 0.5) ) + xlab("Species")

modPlots[[11]] + dayOfYear_x +
  guides(
    color=guide_legend(
      title="OriginCluster", title.position = "top", title.hjust = 0.5),
    fill = guide_legend(
      title="OriginCluster", title.position = "top", title.hjust = 0.5)
    )  +
  scale_color_viridis_d( option = 4, end = 0.7 ) +
  scale_fill_viridis_d(  option = 4, end = 0.7 )
# "yday2" , "N"

modPlots[[12]] <- sjPlot::plot_model(m4, terms = c("yday2" , "N[0.2,0.5,0.8]"), type = "pred")  + universalPreds + dayOfYear_x +
    guides(
    color=guide_legend(
      title="Probability of summering north of sampling location", title.position = "top", title.hjust = 0.5),
    fill = guide_legend(
      title="Probability of summering north of sampling location", title.position = "top", title.hjust = 0.5)
    )
modPlots[[12]]

