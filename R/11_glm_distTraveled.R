## ----setup --------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(sjPlot)
library(car)
library(MuMIn)
library(caret)
library(gtsummary)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )

theme_set(ggpubr::theme_pubclean())
options(na.action = "na.fail")


# Quick fun to return params to include in the next model.
topDredgeModelPredictors <- function(dredgeModelOutput) {
  dredgeModelOutput %>%
    # Find top-performing model
    dplyr::filter(delta < 2) %>% arrange(df) %>% slice(1) %>% as.data.frame %>%
    # Pull out predictors.
    dplyr::select(-c("(Intercept)", "df", "AICc", "delta", "weight", "logLik")) %>%
    # Make a df.
    t %>% as.data.frame() ->
    step1
  print(paste0("Drop: ", row.names(dplyr::filter(step1, is.na(V1))) ) )
  print(paste0("Keep: ", row.names(dplyr::filter(step1, !is.na(V1))) ) )
}

# Quick fun to return params with too-high VIF.
dropVIF <- function(vifOUT) {
  vifOUT %>%
    as.data.frame %>%
    dplyr::filter(`GVIF^(1/(2*Df))` >= 5) %>%
    row.names() -> p
  print(paste0("Consider dropping: ", p))
}

## ----plotPosSkew-----------------------------------------------------------------

ggarrange(
  {
    mydata_minDistDir %>%
      mutate(didMove = as.numeric(as.factor(didMove))) %>%
      ggplot() +
      aes(didMove) +
      geom_histogram() +
      xlab("Movement > 100km observed")
  }, {
    v %>%
      dplyr::filter(didMove == "Y") %>%
      ggplot() +
      aes(dist_km) +
      geom_histogram() +
      xlab("Distance when movement observed")
  }
)


# Model selection ---------------------------------------------------------

mdf <- mydata_minDistDir %>%
  dplyr::filter(
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
#plot(m3)

### Plot. -----

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
    sjPlot::plot_model(m3, type = "pred", terms = x) +
      coord_cartesian(ylim = c(0,1)) +
      theme_pubclean() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  } ) -> modPlots1

#gridExtra::grid.arrange(grobs = modPlots1)
plot_model(gl3, sort.est = TRUE)

## Model performance. ----
summary(m3)
gtsummary::tbl_regression(m3, exponentiate = F) %>% add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(m3)
anova(m3)
caret::varImp(m3) %>% arrange(desc(Overall))



## Model B -- how far? ----

mdf2 <- mdf %>% dplyr::filter(didMove == 1)

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
    data = mdf,
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

#plot(gl3)
summary(gl3)

### Plot -----

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
    sjPlot::plot_model(gl3, type = "pred", terms = x) +
      coord_cartesian(ylim = c(0,700)) +
      theme_pubclean() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  } ) -> modPlots2
#gridExtra::grid.arrange(grobs = modPlots2)

plot_model(gl3, sort.est = TRUE)

## Model performance. ----
# See https://stats.stackexchange.com/questions/431120/how-to-interpret-parameters-of-glm-output-with-gamma-log-link
summary(gl3)
gtsummary::tbl_regression(gl3) %>%
  add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(gl3)
anova(gl3)
caret::varImp(gl3) %>% arrange(desc(Overall))

## Interaction details. ----
modPlots2[[6]] -> o
o$data


# Setup for plotting ------------------------------------------------------
## Setup axes. ----
library(ggpubr)

margins <- list(
  theme(
    #/t/r/b/l/
    plot.margin = unit(c(4, 4, 4, 16), "points"),
    axis.title = element_text(size = 10)
  )
)

blues <- colorRampPalette(c("#03045e", "#00b4d8"))
OriginClusterColors <- list(
  scale_color_manual(
    breaks = 1:4,
    values = blues(4)
  ),
  scale_fill_manual(
    breaks = 1:4,
    values = blues(4)
  )
)
speciesColors <- list(
  scale_color_manual(
    "Species",
    breaks = c("Hoary", "Eastern red", "Silver-haired"),
    values = c("#E09F3E", "#A43828", "#335C67")
  ),
  scale_fill_manual(
    "Species",
    breaks = c("Hoary", "Eastern red", "Silver-haired"),
    values = c("#E09F3E", "#A43828", "#335C67")
  )
)
dayOfYear_x <- list(
  scale_x_continuous(
    "Day of year",
    breaks = c(seq(150,365, 50), 365, seq(415, 600, 50)),
    labels = c(c(150, 200, 250, 300, 350), c( 365,  415, 465, 515, 565)-365)
  )
)
wind_killed <- list(
  scale_x_continuous(
    "Wind killed",
    breaks = c(1,2),
    labels = c("no", "yes"),
    expand = c(0.15,0.15)
  )
)
lat <- list(
  scale_x_continuous(
    "Sampling latitude"
  )
)

species_x <- list(
  scale_x_continuous(
    "Species",
    breaks = c(1,2,3),
    labels = c("Hoary", "Eastern red", "Silver-haired"),
    expand = c(0.15,0.15)
  )
)

dist_y <- list(
  scale_y_continuous(
    "Min. dist (km)",
    position = "right",
    breaks = seq(0,750,by=150),
    labels = seq(0,750,by=150)
  ),
  theme(
    plot.title = element_blank()
  )
)
prob_y <- list(
  scale_y_continuous(
    "Prob. of movement",
    position = "left",
    limits = c(0,1)
  ),
  theme(
    plot.title = element_blank()
  )
)

legendPosition <- "bottom"


# Plot each significant predictor -----------------------------------------------------------------

## Arrange -----
myGrobs <- list(

  ggarrange(
    plotlist = list(

      ggarrange(
        plotlist = list(
          modPlots1[[1]] + margins + prob_y + dayOfYear_x,
          modPlots2[[1]] + margins + dist_y + dayOfYear_x
        ),
        labels = c(LETTERS[1:2])
      ),
      ggarrange(
        plotlist = list(
          modPlots1[[2]] + margins + prob_y ,
          modPlots2[[2]] + margins + dist_y
        ),
        labels = c(LETTERS[3:4])
      ),
      ggarrange(
        plotlist = list(
          modPlots1[[3]] + margins + prob_y + wind_killed,
          modPlots2[[3]] + margins + dist_y + wind_killed
        ),
        labels = c(LETTERS[5:6])
      ),
      ggarrange(
        plotlist = list(
          modPlots1[[4]] + margins + prob_y + lat,
          modPlots2[[4]] + margins + dist_y + lat
        ),
        labels = c(LETTERS[7:8])
      )

    )
  ),

  ggpubr::ggarrange(
    plotlist = list(
      modPlots1[[5]] + margins + prob_y + OriginClusterColors + species_x,
      modPlots2[[5]] + margins + dist_y + OriginClusterColors + species_x,
      modPlots1[[8]] + margins + prob_y + OriginClusterColors + dayOfYear_x,
      modPlots2[[8]] + margins + dist_y + OriginClusterColors + dayOfYear_x
    ),
    ncol = 2, nrow = 2, common.legend = T, legend = legendPosition,
    labels = c(LETTERS[9:12]), hjust=-0.1, vjust = -1
  ) +
    theme(plot.margin = margin(1,0,0,0, "cm")) ,

  ggpubr::ggarrange(
    plotlist = list(
      modPlots1[[6]] + margins + prob_y + speciesColors + wind_killed,
      modPlots2[[6]] + margins + dist_y + speciesColors + wind_killed,
      modPlots1[[7]] + margins + prob_y + speciesColors + lat,
      modPlots2[[7]] + margins + dist_y + speciesColors + lat
    ),
    ncol = 2, nrow = 2, common.legend = T, legend = legendPosition,
    labels = c(LETTERS[12:15]), hjust=-0.1, vjust = -1
  ) +
    theme(plot.margin = margin(1,0,0,0, "cm"))

)


arrangeGrob(
  grobs = myGrobs, ncol=1, nrow= 3
) -> bigP

## Save ----
ggsave(bigP, filename = file.path(wd$figs, "SI_allDistanceModelResults.png"),
       width = 8, height = 11)



# Figure 3 Selected interactions---------------------------------------------------------

## Effect of doy -----------------------------------------------------------

p1 <- sjPlot::plot_model(m3, type = "pred", terms = c("yday2","commonName"))
doy_spp1 <- p1$data %>%
  ggplot() +
  aes(x=x, color = group, fill = group) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
  geom_path(aes(y=predicted), size = 1.5) +
  speciesColors +
  dayOfYear_x +
  prob_y
p2 <- sjPlot::plot_model(gl3, type = "pred", terms = c("yday2","commonName"))
doy_spp2 <- p2$data %>%
  ggplot() +
  aes(x=x, color = group, fill = group) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
  geom_path(aes(y=predicted), size = 1.5) +
  speciesColors +
  dayOfYear_x +
  dist_y

plot_doySpp <- ggpubr::ggarrange( plotlist = list( doy_spp1, doy_spp2 ), common.legend = T )



## lat -----------------------------------------------------------
lat_orig_spp1 <- sjPlot::plot_model(m3, type = "pred", terms = c("decimalLatitude","commonName")) %>%
  {
    .$data %>%
      ggplot() +
      aes(x=x, color = group, fill = group) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
      geom_path(aes(y=predicted), size = 1.5) +
      prob_y +
      speciesColors +
      lat
  }


# So step 2 is contingent on IF movement is detected! So we can only ever bottom out IF
lat_orig_spp2 <- sjPlot::plot_model(gl3, type = "pred", terms = c("decimalLatitude","commonName")) %>%
  {
    .$data %>%
      ggplot() +
      aes(x=x, color = group, fill = group) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
      geom_path(aes(y=predicted), size = 1.5) +
      dist_y +
      speciesColors +
      lat
  }

plot_lat <- ggpubr::ggarrange(
  plotlist = list( lat_orig_spp1, lat_orig_spp2 ),
  common.legend = T ,
  ncol = 2)


## Sampling method ---------------------------------------------------------

# Add sampling method, w respect to species.
sjPlot::plot_model(m3, type = "pred", terms = c("wind_killed","commonName")) %>%
  {.$data} %>%
  mutate(
    x_adjusted = case_when(
      group == "Hoary" ~ x-0.05,
      group == "Silver-haired" ~ x + 0.05,
      TRUE ~ x
    )
  ) %>%
  ggplot() +
  aes(x=x_adjusted,y=predicted, color = group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05) +
  geom_point(size = 3) +
  geom_path(linetype = 2, size = 0.25) ->
  o1
p_sam1 <- o1 +
  scale_y_continuous(
    limits = c(0,1)
  ) +
  speciesColors +
  scale_x_continuous(
    "Sampling method",
    breaks = c(1,2),
    labels = c("Live-caught, etc.", "Wind killed"),
    limits = c(0.5,2.5)
  ) +
  prob_y

sjPlot::plot_model(gl3, type = "pred", terms = c("wind_killed","commonName")) %>%
  {.$data} %>%
  mutate(
    x_adjusted = case_when(
      group == "Hoary" ~ x-0.05,
      group == "Silver-haired" ~ x + 0.05,
      TRUE ~ x
    )
  ) %>%
  ggplot() +
  aes(x=x_adjusted,y=predicted, color = group) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05) +
  geom_point(size = 3) +
  geom_path(linetype = 2, size = 0.25) ->
  o2
p_sam2 <- o2 +
  dist_y +
  speciesColors +
  scale_x_continuous(
    "Sampling method",
    breaks = c(1,2),
    labels = c("Live-caught, etc.", "Wind killed"),
    limits = c(0.5,2.5)
  )

plot_sampling <- ggpubr::ggarrange(
  plotlist = list(
    p_sam1,
    p_sam2
  ),
  common.legend = T
)


## OriginCluster -----------------------------------------------------------
clust_1 <- sjPlot::plot_model(m3, type = "pred", terms = c("OriginCluster","commonName")) %>%
  {
    .$data %>%
      ggplot() +
      aes(x=x, color = group, fill = group) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
      geom_path(aes(y=predicted), size = 1.5) +
      prob_y +
      speciesColors +
      scale_x_continuous("OriginCluster")
  }


clust_2 <- sjPlot::plot_model(gl3, type = "pred", terms = c("OriginCluster","commonName")) %>%
  {
    .$data %>%
      ggplot() +
      aes(x=x, color = group, fill = group) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha =0.1, linetype = 1, size = 0.15) +
      geom_path(aes(y=predicted), size = 1.5) +
      dist_y +
      speciesColors +
      scale_x_continuous("OriginCluster")
  }



## Combine -----------------------------------------------------------------

(
  BigPlotBySpecies <-
    ggpubr::ggarrange(
      plotlist = list(
        doy_spp1, doy_spp2 ,
        p_sam1, p_sam2,
        lat_orig_spp1, lat_orig_spp2,
        clust_1, clust_2
      ),
      common.legend = T, labels = c(LETTERS[1:8]),
      legend = legendPosition, hjust=-0.1, vjust = -0.7,
      ncol = 2, nrow = 4
      ) +
    theme(plot.margin = margin(1,0,0,0, "cm"))
)


ggsave(BigPlotBySpecies, filename = file.path(wd$figs, "distanceModelResults.png"),
       width = 8, height = 11)
