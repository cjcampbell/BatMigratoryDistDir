
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

# An important interpretation note for anyone checking out this code--
# individual level "direction" results are presented as "which direction is
# the likely summer origin from the sample site?". As a result, the models are
# called 'is-southerly' and 'p_S' and the like, again referring to the direction of the
# origin relative to the sample site. For clarity in-text, we chose to discuss
# the direction of the sample site relative to the summer origins (which flows
# more intuitively from past origin to later sample site). This results in
# an apparent "flip" of the labels relative to object names-- not an error!

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

mydata_minDistDir %>% count(Species, Sourcefile,dir) %>% ggplot() + geom_bar(aes(y=n, x = Sourcefile , fill = dir), stat="identity") + facet_grid(rows = vars(Species)) + scale_color_viridis_d()


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
    origin_is_southerly = if_else(dir == "S", 1, 0),
    origin_is_northerly = if_else(dir == "N", 1, 0),
    is_dir       = if_else(dir != "U", 1, 0)
  )

## southerly model ----
m_S1 <- glm(
  origin_is_southerly ~
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
  origin_is_southerly ~
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


pS <-sjPlot::plot_model(m_S2, "pred", terms = c("yDay","commonName"))

### southern origin Model performance. ----
summary(m_S2)
gtsummary::tbl_regression(m_S2, exponentiate = F, intercept = T) %>% add_q() %>% bold_p(t = 0.10, q = TRUE) %>% italicize_levels()
performance::r2(m_S2)
anova(m_S2)
caret::varImp(m_S2) %>% arrange(desc(Overall))

### Additional calculations referenced in the ms: ----
pS$data %>%
  as.data.frame %>%
  group_by(group) %>%
  arrange(desc(predicted)) %>%
  slice(1)

hoaryDat <- pS$data %>%
  as.data.frame %>%
  dplyr::filter(group == "Hoary")
hoaryDat %>%
  ggplot() +
  aes(predicted) +
  stat_ecdf()
thresh0.90 <- hoaryDat %>%
  dplyr::summarise(q90 = quantile(predicted, 0.90)) %>%
  unlist
hoaryDat %>%
  dplyr::filter(predicted >= thresh0.90) %>%
  dplyr::summarise(min = min(x), max = max(x))

LANODat <- pS$data %>%
  as.data.frame %>%
  dplyr::filter(group == "Silver-haired")
thresh0.90 <- LANODat %>%
  dplyr::summarise(q90 = quantile(predicted, 0.90)) %>%
  unlist
LANODat %>%
  dplyr::filter(predicted >= thresh0.90) %>%
  dplyr::summarise(min = min(x), max = max(x))




## northerly model ----
m_N1 <- glm(
  origin_is_northerly ~
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
  origin_is_northerly ~
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

# col_N <- "#e76f51"
# col_S <- "#023047"
# col_N <- "#1780a1"
# col_S <- "#a01a58"
col_N <- "#5f5096"
#col_S <- "#52B788"
col_S <- "#429f74"

df_N <- ggplot_build(pN)$plot$data %>% as.data.frame %>% mutate(mod = "N")
df_S <- ggplot_build(pS)$plot$data %>% as.data.frame %>% mutate(mod = "S")
df_wide <- rbind(df_N, df_S) %>%
  dplyr::rename(yDay = x, Species = group) %>%
  mutate(Species = factor(Species, levels = mySpecies))

rug_df <- mdf %>%
  dplyr::select(-c(x, Species)) %>%
  dplyr::rename(Species = commonName) %>%
  mutate(Species = factor(Species, levels = mySpecies))

# Crop plotted projection to within a certain window of any observations.
windowToPlot <- 14
## By species and direction:
rug_df %>%
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

### Additional calculations referenced in the ms: ----
pN$data %>%
  as.data.frame %>%
  group_by(group) %>%
  arrange(desc(predicted)) %>%
  slice(1)

pN$data %>% as.data.frame %>% dplyr::filter(group == "Eastern red", x == 1)


# At the peak detected-south-of-summering-grounds for hoary bats, what
# were model predictions for eastern reds?
peak_x <- df_S %>% dplyr::filter(group_col == "Hoary") %>% arrange(desc(predicted)) %>% slice(1) %>% dplyr::select(x) %>% unlist
dplyr::filter(df_S, group_col == "Eastern red", x == peak_x)
dplyr::filter(df_N, group_col == "Eastern red", x == peak_x)

# When was north-of-summering-grounds more probable than south-of-summering-grounds?
df_wide_filtered %>%
  dplyr::select(Species, yDay, predicted, mod) %>%
  pivot_wider(names_from = "mod", values_from = "predicted") %>%
  dplyr::filter(S > N) %>%
  nrow


# Plot overlapping species CI's -----
df_wide_filtered %>%
  ggplot() +
  geom_ribbon(
    aes(x=yDay, ymin = conf.low, ymax = conf.high, fill = Species, color = Species,
      group = interaction(Species, mod)) ,
    alpha =0.25, linetype = 1, size = 0.15
    ) +
  facet_grid(rows = vars(mod)) +
  scale_color_manual(
    "Species",
    breaks = mySpecies,
    values = c("#E09F3E", "#A43828", "#335C67")
    )+
  scale_fill_manual(
    "Species",
    breaks = mySpecies,
    values = c("#E09F3E", "#A43828", "#335C67")
    ) +
  geom_vline(xintercept = peak_x, linetype = 2, alpha = 0.5)




# Plot --------------------------------------------------------------------


myPlot <- ggplot() +
  # Plot CI's
  geom_ribbon(
    data = df_wide_filtered,
    aes(x=yDay, ymin = conf.low, ymax = conf.high, fill = mod, color = mod,
        group = interaction(Species, mod)),
    alpha =0.1, linetype = 1, size = 0.15
  ) +
  # Plot model predictions.
  geom_path(
    data = df_wide_filtered,
    aes(x=yDay, y = predicted, color = mod, group = interaction(Species, mod) ),
    linetype = 1, size = 1
  ) +
  # Plot rug plots
  geom_rug(
    data =  dplyr::filter(rug_df, origin_is_northerly == 1) ,
    aes(x=yDay),
    color = col_N,
    sides = "t"
  )  +
  geom_rug(
    data = dplyr::filter(rug_df, origin_is_southerly == 1) ,
    aes(x=yDay),
    color = col_S,
    sides = "b"
  )  +
  # Facet.
  facet_wrap(~Species, ncol = 1) +
  # Appearance
  scale_fill_manual(  breaks = c("N", "S"), values = c(col_N, col_S) ) +
  scale_color_manual( breaks = c("N", "S"), values = c(col_N, col_S) ) +
  scale_x_continuous(expand = c(0,0)) +
  xlab("Day of Year") +
  ylab("Probability of latitudinal movement") +
  theme(
    strip.background = element_rect(fill = "white"),
    legend.position = "none",
    panel.spacing = unit(2, "lines")
  )


north_touch_y <- pN$data %>% as.data.frame %>%
  dplyr::filter(x == 49, group == "Hoary") %>%
  {.$conf.high}
south_touch_y <- pS$data %>% as.data.frame %>%
  dplyr::filter(x == 120, group == "Hoary") %>%
  {.$conf.high}

label_df <-
  data.frame(
    label = c("South of summer origin", "North of summer origin"),
    Species = rep("Hoary", 2),
    x=c(100,00),
    y=c(0.9, 0.2),
    ax1 = c(110, 100),
    ax2 = c(49,120),
    ay1 = c(0.88,0.18),
    ay2 = c(north_touch_y, south_touch_y)
  ) %>%
  dplyr::mutate(
    Species = factor(Species, levels = mySpecies)
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
  width = 3, height = 7
  )




# New version ------------------------------------------------------------

plots <- lapply(mySpecies, function(spp) {

   lapply(c("S", "N"), function(dir){
    df1 <- dplyr::filter(df_wide_filtered, Species == spp, mod == dir)

    df2a <- dplyr::filter(rug_df, Species == spp)
    if(dir == "S") {
      df2 <- dplyr::filter(df2a, origin_is_southerly == 1)
      df3 <- dplyr::filter(df2a, origin_is_southerly == 0 )
      whichCol <- col_S
      ymx <- 0.5
    } else if (dir == "N") {
      df2 <- dplyr::filter(df2a, origin_is_northerly == 1)
      df3 <- dplyr::filter(df2a, origin_is_northerly == 0)
      whichCol <- col_N
      ymx <- 1
    }

    p <- ggplot() +
      geom_segment(aes(x = 0,   xend = 0,   y = 0, yend=Inf, color = "grey50") ) +
      geom_segment(aes(x = 365, xend = 365, y = 0, yend=Inf, color = "grey50") ) +

      # Plot CI's
      geom_ribbon(
        data = df1,
        aes(x=yDay, ymin = conf.low, ymax = conf.high, fill = mod, color = mod,
            group = interaction(Species, mod)),
        alpha =0.1, linetype = 1, size = 0.15
      ) +
      # Plot model predictions.
      geom_path(
        data = df1,
        aes(x=yDay, y = predicted, color = mod, group = interaction(Species, mod) ),
        linetype = 1, size = 1
      ) +
      scale_x_continuous(expand = c(0,0), limits = c(0,365), breaks = c(seq(0,365,by = 75), 365)) +
      scale_fill_manual(  breaks = c("N", "S"), values = c(col_N, col_S) ) +
      scale_color_manual( breaks = c("N", "S"), values = c(col_N, col_S) ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.position = "none",
        plot.margin = margin(10,20,0,0),
        plot.title = element_text(hjust = 0.5, vjust = 0)
      )

    segheight <- 0.12*ymx
    spacing <- 0.025 * ymx

    p <- p +
      scale_y_continuous(limits = c(-(spacing*2 + segheight*2), ymx),expand = c(0,0), breaks = seq(0,1, by = 0.2), labels = seq(0,1,by=0.2)) +
      geom_segment(df2, mapping = aes(x=yDay, xend = yDay, y= -(spacing), yend = -(spacing + segheight) ), color = whichCol, alpha = 0.75) +
      geom_segment(df3, mapping = aes(x=yDay, xend = yDay, y= -(spacing*2 + segheight), yend = -(spacing*2 + segheight*2) ), color = "grey50", alpha = 0.75)

    return(p)

  })

})

ps <- unlist(plots, recursive = F)

pp <- ggarrange(
  plotlist = list(
    ps[[5]] +  ggtitle("Silver-haired") ,
    ps[[1]] +  ggtitle("Hoary")       + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
    ps[[3]] +  ggtitle("Eastern red") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
    ps[[6]] ,
    ps[[2]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()),
    ps[[4]] + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  )
  , ncol = 3, nrow = 2) %>%
  arrangeGrob(bottom = "Day of Year", left = "Probability")
ggsave(pp, filename = file.path(wd$figs, "directionByDay.png"))
