# 0. Setup ------
source("R/0_setup.R")
library(splines)
library(brms)
library(bayesplot)

df <- readRDS(file.path(wd$bin, "mydata_minDistDir.rds"))

df <- df %>%
  dplyr::mutate(
    dir_S = case_when(dir == "S" ~ 1, TRUE ~ 0),
    dir_N = case_when(dir == "N" ~ 1, TRUE ~ 0),
    Sex = factor(Sex, levels = c( "M", "F")),
    # Distances traveled that are arbitrarily close to 0, move to 0.
    dist_km_adjusted = case_when(dist_km < 50 ~ 0, TRUE ~ dist_km),
    # Fix bug where 0's missing from 'N' and 'S' columns
    N = case_when(is.na(N) & S ==1 ~ 0, TRUE ~ N),
    S = case_when(is.na(S) & N ==1 ~ 0, TRUE ~ S),
    # Make factor of whether individual is known juvenile.
    isJ = factor(case_when(Age == "J" ~ 1, TRUE ~ 0))
  )
df_lano <- dplyr::filter(df, commonName == "Silver-haired", !is.na(yday2))
df_laci <- dplyr::filter(df, commonName == "Hoary", !is.na(yday2))
df_labo <- dplyr::filter(df, commonName == "Eastern red", !is.na(yday2))
set.seed(42)

# 1. Sex ID model -------

df %>% count(commonName, wind_killed, Sex)

df %>%
  count(commonName, wind_killed, Sex) %>%
  ggplot() +
  geom_col(aes(x=wind_killed, y = n, fill = Sex), position = "stack") +
  facet_wrap(~commonName)

## LACI ----
# Filtered to reliable data only.
df_laci_sex <- dplyr::filter(df_laci, !is.na(Sex), wind_killed == "no")
m_sex_laci <- brm(
  formula =
    Sex ~
    OriginCluster + # summer latitude
    decimalLatitude*poly(yday2,2) , # sampling latitude * day of year (start at day 150)
  data = df_laci_sex,
  family = bernoulli(),
  cores = 8,
  seed = 42,
  iter = 1000,
  warmup = 500,
  threads = threading(8),
  backend = "cmdstanr"
)
summary(m_sex_laci)
conditional_effects(m_sex_laci, effects = "yday2", conditions = data.frame(decimalLatitude = c(20, 30, 40, 50)))
conditional_effects(m_sex_laci, effects = "OriginCluster")

pp_check(m_sex_laci)
pp_check(m_sex_laci, type = "stat", stat = "mean")

## LANO ----
df_lano_sex <- dplyr::filter(df_lano, !is.na(Sex), wind_killed == "no")
m_sex_lano <- update(m_sex_laci, newdata = df_lano_sex)
summary(m_sex_lano)
conditional_effects(m_sex_lano) %>% plot(ask = F)
conditional_effects(m_sex_lano, effects = "yday2", conditions = data.frame(decimalLatitude = c(20, 30, 40, 50)))
pp_check(m_sex_lano)
pp_check(m_sex_lano, type = "stat", stat = "mean")

## LABO ----
df_labo_sex <- dplyr::filter(df_labo, !is.na(Sex), wind_killed == "no")
m_sex_labo <- update(m_sex_laci,  newdata = df_labo_sex )
summary(m_sex_labo)
conditional_effects(m_sex_labo) %>% plot(ask = F)
conditional_effects(m_sex_labo, effects = "yday2", conditions = data.frame(decimalLatitude = c(20, 30, 40, 50)))

pp_check(m_sex_labo)
pp_check(m_sex_labo, type = "stat", stat = "mean")


# 2. Distance traveled model ------
## Initial comparative models across families.

### LACI -----

m_laci_dist_hurdlegamma <-
  update(
    m_sex_laci,
    formula = dist_km_adjusted ~
      poly(yday2, 2) +
      OriginCluster +
      decimalLatitude
    ,
    newdata = df_laci,
    family = hurdle_gamma(),
  )
m_laci_dist_hurdlelognorm <-
  update( m_laci_dist_hurdlegamma,family = hurdle_lognormal() )

loo_laci_dist_hurdlegamma   <- loo(m_laci_dist_hurdlegamma)
loo_laci_dist_hurdlelognorm <- loo(m_laci_dist_hurdlelognorm)

loo_compare( loo_laci_dist_hurdlegamma, loo_laci_dist_hurdlelognorm)

conditional_effects(m_laci_dist_hurdlegamma) %>% plot(ask = F)
conditional_effects(m_laci_dist_hurdlegamma,
                    effects = "yday2",
                    conditions = data.frame(OriginCluster = 1:4)
) %>% plot(ask = F)

### LANO -----

m_lano_dist_hurdlegamma   <- update(m_laci_dist_hurdlegamma, newdata = df_lano)
m_lano_dist_hurdlelognorm <- update(m_laci_dist_hurdlelognorm, newdata = df_lano)

loo_lano_dist_hurdlegamma   <- loo(m_lano_dist_hurdlegamma, moment_match = T)
loo_lano_dist_hurdlelognorm <- loo(m_lano_dist_hurdlelognorm)

loo_compare(loo_lano_dist_hurdlegamma, loo_lano_dist_hurdlelognorm )

### LABO -----

m_labo_dist_hurdlegamma   <- update(m_laci_dist_hurdlegamma, newdata = df_labo)
m_labo_dist_hurdlelognorm <- update(m_laci_dist_hurdlelognorm, newdata = df_labo)

loo_labo_dist_hurdlegamma   <- loo(m_labo_dist_hurdlegamma)
loo_labo_dist_hurdlelognorm <- loo(m_labo_dist_hurdlelognorm)

loo_compare(loo_labo_dist_hurdlegamma, loo_labo_dist_hurdlelognorm )


#### Bring in informative priors for LABO -----
# Because of weaker temporal sampling coverage for this species, also bring in informative priors from the laci models for day of year.
mLaci_mod_sum1 <- summary(m_laci_dist_hurdlegamma)$fixed
mLaci_mod_sum1 <- mLaci_mod_sum1[grep("polyyday", rownames(mLaci_mod_sum1)), 1:2]

m_labo_dist_hurdlegamma2   <- update(
  m_labo_dist_hurdlegamma,
  prior = c(
    prior_string(paste0("normal(", mLaci_mod_sum1[1,1], ", ", mLaci_mod_sum1[1,2]/2, ")"), coef = rownames(mLaci_mod_sum1)[1]),
    prior_string(paste0("normal(", mLaci_mod_sum1[2,1], ", ", mLaci_mod_sum1[2,2]/2, ")"), coef = rownames(mLaci_mod_sum1)[2])
  ))


loo_labo_dist_hurdlegamma2   <- loo(m_labo_dist_hurdlegamma2)

loo_compare( loo_labo_dist_hurdlegamma, loo_labo_dist_hurdlegamma2 )

conditional_effects(
  m_labo_dist_hurdlegamma2,
  effects = c("yday2", "OriginCluster")) %>% plot(ask = F)

### Select final models -------

m_dist_laci <- m_laci_dist_hurdlegamma
m_dist_lano <- m_lano_dist_hurdlegamma
m_dist_labo <- m_labo_dist_hurdlegamma2

pp_check(m_dist_laci)
pp_check(m_dist_laci, type = "stat", stat = "mean")
pp_check(m_dist_lano)
pp_check(m_dist_lano, type = "stat", stat = "mean") # not the best.
pp_check(m_dist_labo)
pp_check(m_dist_labo, type = "stat", stat = "mean")


# 3. Direction model ------

## LACI  -----
### Southern origin model ------
m_dir_LACI_S <- update( m_dist_laci,
                        formula =
                          dir_S ~ poly(yDay,2) ,
                        newdata = df_laci,
                        family = bernoulli(),
)
m_dir_LACI_S
summary(m_dir_LACI_S)
pp_check(m_dir_LACI_S)
pp_check(m_dir_LACI_S, type = "stat", stat = "mean")

### Northern origin model -----
m_dir_LACI_N <- update(
  m_dir_LACI_S,
  formula = dir_N ~ poly(yDay,2) ,
  newdata = df_laci
)
m_dir_LACI_N
pp_check(m_dir_LACI_N)
pp_check(m_dir_LACI_N, type = "stat", stat = "mean")


## LABO ----
# Add informative priors from LACI model for day of year.
prs <- summary(m_dir_LACI_S)$fixed
prs <- prs[grep("polyyDay2", rownames(prs)), ]

m_dir_LABO_S <- update(
  m_dir_LACI_S,
  newdata = df_labo,
  prior = c(
    prior_string(paste0("normal(", prs[1,1], ", ", prs[1,2], ")"), coef = rownames(prs)[1]),
    prior_string(paste0("normal(", prs[2,1], ", ", prs[2,2], ")"), coef = rownames(prs)[2])
  )
)

m_dir_LABO_S

prs2 <- summary(m_dir_LACI_N)$fixed
prs2 <- prs2[grep("polyyDay2", rownames(prs2)), ]

m_dir_LABO_N <- update(
  m_dir_LACI_N,
  newdata = df_labo,
  prior = c(
    prior_string(paste0("normal(", prs2[1,1],",",  prs2[1,2], ")"), coef = rownames(prs2)[1]),
    prior_string(paste0("normal(", prs2[2,1],",",  prs2[2,2], ")"), coef = rownames(prs2)[2])
  )
)
m_dir_LABO_N
summary(m_dir_LABO_N)$fixed
pp_check(m_dir_LABO_S)
pp_check(m_dir_LABO_S, type = "stat", stat = "mean")

pp_check(m_dir_LABO_N)
pp_check(m_dir_LABO_N, type = "stat", stat = "mean")


## LANO ------
m_dir_LANO_S <- update( m_dir_LACI_S, newdata = df_lano)
m_dir_LANO_S
m_dir_LANO_N <- update( m_dir_LACI_N, newdata = df_lano)
m_dir_LANO_N

pp_check(m_dir_LANO_S)
pp_check(m_dir_LANO_S, type = "stat", stat = "mean")

pp_check(m_dir_LANO_N)
pp_check(m_dir_LANO_N, type = "stat", stat = "mean")


## 3b. Direction x Sex -------

df %>%
  count(commonName, wind_killed, Sex) %>%
  ggplot() +
  geom_col(aes(x=wind_killed, y = n, fill = Sex), position = "stack") +
  facet_wrap(~commonName)


m_dir_LACI_S_sex <- update( m_dir_LACI_S,
                            formula =
                              dir_S ~ poly(yDay,2) + Sex ,
                            newdata = df_laci_sex,
                            family = bernoulli(),
)
conditional_effects(m_dir_LACI_S_sex) %>% plot(ask = F)
pp_check(m_dir_LACI_S_sex)
pp_check(m_dir_LACI_S_sex, type = "stat", stat = "mean")

m_dir_LANO_S_sex <- update( m_dir_LACI_S_sex, newdata = df_lano_sex)
conditional_effects(m_dir_LANO_S_sex) %>% plot(ask = F)
pp_check(m_dir_LANO_S_sex)
pp_check(m_dir_LANO_S_sex, type = "stat", stat = "mean")

# Add priors
prs_dirsex <- summary(m_dir_LACI_S_sex)$fixed
prs_dirsex <- prs_dirsex[grep("polyyDay2", rownames(prs_dirsex)), ]

m_dir_LABO_S_sex <- update(
  m_dir_LACI_S_sex,
  newdata = df_labo_sex,
  prior = c(
    prior_string(paste0("normal(", prs_dirsex[1,1],",",  prs_dirsex[1,2], ")"), coef = rownames(prs_dirsex)[1]),
    prior_string(paste0("normal(", prs_dirsex[2,1],",",  prs_dirsex[2,2], ")"), coef = rownames(prs_dirsex)[2])
  ))
conditional_effects(m_dir_LABO_S_sex) %>% plot(ask = F)
pp_check(m_dir_LABO_S_sex)
pp_check(m_dir_LABO_S_sex, type = "stat", stat = "mean")


m_dir_LACI_N_sex <- update( m_dir_LACI_S,
                            formula = dir_S ~ poly(yDay,2) + Sex,
                            newdata = df_laci_sex
)
conditional_effects(m_dir_LACI_N_sex) %>% plot(ask = F)
pp_check(m_dir_LACI_N_sex)
pp_check(m_dir_LACI_N_sex, type = "stat", stat = "mean")


m_dir_LANO_N_sex <- update( m_dir_LACI_N_sex, newdata = df_lano_sex)
conditional_effects(m_dir_LANO_N_sex) %>% plot(ask = F)
pp_check(m_dir_LANO_N_sex)
pp_check(m_dir_LANO_N_sex, type = "stat", stat = "mean")


prs_dirsex2 <- summary(m_dir_LACI_N_sex)$fixed
prs_dirsex2 <- prs_dirsex[grep("polyyDay2", rownames(prs_dirsex2)), ]

m_dir_LABO_N_sex <- update(
  m_dir_LACI_N_sex,
  newdata = df_labo_sex,
  prior = c(
    prior_string(paste0("normal(", prs_dirsex2[1,1],",",  prs_dirsex2[1,2], ")"), coef = rownames(prs_dirsex2)[1]),
    prior_string(paste0("normal(", prs_dirsex2[2,1],",",  prs_dirsex2[2,2], ")"), coef = rownames(prs_dirsex2)[2])
  ))
conditional_effects(m_dir_LABO_N_sex) %>% plot(ask = F)
pp_check(m_dir_LABO_N_sex)
pp_check(m_dir_LABO_N_sex, type = "stat", stat = "mean")

## 3c. Direction x Age exploration ------
df %>% dplyr::mutate(hasAge = !is.na(Age)) %>% count(hasAge)

df %>%
  count(commonName, dir, Age) %>%
  ggplot() +
  geom_col(aes(x=dir, y = n, fill = Age), position = "stack") +
  facet_wrap(~commonName)

juvenileDateQuantiles <- df %>%
  group_by(commonName) %>%
  dplyr::filter(Age == "J") %>%
  dplyr::summarise(
    q025 = quantile(yday2, 0.025, na.rm = T),
    q975 = quantile(yday2, 0.975, na.rm = T),
    q25 = quantile(yday2, 0.25, na.rm = T),
    q75 = quantile(yday2, 0.75, na.rm = T),
  )

df_juv_exploration <- df %>%
  dplyr::filter(!is.na(Age)) %>%
  left_join(., juvenileDateQuantiles) %>%
  dplyr::filter(yday2 >= q25 & yday2 <= q75)

df_juv_exploration %>%
  count(commonName, dir, Age, yDay) %>%
  ggplot() +
  geom_col(aes(x=commonName, y = n, fill = Age), position = "stack")

df_juv_exploration %>%
  count(commonName, Age) %>%
  dplyr::mutate(prop = signif(n/sum(n)*100, 2), .by = commonName)

df %>%
  dplyr::filter(!is.na(Age)) %>%
  left_join(., juvenileDateQuantiles) %>%
  dplyr::filter(yday2 >= q025 & yday2 <= q975) %>%
  count(commonName, Age) %>%
  dplyr::mutate(prop = signif(n/sum(n)*100, 2), .by = commonName)

ggplot(df) +
  geom_histogram(aes(x = yday2, group = isJ, fill = isJ)) +
  facet_wrap(dir~commonName)

# 4. Wind model ------
## Primary wind model with categorical definition for direction -----

df %>%
  ggplot() +
  geom_bar(aes(x = wind_killed,  fill = dir )) +
  facet_wrap(~commonName)

df %>%
  ggplot() +
  geom_boxplot(aes(y = wind_killed,  x = dist_km_adjusted )) +
  facet_wrap(~commonName) +
  scale_x_log10()

### LACI -----
m_wind_distDir_laci_cat_dir <- update(
  m_dir_LACI_S,
  formula =
    wind_killed ~
    dir +
    dist_km +
    OriginCluster,
  newdata = df_laci
)
m_wind_distDir_laci_cat_dir
pp_check(m_wind_distDir_laci_cat_dir)
conditional_effects(m_wind_distDir_laci_cat_dir)
conditional_effects(m_wind_distDir_laci_cat_dir, effects = "dir")
conditional_effects(m_wind_distDir_laci_cat_dir, effects = "dist_km")

### LABO -----
m_wind_distDir_labo_cat_dir <- update(
  m_wind_distDir_laci_cat_dir,
  newdata = df_labo
)
m_wind_distDir_labo_cat_dir
pp_check(m_wind_distDir_labo_cat_dir)
conditional_effects(m_wind_distDir_labo_cat_dir)

### LANO -----
m_wind_distDir_lano_cat_dir <- update(
  m_wind_distDir_laci_cat_dir,
  newdata = df_lano
)
m_wind_distDir_lano_cat_dir
pp_check(m_wind_distDir_lano_cat_dir)
conditional_effects(m_wind_distDir_lano_cat_dir)

## Alternate model -------
## Continuous variables for direction instead.

m_wind_distDir_laci_cat_dir_cont <- update(
  m_wind_distDir_laci_cat_dir,
  formula =
    wind_killed ~
    S +
    dist_km +
    OriginCluster,
  newdata = df_laci
)
m_wind_distDir_laci_cat_dir_cont
pp_check(m_wind_distDir_laci_cat_dir_cont)
conditional_effects(m_wind_distDir_laci_cat_dir_cont)


m_wind_distDir_labo_cat_dir_cont <- update(
  m_wind_distDir_laci_cat_dir_cont,
  newdata = df_labo
)
m_wind_distDir_labo_cat_dir_cont
pp_check(m_wind_distDir_labo_cat_dir_cont)
conditional_effects(m_wind_distDir_labo_cat_dir_cont)


m_wind_distDir_lano_cat_dir_cont <- update(
  m_wind_distDir_laci_cat_dir_cont,
  newdata = df_lano
)
m_wind_distDir_lano_cat_dir_cont
pp_check(m_wind_distDir_lano_cat_dir_cont)
conditional_effects(m_wind_distDir_lano_cat_dir_cont)


## Age exploration ----
# Are juveniles more likely to be killed @ turbines, accounting for doy effects?
# Doy and turbine date are so correlated, hard to disentangle.

df %>% count(commonName, wind_killed, Age)

df %>%
  count(commonName, wind_killed, Age) %>%
  ggplot() +
  geom_col(aes(x=wind_killed, y = n, fill = Age), position = "stack") +
  facet_wrap(~commonName)

df_juvenile <- df %>%
  dplyr::filter(!is.na(Age)) %>%
  dplyr::mutate(isJ = factor(case_when(Age == "J" ~ 1, TRUE ~ 0)))

ggplot(df_juvenile) +
  geom_histogram(aes(x = yday2, group = isJ, fill = isJ))

ggplot(df_juvenile) +
  geom_histogram(aes(x = yday2, group = isJ, fill = isJ)) +
  facet_wrap(~commonName)


## Sex exploration -----

df %>%
  dplyr::filter(wind_killed == "no", !is.na(Sex)) %>%
  count(commonName, wind_killed, Sex)

df %>%
  dplyr::filter(!is.na(Sex)) %>%
  ggplot() +
  geom_bar(aes(fill = Sex, x = wind_killed))


# Save ----
save.image(file = paste0("bin/modelFits_", Sys.Date(),".RData"))
