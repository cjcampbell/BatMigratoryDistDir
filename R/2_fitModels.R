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
    S = case_when(is.na(S) & N ==1 ~ 0, TRUE ~ S)
  )
df_lano <- dplyr::filter(df, commonName == "Silver-haired", !is.na(yday2))
df_laci <- dplyr::filter(df, commonName == "Hoary", !is.na(yday2))
df_labo <- dplyr::filter(df, commonName == "Eastern red", !is.na(yday2))

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
    OriginCluster +
    decimalLatitude*poly(yday2,2) ,
  data = df_laci_sex,
  family = bernoulli(),
  cores = 8,
  seed = 42,
  iter = 1000,
  warmup = 500,
  threads = threading(8),
  backend = "cmdstanr"
)
conditional_effects(m_sex_laci, effects = "yday2", conditions = data.frame(decimalLatitude = c(20, 30, 40, 50)))
conditional_effects(m_sex_laci, effects = "OriginCluster")

pp_check(m_sex_laci)
pp_check(m_sex_laci, type = "stat", stat = "mean")

## LANO ----
df_lano_sex <- dplyr::filter(df_lano, !is.na(Sex), wind_killed == "no")
m_sex_lano <- update(m_sex_laci, newdata = df_lano_sex)
conditional_effects(m_sex_lano) %>% plot(ask = F)
conditional_effects(m_sex_lano, effects = "yday2", conditions = data.frame(decimalLatitude = c(20, 30, 40, 50)))

pp_check(m_sex_lano)
pp_check(m_sex_lano, type = "stat", stat = "mean")

## LABO ----
df_labo_sex <- dplyr::filter(df_labo, !is.na(Sex), wind_killed == "no")
m_sex_labo <- update(m_sex_laci,  newdata = df_labo_sex )
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

loo_lano_dist_hurdlegamma   <- loo(m_lano_dist_hurdlegamma)
loo_lano_dist_hurdlelognorm <- loo(m_lano_dist_hurdlelognorm)

loo_compare(loo_lano_dist_hurdlegamma, loo_lano_dist_hurdlelognorm )

### LABO -----

m_labo_dist_hurdlegamma   <- update(m_laci_dist_hurdlegamma, newdata = df_labo)
m_labo_dist_hurdlelognorm <- update(m_laci_dist_hurdlelognorm, newdata = df_labo)

loo_labo_dist_hurdlegamma   <- loo(m_labo_dist_hurdlegamma)
loo_labo_dist_hurdlelognorm <- loo(m_labo_dist_hurdlelognorm)

loo_compare(loo_labo_dist_hurdlegamma, loo_labo_dist_hurdlelognorm )


#### Bring in informative priors for LABO -----
# Because of weaker temporal sampling coverage for this species, also bring in informative priors from the laci models.
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
# Note that there's still something interesting in LACI model containing interaction term,
# but not enough to justify retaining for all species. Drop for now.

m_dist_laci <- m_laci_dist_hurdlegamma
m_dist_lano <- m_lano_dist_hurdlegamma
m_dist_labo <- m_labo_dist_hurdlegamma2

pp_check(m_dist_laci)
pp_check(m_dist_laci, type = "stat", stat = "mean")
pp_check(m_dist_lano)
pp_check(m_dist_lano, type = "stat", stat = "mean")
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
# Add informative priors from LACI model.
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

m_dir_LABO_N0 <- update(
  m_dir_LACI_N,
  newdata = df_labo
)
summary(m_dir_LABO_N0)$fixed
m_dir_LABO_N <- update(
  m_dir_LACI_N,
  newdata = df_labo,
  prior = c(
    prior_string(paste0("normal(", prs2[1,1],",",  prs2[1,2], ")"), coef = rownames(prs2)[1]),
    prior_string(paste0("normal(", prs2[2,1],",",  prs2[2,2], ")"), coef = rownames(prs2)[2])
  )
)
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


## Direction x Sex -------

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

# TODO Add priors
m_dir_LABO_S_sex <- update( m_dir_LACI_S_sex, newdata = df_labo_sex)
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

m_dir_LABO_N_sex <- update( m_dir_LACI_N_sex, newdata = df_labo_sex)
conditional_effects(m_dir_LABO_N_sex) %>% plot(ask = F)
pp_check(m_dir_LABO_N_sex)
pp_check(m_dir_LABO_N_sex, type = "stat", stat = "mean")

## Direction x Age ------
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


# 4. Wind model ------
## With categorical direction -----
m_wind_laci_1 <- update(
  m_sex_laci,
  formula =
    wind_killed ~
    dir +
    OriginCluster +
    poly(yDay, 2),
  newdata = df_laci,
  prior = c(
    prior(normal(0,2), class = b)
  )
)
get_prior(m_wind_laci_1)
m_wind_laci_1
pp_check(m_wind_laci_1)
pp_check(m_wind_laci_1, type = "stat", stat = "mean")


### LABO -----
# With priors for date:
prs3 <- summary(m_wind_laci_1)$fixed
prs3 <- prs3[grep("polyyDay2", rownames(prs3)), ]

m_wind_labo_1 <- update(
  m_wind_laci_1,
  prior = c(
    prior(normal(0,2), class = b),
    prior_string(paste0("normal(", prs3[1,1], ", ", prs3[1,2], ")"), coef = rownames(prs3)[1]),
    prior_string(paste0("normal(", prs3[2,1], ", ", prs3[2,2], ")"), coef = rownames(prs3)[2])
  ),
  newdata = df_labo
)

pp_check(m_wind_labo_1)
pp_check(m_wind_labo_1, type = "stat", stat = "mean")


### LANO -----
m_wind_lano_1 <- update(
  m_wind_labo_1,
  newdata = df_lano
)
pp_check(m_wind_lano_1)
pp_check(m_wind_lano_1, type = "stat", stat = "mean")



## With distance and direction as continuous predictors ----
# Exploratory models with different parameters (dir is specified using N, S, and dist_km)
### laci -----
m_wind_dist_laci <- update(
  m_wind_laci_1,
  formula =
    wind_killed ~
    poly(N,2) +
    poly(S,2) +
    dist_km +
    OriginCluster +
    poly(yDay, 2),
  newdata = df_laci,
)
conditional_effects(m_wind_dist_laci) %>% plot(ask = F)

pp_check(m_wind_dist_laci)
pp_check(m_wind_dist_laci, type = "stat", stat = "mean")


### labo ----
# Include priors for day of year from laci model
mm <- summary(m_wind_dist_laci)$fixed
mm <- mm[grep("polyyDay", rownames(mm)), 1:2]

m_wind_dist_labo <- update(
  m_wind_dist_laci,
  newdata = df_labo,
  prior = c(
    prior_string(paste0("normal(", mm[1,1], ", ", mm[1,2], ")"), coef = rownames(mm)[1]),
    prior_string(paste0("normal(", mm[2,1], ", ", mm[2,2], ")"), coef = rownames(mm)[2])
  )
)
conditional_effects(m_wind_dist_labo) %>% plot(ask = F)
pp_check(m_wind_dist_labo)
pp_check(m_wind_dist_labo, type = "stat", stat = "mean")


### lano ----
m_wind_dist_lano <- update( m_wind_dist_laci, newdata = df_lano)
conditional_effects(m_wind_dist_lano) %>% plot(ask = F)

pp_check(m_wind_dist_lano)
pp_check(m_wind_dist_lano, type = "stat", stat = "mean")


## With distance and direction -------
### laci ---
m_wind_distDir_laci <- update(
  m_wind_laci_1,
  formula =
    wind_killed ~
    dir +
    dist_km +
    OriginCluster +
    poly(yDay, 2),
  newdata = df_laci
)
conditional_effects(m_wind_distDir_laci) %>% plot(ask = F)

pp_check(m_wind_distDir_laci)
pp_check(m_wind_distDir_laci, type = "stat", stat = "mean")

### lano -----
m_wind_distDir_lano <- update(m_wind_distDir_laci, newdata = df_lano)
conditional_effects(m_wind_distDir_lano) %>% plot(ask = F)

pp_check(m_wind_distDir_lano)
pp_check(m_wind_distDir_lano, type = "stat", stat = "mean")

### labo ----
mm3 <- summary(m_wind_distDir_laci)$fixed
mm3 <- mm[grep("polyyDay", rownames(mm)), 1:2]

m_wind_distDir_labo <- update(
  m_wind_distDir_laci,
  newdata = df_labo,
  prior = c(
    prior_string(paste0("normal(", mm3[1,1], ", ", mm3[1,2], ")"), coef = rownames(mm3)[1]),
    prior_string(paste0("normal(", mm3[2,1], ", ", mm3[2,2], ")"), coef = rownames(mm3)[2])
  )
)
conditional_effects(m_wind_distDir_labo)
pp_check(m_wind_distDir_labo)
pp_check(m_wind_distDir_labo, type = "stat", stat = "mean")


## Age model ----
# Are juveniles more likely to be killed @ turbines, accounting for doy effects?

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

# Break out by species.
df_juvenile_laci <- dplyr::filter(df_juvenile, commonName == "Hoary")
m_wind_isJ_laci <- update(
  m_wind_laci_1,
  formula = wind_killed ~ isJ +ns(yDay, df = 4),
  newdata = df_juvenile_laci)
conditional_effects(m_wind_isJ_laci, effect = "isJ") %>% plot(ask = F)
pp_check(m_wind_isJ_laci)
pp_check(m_wind_isJ_laci, type = "stat", stat = "mean")


df_juvenile_lano <- dplyr::filter(df_juvenile, commonName == "Silver-haired")
m_wind_isJ_lano <- update(m_wind_isJ_laci, newdata = df_juvenile_lano)
conditional_effects(m_wind_isJ_lano, effect = "isJ") %>% plot(ask = F)
pp_check(m_wind_isJ_lano)
pp_check(m_wind_isJ_lano, type = "stat", stat = "mean")

df_juvenile_labo <- dplyr::filter(df_juvenile, commonName == "Eastern red")
m_wind_isJ_labo <- update(m_wind_isJ_laci, newdata = df_juvenile_labo)
conditional_effects(m_wind_isJ_labo, effect = "isJ") %>% plot(ask = F)
pp_check(m_wind_isJ_labo)
pp_check(m_wind_isJ_labo, type = "stat", stat = "mean")

## Sex model -----

m_wind_sex_LACI <- update(
  m_wind_laci_1,
  formula = wind_killed ~ Sex + OriginCluster,
  newdata = df_laci_sex,
  adapt_delta = 0.95
)
conditional_effects(m_wind_sex_LACI) %>% plot(ask = F)
pp_check(m_wind_sex_LACI)
pp_check(m_wind_sex_LACI, type = "stat", stat = "mean")


m_wind_sex_LANO <- update(
  m_wind_laci_1,
  formula = wind_killed ~ Sex + OriginCluster,
  newdata = df_lano_sex,
  adapt_delta = 0.9
)
conditional_effects(m_wind_sex_LANO) %>% plot(ask = F)
pp_check(m_wind_sex_LANO)
pp_check(m_wind_sex_LANO, type = "stat", stat = "mean")

m_wind_sex_LABO <- update(
  m_wind_laci_1,
  formula = wind_killed ~ Sex + OriginCluster,
  newdata = df_labo_sex,
  adapt_delta = 0.9
)
conditional_effects(m_wind_sex_LABO) %>% plot(ask = F)
pp_check(m_wind_sex_LABO)
pp_check(m_wind_sex_LABO, type = "stat", stat = "mean")

# Save ----
save.image(file = paste0("bin/modelFits_", Sys.Date(),".RData"))
