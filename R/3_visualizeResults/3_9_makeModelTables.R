
# Setup and define functions ----------------------------------------------
library(gt)
library(gtExtras)
load("bin/modelFits.RData")

renameEffects <- function(df) {
  df$`Fixed Effect` <- df$`Fixed Effect` %>%
    gsub("OriginCluster", "Summer latitude", .) %>%
    gsub("decimalLatitude", "Sampling latitude", .) %>%
    gsub("polyyday22", "Day of year (start at 150)", .) %>%
    gsub("polyyDay2", "Day of year (start at 1)", .) %>%
    gsub("dirS", "Movement to higher latitude", .) %>%
    gsub("dirU", "Movement direction not determined", .)
  return(df)
}

myRename <- function(vect) {
  paste(gsub("df_","", attr(mod$data, "data_name")), vect, sep = "_")
}
makeModelTable <- function(mod) {
  out <- mod %>%
    summary(., robust = T) %>%
    {.$fixed} %>%
    dplyr::mutate("Fixed Effect" = row.names(.)) %>%
    dplyr::select(`Fixed Effect`, Estimate, contains("CI")) %>%
    renameEffects
  myprefix <- attr(mod$data, "data_name") %>%
    gsub("df_","", .) %>%
    gsub("_sex", "", .)
  names(out) <- paste(myprefix, names(out), sep = "_")
  return(out)
}

# List formula, family, links.

# 1. Sex ID model -----
tab_sex <- bind_cols(
  makeModelTable(m_sex_lano),
  makeModelTable(m_sex_laci),
  makeModelTable(m_sex_labo),
  .name_repair = c( "unique")
  ) %>%
  dplyr::select(1, everything(), -"laci_Fixed Effect",- "labo_Fixed Effect") %>%
  dplyr::rename(`Fixed Effect` = `lano_Fixed Effect`) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  gt_theme_538()

gt::gtsave(tab_sex, filename = "out/tables/tab_sex.docx")



# 2. Distance traveled model ------
## Initial comparative models across families.

loo_compare( loo_lano_dist_hurdlegamma, loo_lano_dist_hurdlelognorm)
loo_compare( loo_laci_dist_hurdlegamma, loo_laci_dist_hurdlelognorm)
loo_compare( loo_labo_dist_hurdlegamma, loo_labo_dist_hurdlelognorm)

# Report selected model results .
tab_distance <- bind_cols(
  makeModelTable(m_dist_lano),
  makeModelTable(m_dist_laci),
  makeModelTable(m_dist_labo),
  .name_repair = c( "unique")
) %>%
  dplyr::select(1, everything(), -"laci_Fixed Effect",- "labo_Fixed Effect") %>%
  dplyr::rename(`Fixed Effect` = `lano_Fixed Effect`) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), n_sigfig = 2) %>%
  gt_theme_538()
gt::gtsave(tab_distance, "out/tables/tab_distance.docx")



# 3. Direction model ------------------------------------------------------
## Direction timing models ----
dir_n_results <- bind_cols(
  makeModelTable(m_dir_LANO_N),
  makeModelTable(m_dir_LACI_N),
  makeModelTable(m_dir_LABO_N),
  .name_repair = c( "unique")
) %>%
  setNames(paste0('N_', names(.)))
dir_s_results <- bind_cols(
  makeModelTable(m_dir_LANO_S),
  makeModelTable(m_dir_LACI_S),
  makeModelTable(m_dir_LABO_S),
  .name_repair = c( "unique")
) %>%
  setNames(paste0('S_', names(.)))

tab_dir_timing <- bind_cols(dir_n_results, dir_s_results) %>%
  dplyr::select(1, everything(),  "S_lano_Fixed Effect",-contains("laci_Fixed Effect"),-contains("labo_Fixed Effect")) %>%
  dplyr::rename(`Fixed Effect` = `N_lano_Fixed Effect`) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), n_sigfig = 2) %>%
  gt_theme_538()

gt::gtsave(tab_dir_timing, "out/tables/tab_dir_timing.docx")


## Direction / sex model ----

dir_n_results_sex <- bind_cols(
  makeModelTable(m_dir_LANO_N_sex),
  makeModelTable(m_dir_LACI_N_sex),
  makeModelTable(m_dir_LABO_N_sex),
  .name_repair = c( "unique")
) %>%
  setNames(paste0('N_', names(.)))
dir_s_results_sex <- bind_cols(
  makeModelTable(m_dir_LANO_S_sex),
  makeModelTable(m_dir_LACI_S_sex),
  makeModelTable(m_dir_LABO_S_sex),
  .name_repair = c( "unique")
) %>%
  setNames(paste0('S_', names(.)))

tab_dir_sex <- bind_cols(dir_n_results_sex, dir_s_results_sex) %>%
  dplyr::select(1, everything(),  "S_lano_Fixed Effect",-contains("laci_Fixed Effect"),-contains("labo_Fixed Effect")) %>%
  dplyr::rename(`Fixed Effect` = `N_lano_Fixed Effect`) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), n_sigfig = 2) %>%
  gt_theme_538()
gt::gtsave(tab_dir_sex, "out/tables/tab_dir_sex.docx")



# 4. Wind models. ------

## Main wind model (direction param) ----
tab_wind <- bind_cols(
  makeModelTable(m_wind_distDir_lano_cat_dir),
  makeModelTable(m_wind_distDir_laci_cat_dir),
  makeModelTable(m_wind_distDir_labo_cat_dir),
  .name_repair = c( "unique")
) %>%
  dplyr::select(1, everything(), -"laci_Fixed Effect",- "labo_Fixed Effect") %>%
  dplyr::rename(`Fixed Effect` = `lano_Fixed Effect`) %>%
  dplyr::mutate(
    `Fixed Effect` = case_when(
      `Fixed Effect` == "Intercept" ~ "Intercept (Movement to lower latitude)",
      TRUE ~`Fixed Effect`
    )
  ) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), decimals = 2) %>%
  gt_theme_538()
gt::gtsave(tab_wind, "out/tables/table_wind_sig.docx")
tab_wind %>%
  fmt_scientific(columns = where(is.numeric), n_sigfig = 3) %>%
  gt::gtsave("out/tables/table_wind_sci.docx")


# Get 50% CIs:
posterior_summary(m_wind_distDir_lano_cat_dir, probs = c(.25, .95)) %>% signif(2)
posterior_summary(m_wind_distDir_laci_cat_dir, probs = c(.25, .95)) %>% signif(2)
posterior_summary(m_wind_distDir_labo_cat_dir, probs = c(.25, .95)) %>% signif(2)


## Alt wind model (direction continuous, includes distance) -----

tab_wind_alt <- bind_cols(
  makeModelTable(m_wind_distDir_lano_cat_dir_cont),
  makeModelTable(m_wind_distDir_laci_cat_dir_cont),
  makeModelTable(m_wind_distDir_labo_cat_dir_cont),
  .name_repair = c( "unique")
) %>%
  dplyr::select(1, everything(), -"laci_Fixed Effect",- "labo_Fixed Effect") %>%
  dplyr::rename(`Fixed Effect` = `lano_Fixed Effect`) %>%
  gt %>%
  tab_spanner_delim(delim = "_") %>%
  fmt_number(columns = where(is.numeric), n_sigfig = 3) %>%
  gt_theme_538()
gt::gtsave(tab_wind_alt, "out/tables/table_wind_alternateModel_sig.docx")
tab_wind_alt %>%
  fmt_scientific(columns = where(is.numeric), n_sigfig = 3) %>%
  gt::gtsave("out/tables/table_wind_alternateModel_sci.docx")



# Big list of models ------------------------------------------------------

# Sex
m_sex_lano$formula
m_sex_lano$family

# Distance
m_dist_lano$formula
m_dist_lano$family

# Direction timinig
m_dir_LANO_N$formula
m_dir_LANO_S$formula
m_dir_LANO_N$family

# Direction x sex
m_dir_LANO_N_sex$formula
m_dir_LANO_S_sex$formula
m_dir_LANO_S_sex$family

# Wind -- discrete direction only
m_wind_distDir_lano_cat_dir$formula
m_wind_distDir_lano_cat_dir$family

# Wind -- continous distance and direction model
m_wind_distDir_lano_cat_dir_cont$formula
m_wind_distDir_lano_cat_dir_cont$family



