selectedTransferFunctions <- readRDS(file.path( wd$bin, "sma_selected.rds"))
resids <- readRDS(file.path(wd$bin, "residual_sumtab.rds"))

selectedTransferFunctions %>%
  dplyr::select(Species, isoscape, from, to, mean_intercept, mean_slope, mean_r2) %>%
  dplyr::rename_at(vars(starts_with("mean_")), ~gsub("mean_", "", .x)) %>%
  full_join(., pivot_wider(resids, names_from = Sourcefile, values_from = sd_resid_bySource) ) %>%
  mutate(isoscape = case_when(
    isoscape == "precip_val_66100" ~ "Annual",
    isoscape == "precip_val_66098"  ~ "June-August"
  )) %>%
  write.csv(., file = file.path(wd$figs, "selectedTransferFunctionsTable.csv"), row.names = FALSE)

