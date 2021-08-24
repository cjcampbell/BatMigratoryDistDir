# Setup -------------------------------------------------------------------

# Molt dates (from the literature)
molt_df <- data.frame(
  "Binomial" = c("Lasiurus cinereus", "Lasiurus borealis", "Lasionycteris noctivagans"),
  "Species" = c("LACI", "LABO", "LANO"),
  "MoltDateSource" = c("Cryan2014", "Pylant2014", "Fraser2017"),
  "From" = c(171, 165, 171),
  "To" = c(235, 219, 238)
  ) %>%
  dplyr::mutate_if(is.factor, as.character )

mydata_isoVals <- readRDS( file.path(wd$bin, "mydata_isoVals.rds") )

mydata.isoVals.highCertainty <- mydata_isoVals %>%  filter(
  uncertainty<=50e3|is.na(uncertainty) )
saveRDS(mydata.isoVals.highCertainty, file = file.path(wd$bin, "mydata.isoVals.highCertainty.rds"))

mydata <- read.csv( file.path(wd$data, "alldat.csv") )

# Write regression functions ---------------------------------------------------

# Regression function.
doSMA_Regression <- function(data, y.data, x.data, ...) {

  if(class(y.data) == "character"){ y.data <- data[ , y.data] }
  if(class(x.data) == "character"){ x.data <- data[ , x.data] }
  if(length(unique(y.data)) <= 2 | length(unique(x.data)) <= 2){
    stop("Not enough data to run regression") }

  mymodel <- smatr::sma(y.data ~ x.data)

  m <- list(...)
  m$intercept <- coef(mymodel)[1]
  m$slope     <- coef(mymodel)[2]
  m$sdRes     <- sd(residuals(mymodel))
  m$n         <- mymodel$groupsummary$n
  m$p         <- mymodel$groupsummary$pval
  m$r2        <- mymodel$groupsummary$r2
  m$model      <- mymodel
  return(m)
}

regWithResampling <- function( data, spp, xCol, colsToResampleBy, min_n ) {


  sampledData <- data.frame()
  while( nrow(sampledData) < min_n ){
    sampledData  <- data %>%
      group_by_at(vars(tidyselect::all_of(colsToResampleBy))) %>%
      dplyr::sample_n(1)
  }

  ydat <- unlist( sampledData[ , "d2H"] )
  xdat <- unlist( sampledData[ , xCol] )

  mymodel <- smatr::sma(ydat~xdat)

  return(
    cbind(
      coef(mymodel)[1],               # intercept
      coef(mymodel)[2],               # slope
      sd(residuals(mymodel)),         # sdRes
      mymodel$groupsummary$n,         # n
      mymodel$groupsummary$pval,      # pval
      mymodel$groupsummary$r2         # r2
    )
  )
}

getBootResults <- function(startRow, endRow, savedirectory = file.path(wd$bin, "tmp_boot") ) {

  print(paste("running", startRow, "to", endRow, "of", nrow(windows_to_run) ))
  if(!dir.exists(savedirectory)) dir.create(savedirectory)

  bootResults0 <- lapply(startRow:endRow, function(x) {

    df <- dplyr::filter(mdf,
                        Species == windows_to_run[ x, "Species"] ,
                        analysisLab == windows_to_run[ x, "analysisLab"] ,
                        yDay >= windows_to_run[ x, "from"] ,
                        yDay <= windows_to_run[ x, "to"]
    )

    if(windows_to_run[ x, "wind_killed_included"] == "no") {
      df <- dplyr::filter(df, wind_killed == "no")
    }

    if(nrow(df) == 0) {
      howManyRows <- 0
    } else {
      howManyRows <- df %>%
        group_by_at(vars(tidyselect::all_of(colsToResampleBy))) %>%
        dplyr::sample_n(1) %>%
        nrow()
    }

    if(howManyRows >= min_n ) {

      myBoot <- boot::boot(
        statistic = regWithResampling, R = myR, sim = "parametric",
        data = df,
        colsToResampleBy = c("decimalLatitude", "decimalLongitude", "Year"),
        xCol = as.character(windows_to_run[ x, "isoscape"]),
        min_n = min_n,
        parallel = "multicore", ncpus = nClusters
      )

      out <- myBoot$t %>%
        as.data.frame() %>%
        {names(.) <- c("intercept", "slope", "sdRes", "n", "pval", "r2"); .} %>%
        lapply(function(x) data.frame(
          mean = mean(x),
          sd = sd(x),
          min = min(x),
          max = max(x)
        )) %>%
        plyr::ldply() %>%
        dplyr::rename(param = '.id') %>%
        tidyr::pivot_wider(
          names_from = param,
          values_from = c("mean", "sd", "min", "max")
        ) %>%
        data.frame(windows_to_run[ x, ], .)

      return(out)

    }
  } )

  m <- plyr::compact(bootResults0) %>%
    bind_rows()
  if(nrow(m) > 0){
    data.frame(m, R = myR) %>%
      saveRDS(., file = file.path(savedirectory, paste(startRow, endRow, "bootTransferFxnResults.rds", sep = "_") ) )
  }

}

# Fit regression models. -------------------------------------------------------

myR <- 1000
nClusters <- detectCores()
colsToResampleBy = c("decimalLatitude", "decimalLongitude", "Year")
min_n <- 20 # This could be lower, but I want a robust sample size for known-origin individuals! Even this is borderline too low.

# Add n rows where Sourcefile == "all"
mdf <- mutate(mydata.isoVals.highCertainty, Sourcefile = "all") %>%
  bind_rows(mydata.isoVals.highCertainty) %>%
  dplyr::mutate(
    analysisLab = case_when(
      Sourcefile == "all"~ "all",
      Sourcefile == "Baerwald" ~ "Baerwald",
      Sourcefile == "Cryan" ~ "Cryan",
      Sourcefile == "Fraser" ~ "Fraser",
      TRUE ~ "CASIF"
    )
  )

# Add in literature-defined molt periods.
a <- molt_df %>%
  dplyr::rename(from = From, to = To) %>%
  dplyr::select(Species, from, to)
LitmoltPeriods0 <- lapply(1:nrow(a), function(x) {
  o <- expand.grid(unique(mdf$analysisLab), grep("precip_val", names(mdf), value= T)) %>%
    data.frame(., a[x, ]  )
  names(o)[1:2] <- c("analysisLab", "isoscape")
  return(o)
} ) %>%
  bind_rows

LitmoltPeriods <- mutate(LitmoltPeriods0, wind_killed_included = "yes") %>%
  rbind(mutate(LitmoltPeriods0, wind_killed_included = "no"))

windows_to_run <- LitmoltPeriods %>%
  rbind(
    mutate(LitmoltPeriods, to = case_when(
      Species == "LABO" ~ "199" ,
      Species == "LACI" ~ "200" ,
      Species == "LANO" ~ "214"
    ))
  )

# Divide into batches
n <- 50
rawRows <- floor( seq(1, nrow(windows_to_run)+1, length.out = n) )
startRows <- rawRows[1:(n-1)]
endRows <- rawRows[2:n] - 1

# Run.
if(length(list.files(file.path(wd$bin, "tmp_boot"))) != 0) stop("Files exist in tmp directory. Probably want to clear those first!")

mapply(getBootResults, startRows, endRows )

# Save Bootstrapped SMA results ------------------------------------------------------------

# Combine results.
sma_results <- list.files(file.path(wd$bin, "tmp_boot"), full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows %>%
  dplyr::mutate_if(is.factor, as.character)

saveRDS(sma_results, file = file.path( wd$bin, "sma_results.rds") )


# Select transfer functions to apply. ----------------------------

if(!exists("sma_results")) sma_results <- readRDS(file = file.path(wd$bin, "sma_results.rds"))

sma_selected <- sma_results %>%
  group_by(Species) %>%
  dplyr::arrange(desc(mean_r2)) %>%
  slice(1)

saveRDS(sma_selected, file = file.path( wd$bin, "sma_selected.rds") )

# Find model residuals ----------------------------------------------------

if(!exists("mydata.isoVals.highCertainty")) {
  mydata.isoVals.highCertainty <- readRDS(file.path(wd$bin, "mydata.isoVals.highCertainty.rds"))
}

# First, find individuals that fall within the best-performing model's
# molt period.

residual_sumtab <- lapply(SoI, function(spp){
  targetIndivs <- mydata.isoVals.highCertainty %>%
    filter(
      Species == spp,
      yDay >= as.numeric(sma_selected[ sma_selected$Species == spp , "from"]),
      yDay <= as.numeric(sma_selected[ sma_selected$Species == spp , "to"])
    ) %>%
    dplyr::rename(
      precipAtSampleSite = !!as.character(sma_selected[ sma_selected$Species == spp , "isoscape"])
    )

  # Then, fit a dummy model.
  dummyModel <- smatr::sma(d2H~precipAtSampleSite, data = targetIndivs)
  dummyModel2 <- dummyModel
  # Then insert calculated average coefficients
  dummyModel2$coef[[1]][1:2,1] <- unlist(sma_selected[ sma_selected$Species == spp , c("mean_intercept", "mean_slope")])
  # Calculate new residuals
  resids <- smatr::fitted.sma(dummyModel2, type = "residuals", newdata = targetIndivs)
  resids_to_averagedModel <- bind_cols(targetIndivs, resids = resids)

  resids_to_averagedModel %>%
    group_by(Species, Sourcefile) %>%
    dplyr::summarise(sd_resid_bySource = sd(resids))
}) %>% bind_rows()

saveRDS(residual_sumtab, file = file.path(wd$bin, "residual_sumtab.rds"))


# Apply transfer functions, define molt status  ---------------------------------

mydata_transformed <- mydata %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::left_join(
    dplyr::select(sma_selected, Species, isoscape, from, to, mean_intercept, mean_slope),
    by = "Species"
  ) %>%
  dplyr::left_join(residual_sumtab) %>%
  dplyr::mutate(
    dDprecip =  (d2H - mean_intercept) / mean_slope,
    MoltStatus = if_else(is.na(yDay), as.numeric(NA),
                         if_else( yDay >= from & yDay <= to, 1, 0 ) ),
    molt_status =
      if_else( from <= yDay & to >= yDay, "Summering",
               if_else( yDay > (to + migration_interval) | yDay < (from - migration_interval),
                        "Migrated", "Migrating" ) ),
    days_since_molt_period =
      if_else( yDay > to, yDay - to,
               if_else( yDay < from , (365 - to) + yDay,
                        as.numeric(0)) ),
    wind_killed = as.factor( if_else(sampling_method %in% c("TURBINE_CARCASS", "turbine_carcass"), "yes", "no" ) )
  )

saveRDS(mydata_transformed, file = file.path(wd$bin, "mydata_transformed.rds"))
