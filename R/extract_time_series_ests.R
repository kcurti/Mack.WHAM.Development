# Function to extract time series estimates (SSB, Ffull and Recruitment) and associated CIs from WHAM outputs
# Requires sourcing calc_uncertainty_log_ests.R function
# Inputs
#    run.dir = path to directory with WHAM run ("Run19dat_Runs/run0")
#    model.rds = name of model fit rds file  ("m0.RDS")


require(tidyverse)

# source(file.path("R","calc_uncertainty_log_ests.R"))


extract_time_series_ests <- function(run.dir, model.rds) {

  ### Read in model; Extract estimates and stdevs
  
  WHAM_output <- readRDS(file.path(run.dir, model.rds))
  WHAM_output.ests <- as.list(WHAM_output$sdrep, what = "Est", report = TRUE)
  WHAM_output.sd <- as.list(WHAM_output$sdrep, what = "Std", report = TRUE)
  
  ### Model specs
  
  model.yrs <- WHAM_output$years
  model.lyr <- tail(model.yrs,1)
  model.nyrs <- length(model.yrs)
  ages.labels <- WHAM_output$ages.lab
  nage <- length(ages.labels)
  
  SSB.yr <- 
    bind_cols(Year = model.yrs,
              log.est = as.vector(WHAM_output.ests[['log_SSB_all']]),
              log.se = as.vector(WHAM_output.sd[['log_SSB_all']])
    ) %>%
    calc.uncertainty()
  
  Rect.yr <- 
    bind_cols(Year = model.yrs,
              log.est = as.vector(WHAM_output.ests[['log_NAA_rep']][1,1,,1]),
              log.se  = as.vector(WHAM_output.sd[['log_NAA_rep']][1,1,,1])
    ) %>%
    calc.uncertainty() 
  
  F.yr <- 
    bind_cols(Year = model.yrs, 
              log.est = as.vector(WHAM_output.ests[['log_F_tot']]),
              log.se = as.vector(WHAM_output.sd[['log_F_tot']])
    ) %>%
    calc.uncertainty() 
  
  estimates.yr <- 
    bind_rows(SSB=SSB.yr, Rect=Rect.yr, F=F.yr, .id="Estimate")

  return(estimates.yr)
}


