# Function to calculate CVs and CIs for estimated time series that are outputted in log space   
# Input object (series) must be a data.frame of (Year, log.est, log.se)
# Ex: 
#   SSB.yr <- 
#   bind_cols(Year = model.yrs,
#             log.est = as.vector(WHAM_output.ests[['log_SSB']]),
#             log.se = as.vector(WHAM_output.sd[['log_SSB']])
#   ) %>%
#   calc.uncertainty()

require(tidyverse)


calc.uncertainty <- function(series)  {
  series %>%
    mutate( est = exp(log.est),
            se = exp(log.se),
            CV = sqrt(exp(log.se*log.se)-1),
            lo = exp(log.est - qnorm(0.975)*log.se),
            hi = exp(log.est + qnorm(0.975)*log.se)
    ) %>%
    select(Year, est, se, CV, lo, hi)
}  

