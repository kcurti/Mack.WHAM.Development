# Function to
#   Read in ASAP estimates from the csv files that I created during ASAP runs
#   Organize for easy ggplotting
#    asap.dir = path to directory with ASAP run ("2023.MT.ASAP/ASAP.files/")
#    asap.ests.csv.fname = name of ASAP csv file  ("ASAP_summary_Run9.MCMC.csv")


require(tidyverse)


import_asap_ests <- function(asap.dir, asap.ests.csv.fname)
{

  asap.ests.csv <- read_csv(file.path(asap.dir, asap.ests.csv.fname))

  asap.ests <- bind_rows(
    SSB = asap.ests.csv %>%
      rename(est = SSB,
             lo = SSB_95_lo, 
             hi = SSB_95_hi) %>%
      select(Year, est, lo, hi)
    ,
    F = asap.ests.csv %>%
      rename(est = Freport,
             lo = Freport_95_lo, 
             hi = Freport_95_hi) %>%
      select(Year, est, lo, hi)
    ,
    Rect = asap.ests.csv %>%
      rename(est = Recr,
             lo = Recr_95_lo, 
             hi = Recr_95_hi) %>%
      select(Year, est, lo, hi)
    ,
    .id = "Estimate"
  )
                            
  return(asap.ests)                            
}



