### Runs to transition from ASAP to WHAM

# pak::pkg_install("timjmiller/wham@lab", lib = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library(kableExtra)
require(tidyverse)


### Source required functions

# Function to calculate uncertainty
source(file.path("R","calc_uncertainty_log_ests.R"))
# Function to extract time series estimates 
source(file.path("R","extract_time_series_ests.R"))
# Function to read in asap estimates
source(file.path("R","import_asap_ests.R"))
# Function to create plots comparing a WHAM run to previous ASAP output
source(file.path("R","compare_asap_wham_ests.R"))


rungroup.dir <- "Runs.ASAP.WHAM.Transition"


##### ASAP #####

# Read in 2023 ASAP file used in final 2023MT run, modified to meet WHAM requirements
mt2023.orig <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN9_For_WHAM.dat")

# Import ASAP output
asap.dir <- "2023.MT.ASAP/ASAP.files/"
asap.ests.csv.fname <- "ASAP_summary_Run9.MCMC.csv"
mt2023.ests <- import_asap_ests(asap.dir, asap.ests.csv.fname)



##### Run1: asap-like run with file from 2023 MT #####

run1.asap <- mt2023.orig
run1.input <- prepare_wham_input(run1.asap)
run1 <- fit_wham(run1.input, do.osa = F, do.retro = T)
check_convergence(run1)
# Save output
run1.dir <- file.path(rungroup.dir, "run1")
if(!dir.exists(run1.dir)) {dir.create(run1.dir)}

plot_wham_output(run1, dir.main=file.path(getwd(),run1.dir))
saveRDS(run1, file=file.path(run1.dir, "run1.rds"))

# Extract time series estimates for comparison to ASAP output
run1.ests <- extract_time_series_ests(run1.dir, "run1.rds")

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run1.ests, file.path(run1.dir, "Comparison.figures")) 

# Look at selectivity estimates
run1.sel <- run1$rep$selAA

