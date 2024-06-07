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
index.names <- mt2023.orig[[1]]$dat$"index.names"

# Import ASAP output
asap.dir <- "2023.MT.ASAP/ASAP.files/"
asap.ests.csv.fname <- "ASAP_summary_Run9.MCMC.csv"
mt2023.ests <- import_asap_ests(asap.dir, asap.ests.csv.fname)
mt2023.rdat <- dget("2023.MT.ASAP/ASAP.files/Run9.RDAT")

age.vec <- as.character(1:mt2023.orig[[1]]$dat$n_ages)
nage <- length(age.vec)

mt2023.orig[[1]]$dat$"index.names"
mt2023.init.sel.fish <- as_tibble(mt2023.rdat$sel.input.mats$fleet.sel.ini[,1]) %>% filter(value>0) 


# Total number of initial index selectivity parameters
ninit.indexsel <- length(mt2023.rdat$sel.input.mats$index.sel.ini[,2])/length(index.names)
mt2023.sel.index.init <- lapply(split(mt2023.rdat$sel.input.mats$index.sel.ini[,1], rep(1:3, each=ninit.indexsel)),
                                as_tibble)
mt2023.sel.index.phase <- lapply(split(mt2023.rdat$sel.input.mats$index.sel.ini[,2], rep(1:3, each=ninit.indexsel)),
                                as_tibble)



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
run1.sel[[1]][1,] # Fishery
run1.sel[[3]][1,] # Big
run1.sel[[4]][1,] # Alb



##### Run2: Fix albatross selectivity #####

run2.input <- run1.input

run2.sel.spec=list(model=c("age-specific",
                           "age-specific",
                           "age-specific",
                           "age-specific"), 
          initial_pars=list(c(0.1,0.5,0.8,1,1,1,1,1,1,1),
                            c(1,1,1,1,1,1,1,1,1,1),
                            c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                            c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
          fix_pars = list(
            c(6:10),
            c(1:10),
            c(1:3,7:10),
            c(1:3,7:10))
) 

input_1 <- prepare_wham_input(y, 
                              selectivity=sel
)





