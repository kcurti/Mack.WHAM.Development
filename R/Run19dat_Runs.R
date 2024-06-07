##### Defaults

# Recruitment model = 2 (mean + deviations when do not have random effects turned on)


##### General guidance #####
# First move to different likelihood for age comps; Do not necessarily need the "right" age-comp model before going to random effects
  # Once have some idea of random effects, then review likelihood assumptions
# Then start to look at different random effect assumptions
# Start most complex, see what process error terms go to zero and go backwards
# Can do plot_wham_output without sdreport to get parameter estimate table
# # or look at ops$pars and look at the specific sigma parameters to see if they go to zero

# A priori, could be random effects in 1) recruitment
#                                      2) NAA
#                                      3) Fishery selectivity
#                                      4) Index catchability
#                                      5) Index selectivity (though estimability questionable)
# model_name="Run18_ASAP_like"


# pak::pkg_install("timjmiller/wham@lab", lib = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library(kableExtra)
require(tidyverse)

rungroup.dir <- "Run19dat_Runs"



### Source required functions

# Function to calculate uncertainty
source(file.path("R","calc_uncertainty_log_ests.R"))
# Function to extract time series estimates 
source(file.path("R","extract_time_series_ests.R"))
# Function to read in asap estimates
source(file.path("R","import_asap_ests.R"))
# Function to create plots comparing a WHAM run to previous ASAP output
source(file.path("R","compare_asap_wham_ests.R"))


### ASAP Dat files

# Read in 2023 ASAP file but with empirical CVs for all indices
mt2023.emp <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN19.dat")
# Read in 2023 ASAP file used in final 2023MT run, modified to meet WHAM requirements
mt2023.orig <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN9_For_WHAM.dat")

names(mt2023.emp[[1]]) # one stock
names(mt2023.emp[[1]]$dat)
mt2023.emp[[1]]$dat$"index.names"
age.vec <- as.character(1:mt2023.emp[[1]]$dat$n_ages)

# Increase ESS
mt2023.modESS <- mt2023.emp
# Bigelow
big <- mt2023.modESS[[1]]$dat$IAA_mats[[2]]
  colnames(big) <- c("Year","Value","CV",age.vec,"ESS")
big[big[,"ESS"]>0,"ESS"] <- 1000
# big <- as_tibble(mt2023.emp[[1]]$dat$IAA_mats[[2]]) %>%
#   rename_with(~c("Year","Value","CV",age.vec,"ESS")) %>%
#   mutate(ESS = if_else(ESS>0,1000,0))
mt2023.modESS[[1]]$dat$IAA_mats[[2]] <- big
# Albatross
alb <- mt2023.modESS[[1]]$dat$IAA_mats[[3]]
  colnames(alb) <- c("Year","Value","CV",age.vec,"ESS")
alb[alb[,"ESS"]>0,"ESS"] <- 1000
mt2023.modESS[[1]]$dat$IAA_mats[[3]] <- alb
# Fishing fleet
mt2023.modESS[[1]]$dat$catch_Neff[,] <- 1000



# ##### Run0: asap-like run with file from 2023 MT #####

run0.asap <- mt2023.orig
run0.input <- prepare_wham_input(run0.asap)
run0 <- fit_wham(run0.input, do.osa = F, do.retro = T)
  check_convergence(run0)
# Save output
run0.dir <- file.path(rungroup.dir, "run0")
  if(!dir.exists(run0.dir)) {dir.create(run0.dir)}

plot_wham_output(run0, dir.main=file.path(getwd(),run0.dir))
saveRDS(run0, file=file.path(run0.dir, "run0.rds"))

# Extract time series estimates for comparison to ASAP output
run0.ests <- extract_time_series_ests(run0.dir, "run0.rds")
  
# Import ASAP output
asap.dir <- "2023.MT.ASAP/ASAP.files/"
asap.ests.csv.fname <- "ASAP_summary_Run9.MCMC.csv"
mt2023.ests <- import_asap_ests(asap.dir, asap.ests.csv.fname)
  
# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run0.ests, file.path(run0.dir, "Comparison.figures")) 

# Look at selectivity estimates
run0.sel <- run0$rep$selAA















##### M1: asap-like run but with original index CVs and slightly modified Bigelow selectivity #####

m1.asap <- mt2023.emp
m1_input <- prepare_wham_input(m1.asap)
m1_input$map$trans_NAA_rho
# Correlation parameters for NAA random effects
# First one is age, second one is year (if decoupled, it is just for survival) and the third one is for recruitment, if decoupled
# If NAs, none are estimated
m1_input$par$trans_NAA_rho
# Zeros: Mean that the rho is in the middle of the transformed range (which is -1:1)

# Run model
m1_nofit <- fit_wham(m1_input, do.osa = F, do.retro = F, do.fit = F)
m1 <- fit_wham(m1_input, do.osa = T, do.retro = T)
  check_convergence(m1)

# Save output  
m1.dir <- file.path(rungroup.dir, "run1")
if(!dir.exists(m1.dir)) {dir.create(m1.dir)}
  
plot_wham_output(m1, dir.main=file.path(getwd(),m1.dir))
saveRDS(m1, file=file.path(m1.dir, "m1.rds"))

m1.compare <- compare_wham_models(list(run0=run0, m1=m1), calc.rho = TRUE, calc.aic=TRUE, fdir=file.path(getwd(),m1.dir))
  print(m1.compare)



##### M2: Loop over age-composition options #####

m2.dir <- file.path(rungroup.dir, "run2")
if(!dir.exists(m2.dir)) {dir.create(m2.dir)}

# Age comp options
  # Multinomial is the only one not self-weighted; 
  # All of the other options estimate a variance parameter
m2_age.comps <- c( "dir-mult",
                   "dirichlet-pool0",
                   "dirichlet-miss0",
                   "logistic-normal-miss0",
                   "logistic-normal-ar1-miss0",
                   "logistic-normal-pool0",
                   "dir-mult-linear" )
m2_nmodels <- length(m2_age.comps)
names(m2_age.comps) <- paste("m2", 1:m2_nmodels, sep='.')

# Prepare wham input using the asap file with modified ESS values
# Test that modified ESS values inputted correctly
m2_input <- prepare_wham_input(mt2023.modESS)
m2_input$data$index_Neff
m2_input$data$catch_Neff

# Loop over age comps
for(mod.no in 1:m2_nmodels)
{
  agecomp.name <- m2_age.comps[mod.no]
  print(mod.no); print(agecomp.name)
  m2_input <- prepare_wham_input(mt2023.modESS,
                                 age_comp = agecomp.name)
  m2 <- fit_wham(m2_input, do.osa=T, do.retro=T) 
  print(check_convergence(m2))
  
  m2.subdir <- file.path(m2.dir, agecomp.name)
  if(!dir.exists(m2.subdir)) {dir.create(m2.subdir)}
  
  plot_wham_output(m2, dir.main=file.path(getwd(),m2.subdir))
  saveRDS(m2, file=file.path(m2.subdir, "m2.rds"))
}

# Looping over age comps to read in m2 individual model fits for compare_wham
m2.list <- list()
for(mod.no in 1:m2_nmodels)
{
  agecomp.name <- m2_age.comps[mod.no]
  print(mod.no); print(agecomp.name)
  m2.subdir <- file.path(m2.dir, agecomp.name)
  # assign(paste("m2", mod.no, sep='.'), readRDS(m2, file=file.path(m2.subdir, "m2.rds")))
  m2.list[[names(agecomp.name)]] <- readRDS(m2, file=file.path(m2.subdir, "m2.rds"))
}

m2.compare <- compare_wham_models(m2.list, calc.rho = TRUE, calc.aic=TRUE, fdir=file.path(getwd(),m2.dir))
  print(m2.compare)
m2_age.comps[m2.compare$best]

##### ***Selecting logistic-normal-ar1-miss0 age comp #####



##### M3: Given logistic-normal-ar1-miss0 age comp, explore recruitment and survival random effects #####

m3_input <- prepare_wham_input(mt2023.modESS,
                               age_comp = "logistic-normal-ar1-miss0")

m3_input <- set_NAA(m3_input, NAA_re = list(sigma="rec+1",
                                            cor="2dar1",
                                            decouple_recruitment=TRUE))
# m3 <- fit_wham(m3_input, do.osa=F, do.retro=F, do.fit=FALSE) 
m3 <- fit_wham(m3_input, do.osa=F, do.retro=F, do.fit=TRUE, do.sdrep=TRUE) 
check_convergence(m3)
  m3$sdrep
  m3$parList$logit_selpars

### MGC was ok but hessian was not invertible
  # Checked sdrep and all of the SEs are NA; Noticed huge estimates for some logit_selpars
  # looked at parList$logit_selpars to see exactly which selectivity parameters are causing the problem 
  

