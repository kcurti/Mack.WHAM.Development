##### General guidance #####
# First move to different likelihood for age comps; Do not necessarily need the "right" age-comp model before going to random effects
  # Once have some idea of random effects, then review likelihood assumptions
# Then start to look at different random effect assumptions
# Start most complex, see what process error terms go to zero and go backwards
# Can do plot_wham_output without sdreport to get parameter estimate table
# # or look at ops$pars and look at the specific sigma parameters to see if they go to zero


# pak::pkg_install("timjmiller/wham@lab", lib = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library(kableExtra)
require(tidyverse)


# Read in 2023 ASAP file but with empirical CVs for all indices
rungroup.dir <- "Run19dat_Runs"
mt2023 <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN19.dat")

names(mt2023[[1]]) # one stock
names(mt2023[[1]]$dat)
mt2023[[1]]$dat$"index.names"
age.vec <- as.character(1:mt2023[[1]]$dat$n_ages)

# Increase ESS
mt2023.modESS <- mt2023
# Bigelow
big <- mt2023.modESS[[1]]$dat$IAA_mats[[2]]
  colnames(big) <- c("Year","Value","CV",age.vec,"ESS")
big[big[,"ESS"]>0,"ESS"] <- 1000
# big <- as_tibble(mt2023[[1]]$dat$IAA_mats[[2]]) %>%
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



##### M1: asap-like run but with original index CVs and slightly modified Bigelow selectivity #####

m1.asap <- mt2023
m1_input <- prepare_wham_input(m1.asap)
m1_input$map$trans_NAA_rho
# Correlation parameters for NAA random effects
# First one is age, second one is year (if decoupled, it is just for survival) and the third one is for recruitment, if decoupled
# If NAs, none are estimated
m1_input$par$trans_NAA_rho
# Zeros: Mean that the rho is in the middle of the transformed range (which is -1:1)

# Run model
m1_nofit <- fit_wham(m1_input, do.osa = F, do.retro = F, do.fit = F)
m1 <- fit_wham(m1_input, do.osa = F, do.retro = T)
  check_convergence(m1)

# Save output  
m1.dir <- file.path(rungroup.dir, "run1")
if(!dir.exists(m1.dir)) {dir.create(m1.dir)}
  
plot_wham_output(m1, dir.main=file.path(getwd(),m1.dir))
saveRDS(m1, file=file.path(m1.dir, "m1.rds"))



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
  m2 <- fit_wham(m2_input, do.osa=F, do.retro=T) 
  print(check_convergence(m2))
  
  m2.subdir <- file.path(m2.dir, agecomp.name)
  if(!dir.exists(m2.subdir)) {dir.create(m2.subdir)}
  
  plot_wham_output(m2, dir.main=file.path(getwd(),m2.subdir))
  saveRDS(m2, file=file.path(m2.subdir, "m2.rds"))
}



                













