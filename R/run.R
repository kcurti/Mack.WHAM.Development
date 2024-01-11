# This code fits the haddock WHAM model and runs the MASE and 
# self-test analyses.

# Set working directory to source file location, then run.
# (Session->Set Woriking Directory->To Source File Location)

library(wham)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Functions needs to do MASE and self-testing
source("calc_hindcast_mase.R")
source("fit_hindcast.R")
source("do_selftest.R")
source("sim_fn.R")

# Load data
asap3_base2 <- read_asap3_dat("GOM_HADDOCK_ASAP_2022_BASE2.dat")

# Setup fit
input_di <-  prepare_wham_input(asap3_base2, 
                                selectivity = list(model = c(rep("age-specific", 3),
                                                             rep("age-specific", 2)),
                                                   re = c(rep("none", 3), 
                                                          rep("none", 2)),
                                                   initial_pars = list(c(0.01, 0.1, 0.3, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0),
                                                                       c(0.01, 0.1, 0.3, 0.5, 0.8, 1.0, 1.0, 1.0, 1.0),
                                                                       c(0.01, 0.1, 0.3, 0.5, 0.8, 1.0, 1.0, 1.0, 1.0),
                                                                       c(0.2, 0.4, 0.8, rep(1.0, 6)), #<< start surveys
                                                                       c(0.1, 0.3, 0.5, 0.8, rep(1.0, 5))
                                                   ),
                                                   fix_pars = list(c(5:8), 
                                                                   c(6:7),  
                                                                   c(6:9), 
                                                                   c(4:9),  #<< start surveys
                                                                   c(5:9)
                                                   )
                                ),
                                recruit_model = 2, 
                                model_name = "RE dirichlet",
                                NAA_re = list(sigma="rec+1", cor = "2dar1"),
                                age_comp = list(fleets = rep("dirichlet-miss0", 1), 
                                                indices = rep("dirichlet-miss0", 2)),
                                basic_info = list(simulate_process_error = rep(FALSE, 5),
                                                  XSPR_R_avg_yrs = 1:43,
                                                  XSPR_input_average_years = 41:45
                                ))

input_di$map$log_N1_pars <- as.factor(matrix(data=c(1,2,3,4,5,6,NA,NA,7),nrow=1,ncol=9))

# Fit model
mdi   <- fit_wham(input_di, do.osa = F, do.retro = F)


#### Claculate prediction error (MASE) ####
# This peels back survey data and predicts the survey indices in the future.
# It assumes catch is known, but no other survey data is known in the future.
# It generates plots of predicted vs observed, as well as a table in the console
# of MASE over all horizons for each survey, and the convergence rate over all 
# peels.
preds <- calc_hindcast_mase(model = mdi, # Model to use to make predictions
                            peel.max = 10, # Number of peels
                            horizon = c(1:5), # Years ahead to predict (max must be no more than peel.max)
                            drop=list(indices=1:mod$env$data$n_indices, # Drop all indices when making predictions
                                      index_paa=1:mod$env$data$n_indices),
                            indices2calc = NULL, # You can calculate MASE for a subset of indices, default (NULL) is all
                            dir_figures = getwd()) # where to save figures


#### Self-tests ####
# This uses the fitted model to generate simulated timeseries which the
# model then fits itself to. 
# The convergence rate and bias in SSB and F are reported.
# Note: 
# (1) Convergence is classified as first-order convergence only (i.e.
# check_convergence(fit, ret = T))
# (2) Only observation error is simulated due to 
# basic_info = list(simulate_process_error = rep(FALSE, 5)) in the 
# prepare_wham_input() call.

set.seed(0414) # For reproducibility (choose your lucky number)

selffits <- do_selftest(mod = mT, 
                        n_sim = 25, # How many simulations to do
                        n_sim2plot = 2, # Plot a couple of examples
                        dir_figures = getwd())


