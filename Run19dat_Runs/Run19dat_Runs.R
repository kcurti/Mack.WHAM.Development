library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.3/multi_wham")
library(kableExtra)
require(tidyverse)

# read in 2023 ASAP file but with empirical CVs for all indices
mt2023 <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN19.dat")
    #y <- read_asap3_dat("2023.MT.ASAP/ASAP.files/RUN19.dat")

#Increase ESS
  names(mt2023[[1]]) # one stock
  names(mt2023[[1]]$dat)
  mt2023[[1]]$dat$"index.names"
  age.vec <- as.character(1:mt2023[[1]]$dat$n_ages)
# Bigelow
big <- as_tibble(mt2023[[1]]$dat$IAA_mats[[2]]) %>%
  rename_with(~c("Year","Value","CV",age.vec,"ESS")) %>%
  mutate(ESS = if_else(ESS>0,1000,0))
mt2023[[1]]$dat$IAA_mats[[2]] <- big
# Albatross
alb <- as_tibble(mt2023[[1]]$dat$IAA_mats[[3]]) %>%
  rename_with(~c("Year","Value","CV",age.vec,"ESS")) %>%
  mutate(ESS = if_else(ESS>0,1000,0))
mt2023[[1]]$dat$IAA_mats[[3]] <- alb
# Fishing fleet
mt2023[[1]]$dat$catch_Neff <- 1000

mt2023[[1]]$map$trans_NAA_rho


#### m1:asap like run ####
input_1 <- prepare_wham_input(y)

input_1$map$trans_NAA_rho

m1 <- fit_wham(input_1, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m1)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run1")
plot_wham_output(m1)
saveRDS(m1, file=paste0("m",2,".rds"))
