##### Runs to transition from ASAP to WHAM #####

##### Ideas #####
#  Rerun Alex’s SSRT Run1 so that can directly compare single and multi wham


library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_168117c")
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


group.dir <- "Runs.ASAP.WHAM.Transition"



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

# Explore indices and index selectivity parameters
mt2023.orig[[1]]$dat$"index.names"
mt2023.init.sel.fish <- as_tibble(mt2023.rdat$sel.input.mats$fleet.sel.ini[,1]) %>% filter(value>0) 

# Total number of initial index selectivity parameters, initial values and phases from ASAP
mt2023.ninit.indexsel <- length(mt2023.rdat$sel.input.mats$index.sel.ini[,2])/length(index.names)
mt2023.sel.index.init <- lapply(split(mt2023.rdat$sel.input.mats$index.sel.ini[,1], rep(1:3, each=mt2023.ninit.indexsel)),
                                as_tibble)
mt2023.sel.index.phase <- lapply(split(mt2023.rdat$sel.input.mats$index.sel.ini[,2], rep(1:3, each=mt2023.ninit.indexsel)),
                                as_tibble)

# ASAP index selectivity estimates
mt2023.sel.index.values <- mt2023.rdat$index.sel

# ASAP fleet selectivity (time-invariant)
mt2023.sel.fishery.values <- mt2023.rdat$fleet.sel.mats[[1]][1,]

# ASAP catchability estimates
mt2023.q <- mt2023.rdat$q.indices



##### Alex's SSRT Run1 outputs #####

# Read in NAA estimates from Alex's original SSRT Run1 (to use for future comparisons)
ssrt_run1.dir <- c("C:/Users/Kiersten.Curti/Documents/GitHub.Repositories/Mack.WHAM.Development/Runs_SSRT/run1")
ssrt_run1.params <- readRDS(file.path(ssrt_run1.dir, "res_tables", "parameter_estimates_table.RDS"))
dim(ssrt_run1.params)
ssrt_run1.NAA <- readRDS(file.path(ssrt_run1.dir, "res_tables", "NAA_table.RDS"))
dim(ssrt_run1.NAA)

# 21 objects



##### Run1: asap-like run with file from 2023 MT #####

run.name <- 'run1'

# Create wham input object
run.input <- prepare_wham_input(mt2023.orig)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Look at selectivity estimates
run1.sel <- run1.fit$rep$selAA
run1.sel[[1]][1,] # Fishery
run1.sel[[3]][1,] # Big
run1.sel[[4]][1,] # Alb

# Extract NAA
  dim(run1.fit$report()$NAA)
run1.NAA <- run1.fit$report()$NAA[1,1,,]
  dim(run1.NAA)
  head(run1.NAA)

# Compare Run1 NAA to SSRT Run1 NAA - there are differences :(
run1.NAA.diff <- run1.NAA - ssrt_run1.NAA
  head(run1.NAA.diff)
  head(run1.NAA)
  head(ssrt_run1.NAA)

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))
  
# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])


### Take-home points
#   1) Run 1 is different than ASAP output
#   2) Run 1 is also different than SSRT run 1, though Alex changed the ESS's in SSRT run 1



##### Run2: Fix albatross selectivity for ages 9 and 10, bigelow selectivity for age-7 to mimic the ASAP parameterization #####

run.name <- 'run2'

# Create wham input object
run2.sel.spec=list(model=c("age-specific",  # Fishery
                           "age-specific",  # SSB egg index
                           "age-specific",  # Bigelow
                           "age-specific"), # Albatross
          initial_pars=list(c(0.1,0.5,0.8,1,1,1,1,1,1,1),
                            c(1,1,1,1,1,1,1,1,1,1),
                            c(0, 0, 1, 0.6, 0.4, 0.3, 0.3, 0, 0, 0),
                            c(0, 0, 1, 0.6, 0.4, 0.3, 0.3, 0.2, mt2023.sel.index.values[3,9:10])),
          fix_pars = list(
            c(6:10),
            c(1:10),
            c(1:3,7:10),
            c(1:3,9:10))
) 
run.input <- prepare_wham_input(mt2023.orig, selectivity = run2.sel.spec)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Look at selectivity estimates
run2.sel <- run2.fit$rep$selAA
run2.sel[[1]][1,] # Fishery
run2.sel[[3]][1,] # Big
run2.sel[[4]][1,] # Alb

# Extract NAA
dim(run2.fit$report()$NAA)
run2.NAA <- run2.fit$report()$NAA[1,1,,]
dim(run2.NAA)
head(run2.NAA)

# Compare Run2 NAA to SSRT Run1 NAA - there are differences
run2.NAA.diff <- run2.NAA - ssrt_run1.NAA

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

### Compare (again) selectivity ests with those from MT - doesn't seem to be a smoking gun
run2.sel[[1]][1,] # Fishery
run2.sel[[3]][1,] # Big
run2.sel[[4]][1,] # Alb

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()

### Take-home points
#   1) Run 2 is still different than ASAP output but much better than run 1
#   2) Run 2 is also still different than SSRT run 1, though Alex changed the ESS's in SSRT run 1



##### Comparison: Runs 1 and 2 ##### 

run.list <- c('run1', 'run2')
comp.name <- 'runs_1.2'

# Load runs and save to list
run.fits.list <- list()
for (run.name in run.list)
{
  # run.name <- 'run1'
  run.fits.list[[run.name]] <- readRDS(file=file.path(file.path(group.dir, run.name), paste(run.name,"rds",sep='.')))
}

# Create comparison diretory and plot 
comp.dir <- file.path(group.dir, tail(run.list,1), "Comparison.figures", comp.name)
if(!dir.exists(comp.dir)) {dir.create(comp.dir)}
compare_wham_models(run.fits.list, fdir=comp.dir)

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()



##### Run3: Fix all Bigelow selectivity parameters to see if can resolve remaining differences ##### 

run.name <- 'run3'

# Create wham input object
run3.sel.spec=list(model=c("age-specific",  # Fishery
                           "age-specific",  # SSB egg index
                           "age-specific",  # Bigelow
                           "age-specific"), # Albatross
                   initial_pars=list(c(0.1,0.5,0.8,1,1,1,1,1,1,1),
                                     c(1,1,1,1,1,1,1,1,1,1),
                                     c(0, 0, 1, mt2023.sel.index.values[2,4:7], 0, 0, 0),
                                     c(0, 0, 1, 0.6, 0.4, 0.3, 0.3, 0.2, mt2023.sel.index.values[3,9:10])),
                   fix_pars = list(
                     c(6:10),
                     c(1:10),
                     c(1:10),
                     c(1:3,9:10))
) 
run.input <- prepare_wham_input(mt2023.orig, selectivity = run3.sel.spec)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Look at selectivity estimates
run3.sel <- run3.fit$rep$selAA
run3.sel[[1]][1,] # Fishery
run3.sel[[3]][1,] # Big
run3.sel[[4]][1,] # Alb

# Look at catchability estimates
run3.fit$rep$q

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()



##### Comparison: Runs 2 and 3 ##### 

run.list <- c('run2', 'run3')
comp.name <- 'runs_2.3'

# Load runs and save to list
run.fits.list <- list()
for (run.name in run.list)
{
  # run.name <- 'run1'
  run.fits.list[[run.name]] <- readRDS(file=file.path(file.path(group.dir, run.name), paste(run.name,"rds",sep='.')))
}

# Create comparison diretory and plot 
comp.dir <- file.path(group.dir, tail(run.list,1), "Comparison.figures", comp.name)
if(!dir.exists(comp.dir)) {dir.create(comp.dir)}
compare_wham_models(run.fits.list, fdir=comp.dir)

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()

### Take home:
#   Still did not resolve differences in F at end of time series


##### Run4: Fix all (index and fishery) selectivity parameters to see if can resolve remaining differences ##### 

run.name <- 'run4'

# Create wham input object
run4.sel.spec=list(model=c("age-specific",  # Fishery
                           "age-specific",  # SSB egg index
                           "age-specific",  # Bigelow
                           "age-specific"), # Albatross
                   initial_pars=list(mt2023.sel.fishery.values,
                                     c(1,1,1,1,1,1,1,1,1,1),
                                     c(0, 0, 1, mt2023.sel.index.values[2,4:7], 0, 0, 0),
                                     c(0, 0, 1, mt2023.sel.index.values[3,4:10])),
                   fix_pars = list(
                     c(1:10),
                     c(1:10),
                     c(1:10),
                     c(1:10))
) 
run.input <- prepare_wham_input(mt2023.orig, selectivity = run4.sel.spec)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Look at selectivity estimates
run4.sel <- run4.fit$rep$selAA
run4.sel[[1]][1,] # Fishery
run4.sel[[3]][1,] # Big
run4.sel[[4]][1,] # Alb

# Look at catchability estimates
run4.fit$rep$q[1,]

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()

### Take-home
#   Differences reduced but still see difference in 2020-2021 F (in trend and not just magnitude); Index q’s also slightly different


##### Run5: Run4 (all selectivity parameters fixed) but set age-1 and age-2 as 1 instead of zero like it was in the original ASAP file ##### 

run.name <- 'run5'

# Create wham input object
run5.sel.spec=list(model=c("age-specific",  # Fishery
                           "age-specific",  # SSB egg index
                           "age-specific",  # Bigelow
                           "age-specific"), # Albatross
                   initial_pars=list(mt2023.sel.fishery.values,
                                     c(1,1,1,1,1,1,1,1,1,1),
                                     c(1, 1, 1, mt2023.sel.index.values[2,4:7], 0, 0, 0),
                                     c(1, 1, 1, mt2023.sel.index.values[3,4:10])),
                   fix_pars = list(
                     c(1:10),
                     c(1:10),
                     c(1:10),
                     c(1:10))
) 
run.input <- prepare_wham_input(mt2023.orig, selectivity = run5.sel.spec)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()

### Take-home:
#   Setting age-1 and age-2 BTS selectivity to zero does not work because WHAM apparently does not use age of first selectivity designation



##### Run6: Rerun Alex's Run1 from SSRT to make a direct single- to multi-wham comparison ##### 

run.name <- 'run6'

run6.asap <- mt2023.orig

#Increase ESS
run6.asap[[1]][['dat']][['IAA_mats']][[2]][c(42:52,54:55),14] <- 1000
run6.asap[[1]][['dat']][['IAA_mats']][[3]][7:41,14] <- 1000
run6.asap[[1]][['dat']][['IAA_mats']][[3]][42:55,2:13]<- -999
run6.asap[[1]][['dat']][['catch_Neff']][,1] <- 1000

# The following line is from Alex but the map object no longer exists 
# y$map$trans_NAA_rho

# Create wham input object
run.input <- prepare_wham_input(run6.asap)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()



##### Run6a: Add in selectivity; Rerun Alex's Run1 from SSRT to make a direct single- to multi-wham comparison ##### 

run.name <- 'run6a'

run6a.asap <- mt2023.orig

#Increase ESS
run6a.asap[[1]][['dat']][['IAA_mats']][[2]][c(42:52,54:55),14] <- 1000
run6a.asap[[1]][['dat']][['IAA_mats']][[3]][7:41,14] <- 1000
run6a.asap[[1]][['dat']][['IAA_mats']][[3]][42:55,2:13]<- -999
run6a.asap[[1]][['dat']][['catch_Neff']][,1] <- 1000

# Commercial selectivity: flattop --> logistic 
run6a.sel=list(model=c("logistic",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         # re = c("ar1","none","ar1","ar1"), 
         initial_pars=list(c(2,0.3),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(1, 1, 1, 0.6, 0.4, 0.2, 0.2, 0.2, 0.2, 0.2)),
         fix_pars = list(
           c(NULL),
           c(1:10),
           c(1:3,7:10),
           c(1:3))
) 


# Create wham input object
run.input <- prepare_wham_input(run6a.asap, selectivity = run6a.sel)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()



##### Run7: Rerun Alex's Run2 from SSRT to make a direct single- to multi-wham comparison ##### 

run.name <- 'run7'

run7.asap <- mt2023.orig

#Increase ESS
run7.asap[[1]][['dat']][['IAA_mats']][[2]][c(42:52,54:55),14] <- 1000
run7.asap[[1]][['dat']][['IAA_mats']][[3]][7:41,14] <- 1000
run7.asap[[1]][['dat']][['IAA_mats']][[3]][42:55,2:13]<- -999
run7.asap[[1]][['dat']][['catch_Neff']][,1] <- 1000

run7.sel=list(model=c("age-specific",
                  "age-specific",
                  "age-specific",
                  "age-specific"), 
          # re = c("ar1","none","ar1","ar1"), 
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

# Create wham input object
run.input <- prepare_wham_input(run7.asap, 
                              selectivity=run7.sel,
                              age_comp = "logistic-normal-pool0",
)

# Fit wham model
run.fit <- fit_wham(run.input, do.osa = F, do.retro = F)
check_convergence(run.fit)

# Create run directory
run.dir <- file.path(group.dir, run.name)
if(!dir.exists(run.dir)) {dir.create(run.dir)}

# Plot and save output
plot_wham_output(run.fit, dir.main=file.path(getwd(),run.dir))
run.rds.name <- paste(run.name,"rds",sep='.')
saveRDS(run.fit, file=file.path(run.dir, run.rds.name))

# Extract time series estimates for comparison to ASAP output
run.ests <- extract_time_series_ests(run.dir, run.rds.name)

# Compare WHAM run to ASAP output
compare_asap_wham_ests(mt2023.ests, run.ests, file.path(run.dir, "Comparison.figures")) 

# Assign generic objects to run-specific permanent objects
assign(paste(run.name,"input",sep='.'), run.input)
assign(paste(run.name,"fit",sep='.'), run.fit)
assign(paste(run.name,"ests",sep='.'), run.ests)

# Output resulting estimates to csv
write_csv(run.ests, file.path(run.dir, paste(run.name,"ests.csv",sep='.')))

# Remove generic run objects except run.name and run.dir
rm(run.input, run.fit, run.rds.name, run.ests)

# Save image
save.image(file.path(run.dir, paste(run.name,"RDATA",sep='.')))

# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()





##### Code to load saved workspace #####

library("wham", lib.loc = "C:/Users/Kiersten.Curti/AppData/Local/R/win-library/4.4/wham_168117c")
require(tidyverse)

run.name <- 'run4'

group.dir <- "Runs.ASAP.WHAM.Transition"
run.dir <- file.path(group.dir, run.name)
load(file.path(run.dir, paste(run.name,"RDATA",sep='.')))


# Remove all remaining objects associated with run
rm(list=ls()[grep("^run.",ls())])
ls()

