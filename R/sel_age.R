# This is a loop to look at random effects on selectivity

# load
require(wham)
require(tidyverse)
require(here)
require(ggplot2)



write.dir <- "~/Mackerel/sel_re"
dir.create(write.dir)
setwd(write.dir)

x <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN4.dat")

sel_model <- c(rep("age-specific",5))

# time-varying options for each of 3 blocks (b1 = fleet, b2-3 = indices)
sel_re <- list(
  c("none","none","none","none"),
  c("iid","none","none","none"),
  c("ar1","none","none","none"),
  c("ar1_y","none","none","none"),
  c("2dar1","none","none","none"))
n.mods <- length(sel_re)

# summary data frame
df.mods <- data.frame(Model=paste0("m",1:n.mods), 
                      Selectivity=sel_model, # Selectivity model (same for all blocks)
                      Block1_re=sapply(sel_re, function(x) x[[1]])) # Block 1 random effects
rownames(df.mods) <- NULL

df.mods

x <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN4.dat")

# Increase ESS in support of Dirichlet distribution
x$dat$IAA_mats[[2]][42:52,14] <- rep(200,length(42:52))
x$dat$IAA_mats[[3]][7:41,14] <- rep(200,length(7:41))
x$dat$catch_Neff[,1] <- rep(200, length(x$dat$catch_Neff[,1]))


for(m in 1:n.mods){
  
  tryCatch({
  
  sel=list(model=c("age-specific",
                   "age-specific",
                   "age-specific",
                   "age-specific"), 
           re=sel_re[[m]],  
           initial_pars=list(c(0.1, 0.3, 0.6, 0.8, 1, 1, 1, 1, 1, 1),
                             c(1,1,1,1,1,1,1,1,1,1),
                             c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                             c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
           fix_pars = list(
             c(6:10),
             c(1:10),
             c(1:3,7:10),
             c(1:3,7:10))
  )
  
  input_sel_hybrid <- prepare_wham_input(x, 
                                         selectivity=sel,
                                         #recruit_model=2,
                                         age_comp = list(fleets = rep("logistic-normal-miss0",1), 
                                                         indices = rep("logistic-normal-miss0", 3)),
                                         NAA_re =NULL
                                         #NAA_re = list(sigma="rec", type = "ar1_y"),
                                         #model_name="Run1"
  )
  
  
  
  # fit model
  mod <- fit_wham(input_sel_hybrid, do.check=T, do.osa=F, do.proj=F, do.retro=T) 
  saveRDS(mod, file=paste0("m",m,".rds"))
   }, error=function(e){})
}

mod.list <- file.path(write.dir,paste0("m",1:n.mods,".rds"))
mods <- lapply(mod.list, readRDS)

#mod.list<-mod.list[c(1:4)]

#M_model2 <-age_comps[c(1:7,9:10)]

#sel_model2 <-sel_model[c(1:5)]

#mods <- lapply(mod.list, readRDS)

vign4_conv <- lapply(mods, function(x) capture.output(check_convergence(x)))
for(m in 1:n.mods) cat(paste0("Model ",m,":"), vign4_conv[[m]], "", sep='\n')

ok_sdrep = sapply(mods, function(x) if(x$na_sdrep==FALSE & !is.na(x$na_sdrep)) 1 else 0)
pdHess <- as.logical(ok_sdrep)
# did stats::nlminb converge?
conv <- sapply(mods, function(x) x$opt$convergence == 0) # 0 means opt converged
conv_mods <- (1:n.mods)[pdHess] 
#for(m in conv_mods){
#  plot_wham_output(mod=mods[[m]], out.type='html', dir.main=file.path(write.dir,paste0("m",m)))
#}


df.aic <- as.data.frame(compare_wham_models(mods, do.plot = FALSE, table.opts=list(sort=FALSE, calc.rho=TRUE))$tab)
df.aic[!pdHess,] = NA
minAIC <- min(df.aic$AIC, na.rm=T)
df.aic$dAIC <- round(df.aic$AIC - minAIC,1)
df.mods <- cbind(data.frame(#Model=paste0("m",1:length(mods)), 
  #"Block1_re"=sapply(sel_re, function(x) x[[2]]),
  "opt_converged"= ifelse(conv, "Yes", "No"),
  "pd_hessian"= ifelse(pdHess, "Yes", "No"),
  "NLL"=sapply(mods, function(x) round(x$opt$objective,3)),
  "Runtime"=sapply(mods, function(x) x$runtime)), df.aic)
rownames(df.mods) <- NULL
df.mods

write.csv(df.mods,"df.mods.csv")

################################
