# This is a loop to look at different age comps

# load
require(wham)
require(tidyverse)
require(here)
require(ggplot2)

setwd("~/Mackerel/age_comps")

write.dir <- "~/Mackerel/age_comps"

age_comps <- c( "multinomial",
                "dir-mult",
                "dirichlet-miss0",
                "dirichlet-pool0",
                "logistic-normal-miss0",
                "logistic-normal-ar1-miss0",
                "logistic-normal-pool0",
                "logistic-normal-01-infl",
                "logistic-normal-01-infl-2par",
                "mvtweedie" )


n.mods <- length(age_comps)

# summary data frame
df.mods <- data.frame(Model=paste0("m",1:n.mods), 
                      Age_Comp =age_comps)

rownames(df.mods) <- NULL

df.mods

x <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN4.dat")

# Increase ESS in support of Dirichlet distribution
x$dat$IAA_mats[[2]][42:52,14] <- rep(200,length(42:52))
x$dat$IAA_mats[[3]][7:41,14] <- rep(200,length(7:41))
x$dat$catch_Neff[,1] <- rep(200, length(x$dat$catch_Neff[,1]))

sel=list(model=c("logistic",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         # re = c("ar1","none","ar1","ar1"), 
         initial_pars=list(c(2,0.3),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(NULL),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 

for(m in 1:n.mods){
  
  input_ASAP <- prepare_wham_input(x, 
                                   selectivity=sel,
                                   #recruit_model=3,
                                   age_comp = list(fleets = rep(age_comps[m],1), 
                                                   indices = rep(age_comps[m], 3))
                                   #NAA_re = list(sigma="rec", cor ="iid"),
                                   #catchability = q,
                                   #model_name="Run18_ASAP_like"
                                   )
  

 
  # fit model
  mod <- fit_wham(input_ASAP, do.osa=F, do.retro=T) 
  saveRDS(mod, file=paste0("m",m,".rds"))
  
}

mod.list <- file.path(write.dir,paste0("m",1:n.mods,".rds"))
mods <- lapply(mod.list, readRDS)

#mod.list<-mod.list[c(1:7,9:10)]

#M_model2 <-age_comps[c(1:7,9:10)]


#M_re2<-M_re[c(1:7,9:10)]

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

compare_wham_models(mods,do.table = TRUE,
                    do.plot = FALSE)

df.aic <- as.data.frame(compare_wham_models(mods, do.plot = FALSE,table.opts=list(calc.aic=TRUE,sort=TRUE, calc.rho=TRUE))$tab)
#df.aic[!pdHess,] = NA
minAIC <- min(df.aic$AIC, na.rm=T)
df.aic$dAIC <- round(df.aic$AIC - minAIC,1)
df.mods <- cbind(data.frame(Model=paste0("m",1:length(mods)), age_comp=age_comps,
                            "Block1_re"=sapply(age_comps, function(x) x[[1]]),
                            "opt_converged"= ifelse(conv, "Yes", "No"),
                            "pd_hessian"= ifelse(pdHess, "Yes", "No"),
                            "NLL"=sapply(mods, function(x) round(x$opt$objective,3)),
                            "Runtime"=sapply(mods, function(x) x$runtime)), df.aic)
rownames(df.mods) <- NULL
df.mods

write.csv(df.mods,"df.mods.csv")

################################
