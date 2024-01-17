# mackerel
# ASAP --> WHAM
# Summer 2023

# like asap
# age comps
# recruitment
# time varying selectivity 
# time varying NAA

#### load packages & data ####
devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="devel")
require(wham)
require(tidyverse)
require(here)
require(ggplot2)

# read in 2019 ASAP file
y <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN9.dat")

#Increase ESS
y$dat$IAA_mats[[2]][c(42:52,54:55),14] <- 1000
y$dat$IAA_mats[[3]][7:41,14] <- 1000
y$dat$IAA_mats[[3]][42:55,2:13]<--999
y$dat$catch_Neff[,1] <- 1000

#### m1:asap like run ####
input_1 <- prepare_wham_input(y)

m1 <- fit_wham(input_1, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m1)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run1")
plot_wham_output(m1)
saveRDS(m1, file=paste0("m",2,".rds"))


# Commercial selectivity is flattop --> try logistic 
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

sel2=list(model=c("age-specific",
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

input_1 <- prepare_wham_input(y, 
                                 selectivity=sel
)

# run model
m1 <- fit_wham(input_1, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m1)


#### m2:age comps ####
input_2 <- prepare_wham_input(y, 
                                selectivity=sel2,
                                #recruit_model=3,
                                age_comp = "logistic-normal-miss0",
                                #NAA_re = list(sigma="rec", cor ="iid"),
                                #catchability = q,
                                #model_name="Run18_ASAP_like"
)


# run model
m2 <- fit_wham(input_2, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m2)

C:\Users\alex.hansell\Documents\GitHub\Mack.WHAM.2021MT\Runs\run3\plots_png\diagnostics
setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run2")
plot_wham_output(m2)
saveRDS(m2, file=paste0("m",2,".rds"))

#### m3: random walk rec ####
input_3 <- prepare_wham_input(y, 
                              selectivity=sel2,
                              recruit_model=1,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m3 <- fit_wham(input_3, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m3)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run3")
plot_wham_output(m3)
saveRDS(m3, file=paste0("m",3,".rds"))

#### m4: mean rec ar1 ####
input_4 <- prepare_wham_input(y, 
                              selectivity=sel2,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="ar1_y"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m4 <- fit_wham(input_4, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m4)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run4")
plot_wham_output(m4)
saveRDS(m4, file=paste0("m",4,".rds"))
#### m5: mean rec iid ####
input_5 <- prepare_wham_input(y, 
                              selectivity=sel2,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m5 <- fit_wham(input_5, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m5)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run5")
plot_wham_output(m5)
saveRDS(m5, file=paste0("m",5,".rds"))

r5 <- readRDS("C:/Users/alex.hansell/Documents/GitHub/Mack.WHAM.2021MT/Runs/run5/m5.rds")

mohns_rho(r5)

#### m6: BH rec ####
input_6 <- prepare_wham_input(y, 
                              selectivity=sel2,
                              recruit_model=3,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m6 <- fit_wham(input_6, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m6)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run6")
plot_wham_output(m6)
saveRDS(m6, file=paste0("m",6,".rds"))

r0 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run2/m2.rds")
r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run4/m4.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run5/m5.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run6/m6.rds")

mods<-list(r1,r2,r3,r4)
compare_wham_models(mods)

#### m7: Ricker rec ####
input_7 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=4,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m7 <- fit_wham(input_7, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m7)

setwd("~/Mackerel/runs/run7")
plot_wham_output(m7)
saveRDS(m7, file=paste0("m",7,".rds"))


#### m8: tv iid commercial ages ####
sel_re=list(model=c("age-specific",
                  "age-specific",
                  "age-specific",
                  "age-specific"), 
           re = c("iid","none","none","none"), 
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

input_8 <- prepare_wham_input(y, 
                              selectivity=sel_re,
                              recruit_model=1,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m8 <- fit_wham(input_8, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m8)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run8")
plot_wham_output(m8)
saveRDS(m8, file=paste0("m",8,".rds"))


#### m9: tv ar1y commercial ages ####
sel_re2=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1_y","none","none","none"), 
            initial_pars=list(c(0.1,0.5,0.8,1,1,1,1,1,1,1),
                              c(1,1,1,1,1,1,1,1,1,1),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(6:8),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_9 <- prepare_wham_input(y, 
                              selectivity=sel_re2,
                              recruit_model=1,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m9 <- fit_wham(input_9, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m9)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run9")
plot_wham_output(m9)
saveRDS(m9, file=paste0("m",9,".rds"))

#### m10: tv 2dar1 commercial ages ####
sel_re3=list(model=c("age-specific",
                     "age-specific",
                     "age-specific",
                     "age-specific"), 
             re = c("2dar1","none","none","none"), 
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

input_10 <- prepare_wham_input(y, 
                              selectivity=sel_re3,
                              recruit_model=1,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m10 <- fit_wham(input_10, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m10)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run10")
plot_wham_output(m10)
saveRDS(m10, file=paste0("m",10,".rds"))
#### m11: tv iid log ####
sel_re4=list(model=c("logistic",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
          re = c("iid","none","none","none"), 
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

input_11 <- prepare_wham_input(y, 
                               selectivity=sel_re4,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               #NAA_re = list(sigma="rec", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m11 <- fit_wham(input_11, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m11)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run11")
plot_wham_output(m11)
saveRDS(m11, file=paste0("m",11,".rds"))

r0 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run8/m8.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run9/m9.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run10/m10.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run11/m11.rds")

mods<-list(r0,r1,r2,r3,r4)
compare_wham_models(mods)

#### m12: NAA iid ####
input_12 <- prepare_wham_input(y, 
                               selectivity=sel2,
                               recruit_model= 1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m12 <- fit_wham(input_12, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m12)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run12")
plot_wham_output(m12)
saveRDS(m12, file=paste0("m",12,".rds"))

#### m13: NAA 2dar1 ####
input_13 <- prepare_wham_input(y, 
                               selectivity=sel2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m13 <- fit_wham(input_13, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m13)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run13")
plot_wham_output(m13)
saveRDS(m13, file=paste0("m",13,".rds"))




#### m14:dirichlet-pool0 ####
input_14 <- prepare_wham_input(y, 
                               selectivity=sel2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_a"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m14 <- fit_wham(input_14, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m14)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run14")
plot_wham_output(m14)
saveRDS(m14, file=paste0("m",14,".rds"))


#### m15: NAA ar1_ ####
input_15 <- prepare_wham_input(y, 
                               selectivity=sel2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m15 <- fit_wham(input_15, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m15)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run15")
plot_wham_output(m15)
saveRDS(m15, file=paste0("m",15,".rds"))

#r0 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run12/m12.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run13/m13.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run14/m14.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run15/m15.rds")

mods<-list(r1,r2,r3,r4)
compare_wham_models(mods)



#### m16: NAA iid ####

 input_16 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dirichlet-pool0",
                                NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m16 <- fit_wham(input_16, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m16)

setwd("~/Mackerel/runs/run16")
plot_wham_output(m16)
saveRDS(m16, file=paste0("m",16,".rds"))






#### m17: NAA 2dar1 ####
input_17 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dirichlet-pool0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m17 <- fit_wham(input_17, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m17)

setwd("~/Mackerel/runs/run17")
plot_wham_output(m17)
saveRDS(m17, file=paste0("m",17,".rds"))

#### m18: NAA iid ####

input_18 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dirichlet-miss0",
                               NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m18 <- fit_wham(input_18, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m18)

setwd("~/Mackerel/runs/run18")
plot_wham_output(m18)
saveRDS(m18, file=paste0("m",18,".rds"))

#### m19: NAA 2dar1 ####
input_19 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dirichlet-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m19 <- fit_wham(input_19, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m19)
# TMB:sdreport() was performed for this model, but it appears hessian was not invertible

setwd("~/Mackerel/runs/run19")
plot_wham_output(m19)
saveRDS(m19, file=paste0("m",19,".rds"))
#### m20: dir-mult NAA iid ####
input_20 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dir-mult",
                               NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m20 <- fit_wham(input_20, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m20)

setwd("~/Mackerel/runs/run20")
plot_wham_output(m20)
saveRDS(m18, file=paste0("m",20,".rds"))
#### m21: iid sel ####
input_21 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=2,
                               age_comp = "dir-mult",
                               NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m21 <- fit_wham(input_21, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m21)

setwd("~/Mackerel/runs/run21")
plot_wham_output(m21)
saveRDS(m21, file=paste0("m",21,".rds"))









#### m22: multinomial iid NAA ####
sel2=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         # re = c("ar1","none","ar1","ar1"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,1,1,1),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(7:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
)




input_22 <- prepare_wham_input(y, 
                               selectivity=sel2,
                               recruit_model=2,
                               age_comp = "multinomial",
                               NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m22 <- fit_wham(input_22, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m22)

setwd("~/Mackerel/runs/run22")
plot_wham_output(m22)
saveRDS(m22, file=paste0("m",22,".rds"))





#### m23 multinomial ####
input_23 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=2,
                              #age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m23 <- fit_wham(input_23, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m23)

setwd("~/Mackerel/runs/run23")
plot_wham_output(m23)
saveRDS(m23, file=paste0("m",23,".rds"))






#### m24 multinomial sel re ####
sel_re=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("iid","none","none","none"), 
            initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1),
                              c(1,1,1,1,1,1,1,1,1,1),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(7:9),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

sel_re4=list(model=c("logistic",
                     "age-specific",
                     "age-specific",
                     "age-specific"), 
             re = c("iid","none","none","none"), 
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

input_24 <- prepare_wham_input(y, 
                               selectivity=sel_re4,
                               recruit_model=2,
                               #age_comp = "logistic-normal-miss0",
                               #NAA_re = list(sigma="rec", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m24 <- fit_wham(input_24, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m24)

setwd("~/Mackerel/runs/run23")
plot_wham_output(m23)
saveRDS(m23, file=paste0("m",23,".rds"))



sel_re3=list(model=c("age-specific",
                     "age-specific",
                     "age-specific",
                     "age-specific"), 
             re = c("iid","none","none","none"), 
             initial_pars=list(c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
                               c(1,1,1,1,1,1,1,1,1,1),
                               c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                               c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
             fix_pars = list(
               c(4:10),
               c(1:10),
               c(1:3,7:10),
               c(1:3,7:10))
) 

input_25 <- prepare_wham_input(y, 
                               selectivity=sel_re3,
                               recruit_model=3,
                               age_comp = "logistic-normal-ar1-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1",
                                             N1_model = 1)
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)
#NAA_re$N1_model = 1

# run model
m25 <- fit_wham(input_25, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m25)

setwd("~/Mackerel/runs/run25")
plot_wham_output(m25)
saveRDS(m25, file=paste0("m",25,".rds"))



#### m25 logistic-normal random walk rec + iid NAA ####
input_25 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model = 1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m25 <- fit_wham(input_25, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m25)

# does not converge

setwd("~/Mackerel/runs/run12")
plot_wham_output(m12)
saveRDS(m11, file=paste0("m",12,".rds"))




#### m26 logistic-normal random walk rec + 2dar1 NAA ####
input_26 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m26 <- fit_wham(input_26, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m26)
mohns_rho(m26)

# age-comp residuals go to zero! 

setwd("~/Mackerel/runs/run26")
plot_wham_output(m26)
saveRDS(m26, file=paste0("m",26,".rds"))

#### m27 logistic-normal random walk rec + ar1_a NAA ####
input_27 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="ar1_a"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m27 <- fit_wham(input_27, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m27)
mohns_rho(m27)

# does not converge 

setwd("~/Mackerel/runs/run26")
plot_wham_output(m26)
saveRDS(m26, file=paste0("m",26,".rds"))






#### m28 logistic-normal random walk rec + ar1_y NAA + age based commercial sel ####
sel_2=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("none","none","none","none"), 
            initial_pars=list(c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
                              c(1,1,1,1,1,1,1,1,1,1),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(4:10),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_28 <- prepare_wham_input(y, 
                               selectivity=sel_2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m28 <- fit_wham(input_28, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m28)
mohns_rho(m28)

setwd("~/Mackerel/runs/run28")
plot_wham_output(m28)
saveRDS(m28, file=paste0("m",28,".rds"))

#### m29 logistic-normal random walk rec + ar1_a NAA + age based commercial sel ####
input_29 <- prepare_wham_input(y, 
                               selectivity=sel_2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="ar1_a"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m29 <- fit_wham(input_29, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m29)
mohns_rho(m29)

# does not converge

setwd("~/Mackerel/runs/run29")
plot_wham_output(m29)
saveRDS(m29, file=paste0("m",29,".rds"))





#### m30 logistic-normal random walk rec + iid NAA + age sel ####

sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
          re = c("ar1","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(NULL),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_30 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_30$map$logit_selpars),4)
temp[1,c(1:4,6:10)] <-1 
temp[1,c(5)] <- NA
input_30$map$logit_selpars <- factor(temp)

# run model
m31 <- fit_wham(input_30, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m31)

# does not converge

setwd("~/Mackerel/runs/run30")
plot_wham_output(m30)
saveRDS(m30, file=paste0("m",30,".rds"))







#### m31 logistic-normal random walk rec + iid NAA + age sel ####

sel=list(model=c("logistic",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("iid","none","none","none"), 
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


input_31 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m31 <- fit_wham(input_31, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m31)

# does not converge

setwd("~/Mackerel/runs/run31")
plot_wham_output(m31)
saveRDS(m31, file=paste0("m",31,".rds"))




#### m32 logistic-normal random walk rec + M ar1_y + age sel ####
sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("none","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,1,1,1,1,1,1,1),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(4:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
)


input_32 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               #NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               M = list(model="constant", re="ar1_y")
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m32 <- fit_wham(input_32, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m32)

# does not converge

setwd("~/Mackerel/runs/run32")
plot_wham_output(m32)
saveRDS(m32, file=paste0("m",32,".rds"))



#### m33 logistic-normal random walk rec + iid NAA + age sel 2dar1 ####
sel_re=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("2dar1","none","none","none"), 
         initial_pars=list(
           c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
           #c(2,0.3),
           c(1,1,1,1,1,1,1,1,1,1),
           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(8:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 

input_33 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m33 <- fit_wham(input_33, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m33)
mohns_rho(m33)

setwd("~/Mackerel/runs/run33")
plot_wham_output(m33)
saveRDS(m33, file=paste0("m",33,".rds"))


#### m34 logistic-normal random walk rec + iid NAA + age sel ar1 ####
sel_re=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1","none","none","none"), 
            initial_pars=list(
              c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
              #c(2,0.3),
              c(1,1,1,1,1,1,1,1,1,1),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(6),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_34 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_34$map$logit_selpars),4)
temp[1,c(1:5,7:10)] <- 1
temp[1,c(6)] <- NA
input_34$map$logit_selpars <- factor(temp)

# run model
m34 <- fit_wham(input_34, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m34)
mohns_rho(m34)

setwd("~/Mackerel/runs/run34")
plot_wham_output(m34)
saveRDS(m34, file=paste0("m",34,".rds"))


#### m35 logistic-normal random walk rec + iid NAA + age sel ar1y ####
sel_re=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1_y","none","none","none"), 
            initial_pars=list(
              c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
              #c(2,0.3),
              c(1,1,1,1,1,1,1,1,1,1),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(7:10),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_35 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)


# run model
m35 <- fit_wham(input_35, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m35)
mohns_rho(m35)

setwd("~/Mackerel/runs/run35")
plot_wham_output(m35)
saveRDS(m35, file=paste0("m",35,".rds"))



#### m36 logistic-normal random walk rec + iid NAA + age sel ar1 + iid q####
sel_re=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1","none","none","none"), 
            initial_pars=list(
              c(0.5, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1),
              #c(2,0.3),
              c(1,1,1,1,1,1,1,1,1,1),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(6),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

q = list(re = c("none", "iid","iid")) 

input_36 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_36$map$logit_selpars),4)
temp[1,c(1:5,7:10)] <- 1
temp[1,c(6)] <- NA
input_36$map$logit_selpars <- factor(temp)

# run model
m36 <- fit_wham(input_36, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m36)
mohns_rho(m36)

setwd("~/Mackerel/runs/run36")
plot_wham_output(m36)
saveRDS(m36, file=paste0("m",36,".rds"))




#### test ####

sel_re=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1","none","none","none"), 
            initial_pars=list(
              c(0.5, 0.5, 0.5, 0.5, 0.5, 1, 0.5, 0.5, 0.5, 0.5),
              #c(2,0.3),
              c(1,1,1,1,1,1,1,1,1,1),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(6),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_34 <- prepare_wham_input(y, 
                               selectivity=sel_re,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               #NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_34$map$logit_selpars),4)
temp[1,c(1:5,7:10)] <- 1
temp[1,c(6)] <- NA
input_34$map$logit_selpars <- factor(temp)

# run model
mT <- fit_wham(input_34, do.osa = F, do.retro = F)#, do.fit = F)
check_convergence(mT)
mohns_rho(mT)

setwd("~/Mackerel/runs/T")
plot_wham_output(mT)
saveRDS(mT, file=paste0("m",1,".rds"))


#### NAA 2dar1 does not converge####
sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("ar1","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(6),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_37 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_37$map$logit_selpars),4)
temp[1,c(1:5,7:10)] <-1 
temp[1,c(6)] <- NA
input_37$map$logit_selpars <- factor(temp)

# run model
m37 <- fit_wham(input_37, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m37)

# does not converge

setwd("~/Mackerel/runs/run37")
plot_wham_output(m37)
saveRDS(m37, file=paste0("m",37,".rds"))

#### run 37 ####

sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("ar1","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(6),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_37 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_37$map$logit_selpars),4)
temp[1,c(1:5, 7:10)] <-1 
temp[1,c(6)] <- NA
input_37$map$logit_selpars <- factor(temp)

# run model
m37 <- fit_wham(input_37, do.osa = F, do.retro = F)#, do.fit = F)
check_convergence(m37)

setwd("~/Mackerel/runs/run37")
plot_wham_output(m37)
saveRDS(m37, file=paste0("m",37,".rds"))

#### run 38: NAA ar1_a   converge####
sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("ar1","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,1,0.5,0.5,0.5,0.5),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(6),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_38 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_a"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_38$map$logit_selpars),4)
temp[1,c(1:5,7:10)] <-1 
temp[1,c(6)] <- NA
input_38$map$logit_selpars <- factor(temp)

# run model
m38 <- fit_wham(input_38, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m38)

# does not converge

setwd("~/Mackerel/runs/run38")
plot_wham_output(m38)
saveRDS(m38, file=paste0("m",38,".rds"))


#### run 39 ar1_y ####

sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("ar1","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,0.5,0.5,1,1,1,1),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(7:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_39 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_39$map$logit_selpars),4)
temp[1,c(1:5)] <-1 
temp[1,c(6)] <- NA
input_39$map$logit_selpars <- factor(temp)

# run model
m39 <- fit_wham(input_39, do.osa = F, do.retro = F)#, do.fit = F)
check_convergence(m39)

setwd("~/Mackerel/runs/run39")
plot_wham_output(m39)
saveRDS(m39, file=paste0("m",39,".rds"))


#### run 40 2dar1 ####

sel=list(model=c("age-specific",
                 "age-specific",
                 "age-specific",
                 "age-specific"), 
         re = c("none","none","none","none"), 
         initial_pars=list(c(0.5,0.5,0.5,0.5,1,1,1,1,1,1),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(7:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10))
) 


input_40 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

temp <- matrix(as.integer(input_40$map$logit_selpars),4)
temp[1,c(1:5)] <-1 
temp[1,c(6)] <- NA
input_40$map$logit_selpars <- factor(temp)

# run model
m40 <- fit_wham(input_40, do.osa = F, do.retro = F)#, do.fit = F)
check_convergence(m40)

setwd("~/Mackerel/runs/run40")
plot_wham_output(m40)
saveRDS(m40, file=paste0("m",40,".rds"))

#### plots ####

# age comps
setwd("~/Mackerel/age_comps/m1")
r1 <- readRDS("~/Mackerel/age_comps/m1.rds")
r2 <- readRDS("~/Mackerel/age_comps/m5.rds")

mods<-list(r1,r2)

compare_wham_models(mods)

# rec
setwd("~/Mackerel/runs")
#r5<- readRDS("~/Mackerel/runs/m5.rds")

r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run4/m4.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run5/m5.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run6/m6.rds")

mods<-list(r1,r2,r3,r4)

compare_wham_models(mods)

####### tv sel
r0 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run8/m8.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run9/m9.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run10/m10.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run11/m11.rds")
r5 <- readRDS("~/Mackerel/runs/T/m1.rds")

mods<-list(r0,r1,r2,r3,r4,r5)

compare_wham_models(mods)

### NAA
r1 <- readRDS("~/Mackerel/runs/run37/m37.rds")
r2 <- readRDS("~/Mackerel/runs/run38/m38.rds")
r3 <- readRDS("~/Mackerel/runs/run39/m39.rds")
r4 <- readRDS("~/Mackerel/runs/run40/m40.rds")

check_convergence(r3)


mods<-list(m1=r1,m2=r2, m3=r3,m4=r4, m5=r5)

compare_wham_models(mods)







#### m41: NAA iid + ar1_y sel ####
input_41 <- prepare_wham_input(y, 
                               selectivity=sel_re2,
                               recruit_model= 1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m41 <- fit_wham(input_41, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m41)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run41")
plot_wham_output(m41)
saveRDS(m41, file=paste0("m",41,".rds"))

#### m42: NAA 2dar1 + ar1_y sel ####
input_42 <- prepare_wham_input(y, 
                               selectivity=sel_re2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="2dar1"),
                               #basic_info = basic_info
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

basic_info <- list(simulate_process_error = rep(FALSE, 5))

input <- prepare_wham_input(asap3, model_name="insert model name", basic_info = basic_info)

# run model
m42 <- fit_wham(input_42, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m42)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run42")
plot_wham_output(m42)
saveRDS(m42, file=paste0("m",42,".rds"))


#### m43: NAA ar1_a + ar1_y sel ####
input_43 <- prepare_wham_input(y, 
                               selectivity=sel_re2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_a"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m43 <- fit_wham(input_43, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m43)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run43")
plot_wham_output(m43)
saveRDS(m43, file=paste0("m",43,".rds"))


#### m44: NAA ar1_y + ar1_y sel ####
input_44 <- prepare_wham_input(y, 
                               selectivity=sel_re2,
                               recruit_model=1,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(N1_model =1,sigma="rec+1", cor ="ar1_y"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m44 <- fit_wham(input_44, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m44)

setwd("~/GitHub/Mack.WHAM.2021MT/Runs/run44")
plot_wham_output(m44)
saveRDS(m44, file=paste0("m",44,".rds"))

#r0 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run3/m3.rds")
r1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run41/m41.rds")
r2 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run42/m42.rds")
r3 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run43/m43.rds")
r4 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run44/m44.rds")

mods<-list(r1,r2,r3,r4)
compare_wham_models(mods)



