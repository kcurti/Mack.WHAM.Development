# mackerel
# ASAP --> WHAM
# Summer 2023

# like asap
# age comps
# recruitment
# time varying selectivity 
# time varying NAA

#### load packages & data ####
require(wham)
require(tidyverse)
require(here)
require(ggplot2)

# read in 2019 ASAP file
y <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN9.dat")

#Increase ESS
y$dat$IAA_mats[[2]][c(42:52,54:55),14] <- 200
y$dat$IAA_mats[[3]][7:41,14] <- 200
y$dat$IAA_mats[[3]][42:55,2:13]<--999
y$dat$catch_Neff[,1] <- 200

#### m1:asap like run ####
input_1 <- prepare_wham_input(y)

m1 <- fit_wham(input_1, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m1)

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

input_1 <- prepare_wham_input(y, 
                                 selectivity=sel
)

# run model
m1 <- fit_wham(input_1, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m1)



#### m2:age comps ####
input_2 <- prepare_wham_input(y, 
                                selectivity=sel,
                                #recruit_model=3,
                                age_comp = "logistic-normal-miss0",
                                #NAA_re = list(sigma="rec", cor ="iid"),
                                #catchability = q,
                                #model_name="Run18_ASAP_like"
)


# run model
m2 <- fit_wham(input_2, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m2)

setwd("~/Mackerel/runs/run2")
plot_wham_output(m2)
saveRDS(m2, file=paste0("m",2,".rds"))

#### m3: random walk rec ####
input_3 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=1,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m3 <- fit_wham(input_3, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m3)

setwd("~/Mackerel/runs/run3")
plot_wham_output(m3)
saveRDS(m3, file=paste0("m",3,".rds"))

#### m4: mean rec ar1 ####
input_4 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="ar1_y"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m4 <- fit_wham(input_4, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m4)

setwd("~/Mackerel/runs/run4")
plot_wham_output(m4)
saveRDS(m4, file=paste0("m",4,".rds"))
#### m5: mean rec iid ####
input_5 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m5 <- fit_wham(input_5, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m5)

setwd("~/Mackerel/runs/run5")
plot_wham_output(m5)
saveRDS(m5, file=paste0("m",5,".rds"))


#### m6: BH rec ####
input_6 <- prepare_wham_input(y, 
                              selectivity=sel,
                              recruit_model=3,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m6 <- fit_wham(input_6, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m6)

setwd("~/Mackerel/runs/run6")
plot_wham_output(m6)
saveRDS(m6, file=paste0("m",6,".rds"))

mods<-list(m3,m4,m5, m6)
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
            initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 1),
                              c(1,1,1,1,1,1,1,1,1,1),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(7:10),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_8 <- prepare_wham_input(y, 
                              selectivity=sel_re,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m8 <- fit_wham(input_8, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m8)

setwd("~/Mackerel/runs/run8")
plot_wham_output(m8)
saveRDS(m8, file=paste0("m",8,".rds"))


#### m9: tv ar1y commercial ages ####
sel_re2=list(model=c("age-specific",
                    "age-specific",
                    "age-specific",
                    "age-specific"), 
            re = c("ar1_y","none","none","none"), 
            initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 1),
                              c(1,1,1,1,1,1,1,1,1,1),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                              c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
            fix_pars = list(
              c(7:10),
              c(1:10),
              c(1:3,7:10),
              c(1:3,7:10))
) 

input_9 <- prepare_wham_input(y, 
                              selectivity=sel_re2,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m9 <- fit_wham(input_9, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m9)

setwd("~/Mackerel/runs/run9")
plot_wham_output(m9)
saveRDS(m9, file=paste0("m",9,".rds"))

#### m10: tv 2dar1 commercial ages ####
sel_re3=list(model=c("age-specific",
                     "age-specific",
                     "age-specific",
                     "age-specific"), 
             re = c("2dar1","none","none","none"), 
             initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1, 1),
                               c(1,1,1,1,1,1,1,1,1,1),
                               c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                               c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
             fix_pars = list(
               c(7:10),
               c(1:10),
               c(1:3,7:10),
               c(1:3,7:10))
) 

input_10 <- prepare_wham_input(y, 
                              selectivity=sel_re3,
                              recruit_model=2,
                              age_comp = "logistic-normal-miss0",
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              #catchability = q,
                              #model_name="Run18_ASAP_like"
)


# run model
m10 <- fit_wham(input_10, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m10)

setwd("~/Mackerel/runs/run10")
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
                               recruit_model=2,
                               age_comp = "logistic-normal-miss0",
                               #NAA_re = list(sigma="rec", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m11 <- fit_wham(input_11, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m11)

setwd("~/Mackerel/runs/run11")
plot_wham_output(m11)
saveRDS(m11, file=paste0("m",11,".rds"))


#### m12: NAA iid ####
input_12 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="iid"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m12 <- fit_wham(input_12, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m11)

setwd("~/Mackerel/runs/run12")
plot_wham_output(m12)
saveRDS(m11, file=paste0("m",12,".rds"))

### m13: NAA 2dar1 ####
input_13 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "logistic-normal-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m13 <- fit_wham(input_13, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m13)

setwd("~/Mackerel/runs/run13")
plot_wham_output(m13)
saveRDS(m13, file=paste0("m",13,".rds"))



#### test ####
input_14 <- prepare_wham_input(y, 
                               selectivity=sel,
                               recruit_model=2,
                               age_comp = "dirichlet-miss0",
                               NAA_re = list(sigma="rec+1", cor ="2dar1"),
                               #catchability = q,
                               #model_name="Run18_ASAP_like"
)

# run model
m14 <- fit_wham(input_14, do.osa = T, do.retro = T)#, do.fit = F)
check_convergence(m14)

setwd("~/Mackerel/runs/run14")
plot_wham_output(m14)
saveRDS(m14, file=paste0("m",14,".rds"))

