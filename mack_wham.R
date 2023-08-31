# mackerel
# ASAP --> WHAM
# Summer 2023

# load packages
require(wham)
require(tidyverse)
require(here)
require(ggplot2)
####################################################
# read in 2019 ASAP file
y <- read_asap3_dat("C:/Users/alex.hansell/Downloads/RUN4.dat")


####################################################
# ASAP like run
input_ASAP <- prepare_wham_input(y, 
                                 #selectivity=sel
                                 #recruit_model=3,
                                 #age_comp = list(fleets = rep("multinomial",2), 
                                 #                indices = rep("multinomial", 2)),
                                 #NAA_re = list(sigma="rec", cor ="iid"),
                                 #catchability = q,
                                 #model_name="Run18_ASAP_like"
)


# run model
m1 <- fit_wham(input_ASAP, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m1)

# setwd (this is where the plots are save)
setwd("~/Mackerel/Base")

plot_wham_output(m1)

# Commerical selectivity is flattoped --> try logistic 

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

input_ASAP <- prepare_wham_input(y, 
                                 selectivity=sel
                                 #recruit_model=3,
                                 #age_comp = list(fleets = rep("multinomial",2), 
                                 #                indices = rep("multinomial", 2)),
                                 #NAA_re = list(sigma="rec", cor ="iid"),
                                 #catchability = q,
                                 #model_name="Run18_ASAP_like"
)

# run model
m1 <- fit_wham(input_ASAP, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m1)

#################################################
# switch to optiimal age comps - this info is from a seperate loop

#Increase ESS
y$dat$IAA_mats[[2]][42:52,14] <- rep(200,length(42:52))
y$dat$IAA_mats[[3]][7:41,14] <- rep(200,length(7:41))
y$dat$catch_Neff[,1] <- rep(200, length(y$dat$catch_Neff[,1]))

input_log <- prepare_wham_input(y, 
                                selectivity=sel,
                                #recruit_model=3,
                                age_comp = list(fleets = rep("logistic-normal-miss0",1), 
                                                indices = rep("logistic-normal-miss0", 3))
                                #NAA_re = list(sigma="rec", cor ="iid"),
                                #catchability = q,
                                #model_name="Run18_ASAP_like"
)


# run model
m2 <- fit_wham(input_log, do.osa = F, do.retro = T)#, do.fit = F)
check_convergence(m2)

# setwd
setwd("~/Mackerel/Base_log")

plot_wham_output(m2)

# define age comps for easy use in other model runs
ages = list(fleets = rep("logistic-normal-miss0",1), 
            indices = rep("logistic-normal-miss0", 3))


##################################
# ar1 selectivity
#################################
# Free fit age based
sel=list(model=rep("age-specific",4), 
         re = c("ar1","none","ar1","ar1"), 
         initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5, 0.5),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(5),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10)
         ) 
)

#q = list(re = c("none", "iid","none")) 

input_ar1_sel <- prepare_wham_input(y, 
                                    selectivity=sel,
                                    #recruit_model=3,
                                    age_comp = ages,
                                    # NAA_re =NAA_list,
                                    #NAA_re = list(sigma="rec", cor ="iid"),
                                    # catchability = q,
                                    model_name="Run18_ASAP_like")
#for ar1(age) need to fix all estimated mean parameters to be the same value
temp <- matrix(as.integer(input_ar1_sel$map$logit_selpars),4)
#temp<-NA
temp[1,c(1:4,6:10)] <- 1
temp[3,c(2:10)] <- 3
temp[4,c(2:10)] <- 4
temp[3,c(1:3,7:10)]<-NA
temp[4,c(1:3,7:10)]<-NA
input_ar1_sel$map$logit_selpars <- factor(temp)

# run model
m3 <- fit_wham(input_ar1_sel, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ar1_sel")
plot_wham_output(m3)

###################
# time varying selectivity based on loop
#######################
# Free fit age based
sel_re=list(model=c("logistic",
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

input_sel_re <- prepare_wham_input(y, 
                                   selectivity=sel_re,
                                   #recruit_model=3,
                                   age_comp = ages,
                                   # NAA_re =NAA_list,
                                   #NAA_re = list(sigma="rec", cor ="iid"),
                                   # catchability = q,
                                   model_name="Run18_ASAP_like")

m4 <- fit_wham(input_sel_re, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/sel_re_age/iid")

plot_wham_output(m4)

#######################
# lets try iid on q 

q = list(re = c("none", "iid","iid")) # define randome effects on q

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



input_q <- prepare_wham_input(y, 
                              selectivity=sel,
                              #recruit_model=3,
                              age_comp = ages,
                              #NAA_re = list(sigma="rec", cor ="iid"),
                              catchability = q,
                              model_name="Run18_ASAP_like")

# Assume equilibirum
#input3$data$N1_model=1

# run model
m5 <- fit_wham(input_q, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/iid_q")

plot_wham_output(m5)


###########################
# Ar1_y on recruitment; hopefully helps with projections
#############################

input_rec_ar1 <- prepare_wham_input(y, 
                                    selectivity=sel,
                                    #recruit_model=3,
                                    age_comp = ages,
                                    NAA_re = list(sigma="rec", cor ="ar1_y"),
                                    #catchability = q,
                                    model_name="Run18_ASAP_like")

# run model
m6 <- fit_wham(input_rec_ar1, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/rec_ar1_y")

plot_wham_output(m6)

# this is how you run projections - will save to the wd

proj <- project_wham(model=m6,proj.opts=list(use.FMSY = T))
plot_wham_output(proj)


###########################
# iid random effects on recruitment
##########################

input_ss_iid <- prepare_wham_input(y, 
                                   selectivity=sel,
                                   #recruit_model=3,
                                   age_comp = ages,
                                   NAA_re = list(sigma="rec+1", cor ="iid"),
                                   #catchability = q,
                                   model_name="Run18_ASAP_like")

# run model
m7 <- fit_wham(input_ss_iid, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ss/iid")

plot_wham_output(m7)

#############################
# Full state-sape model ar1_a
############################

input_ss_ar1 <- prepare_wham_input(y, 
                                   selectivity=sel,
                                   #recruit_model=3,
                                   age_comp = list(fleets = rep("multinomial",1), 
                                                   indices = rep("multinomial", 3)),
                                   NAA_re = list(sigma="rec+1", cor ="ar1_a"),
                                   #catchability = q,
                                   model_name="Run18_ASAP_like")

# run model
m8 <- fit_wham(input_ss_ar1, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ss/ar1_a")

plot_wham_output(m8)

#############################
#Full state-sape model ar1_y
############################

input_ss_ar1_y <- prepare_wham_input(y, 
                                     selectivity=sel,
                                     #recruit_model=3,
                                     age_comp = list(fleets = rep("multinomial",1), 
                                                     indices = rep("multinomial", 3)),
                                     NAA_re = list(sigma="rec+1", cor ="ar1_y"),
                                     #catchability = q,
                                     model_name="Run18_ASAP_like")

# run model
m9 <- fit_wham(input_ss_ar1, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ss/ar1_y")

plot_wham_output(m9)

#############################
##Full state-sape model 2dar1
############################

input_ss_2dar1 <- prepare_wham_input(y, 
                                     selectivity=sel,
                                     #recruit_model=3,
                                     age_comp = list(fleets = rep("multinomial",1), 
                                                     indices = rep("multinomial", 3)),
                                     NAA_re = list(sigma="rec+1", cor ="2dar1"),
                                     #catchability = q,
                                     model_name="Run18_ASAP_like")

# run model
m10 <- fit_wham(input_ss_2dar1, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ss/2dar1")

plot_wham_output(m10)

mods<-list(m2,m3,m4,m5,m6)

compare_wham_models(mods)


#############################
##Prefferd model run - ar1 selectivity on commercial fleet (ages 1-5); ar1_y on rec
############################

# Free fit age based
sel=list(model=rep("age-specific",4), 
         re = c("ar1","none","ar1","ar1"), 
         initial_pars=list(c(0.5, 0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0),
                           c(1,1,1,1,1,1,1,1,1,1),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0),
                           c(0, 0, 1, 0.6, 0.4, 0.2, 0, 0, 0, 0)),
         fix_pars = list(
           c(6:10),
           c(1:10),
           c(1:3,7:10),
           c(1:3,7:10)
         ) 
)

#q = list(re = c("none", "iid","none")) 

input_ar1_sel <- prepare_wham_input(y, 
                                    selectivity=sel,
                                    #recruit_model=3,
                                    age_comp = ages,
                                    # NAA_re =NAA_list,
                                    NAA_re = list(sigma="rec", cor ="ar1_y"),
                                    # catchability = q,
                                    model_name="Run18_ASAP_like")
#for ar1(age) need to fix all estimated mean parameters to be the same value
temp <- matrix(as.integer(input_ar1_sel$map$logit_selpars),4)
#temp<-NA
temp[1,c(1:5)] <- 1
temp[3,c(2:10)] <- 3
temp[4,c(2:10)] <- 4
temp[3,c(1:3,7:10)]<-NA
temp[4,c(1:3,7:10)]<-NA
input_ar1_sel$map$logit_selpars <- factor(temp)


# run model
m3 <- fit_wham(input_ar1_sel, do.osa = F, do.retro = T)#, do.fit = F)

setwd("~/Mackerel/ar1_rec")
plot_wham_output(m3)
proj <- project_wham(model=m3,proj.opts=list(use.FXPR = T))
plot_wham_output(proj)

