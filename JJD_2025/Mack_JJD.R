#devtools::install_github("lichengxue/whamMSE", dependencies=TRUE)
#devtools::install_github("timjmiller/wham", dependencies=TRUE)
#devtools::install_github("timjmiller/wham", dependencies=TRUE,ref="devel")
library(wham)
setwd("X:/Mack/2025 MT")
#setwd("/home/jderoba/Herring/Mack/2025 MT")

#Run 0 just re-run Run2 and start from scratch.
sub.dir = "Run0"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

run2kc=readRDS(file.path(getwd(),"run2","run2.rds"))
input=run2kc$input

run0=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run0,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
#run0$badpar
#run0$parList$logit_selpars
#run0$rep$selpars
#age-5 selectivity in block 1 (fleet) is on a bound of 1.0. Next run, fix it.
#also probably why KC was seeing difference in F from ASAP.
#initial age-10 abundance is suspiciously low too.

######################################
#Run1 fix age-5 block 1 at 1. and then check age-10 initN
sub.dir = "Run1"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

sel=list(fix_pars=list(5:10,1:10,c(1:3,7:10),c(1:3,9:10)))
input=set_selectivity(input,selectivity = sel)
#input$par$logit_selpars[,]=Inf #set this Inf i.e., 1
run1=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run1,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
#saveRDS(run1,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
#interestingly, now age-4 is on a bound of 1.0. So lets do it again.
#comp=compare_wham_models(list("run0"=run0,"run1"=run1),do.table=T,do.plot=F,calc.aic = T)

######################################
#Run2 fix age-4 block 1 at 1. and then check age-10 initN
sub.dir = "Run2"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)

sel=list(fix_pars=list(4:10,1:10,c(1:3,7:10),c(1:3,9:10)))
input=set_selectivity(input,selectivity = sel)
#input$par$logit_selpars[,]=Inf #set this Inf i.e., 1
run2=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run2,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
run2$opt$objective
saveRDS(run2,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
#comp=compare_wham_models(list("run0"=run0,"run1"=run1),do.table=T,do.plot=F,calc.aic = T)
#check initN age-10 sd
#run2=readRDS(paste0(getwd(),"/Run2/Run2.rds"))
locale=which(run2$sdrep$value==run2$parList$log_N1[10])
initN_age10sd=run2$sdrep$sd[locale]
upper95ci=exp(run2$parList$log_N1[10]+(1.96*initN_age10sd))
lower95ci=exp(run2$parList$log_N1[10]-(1.96*initN_age10sd))
#CI is huge. Try initN as iid_re


######################################
#Run3 initN as iidre
sub.dir = "Run3"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run2$input
newnaa=list(N1_model="iid-re")
input=set_NAA(input,newnaa)
run3=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run3,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
#saveRDS(run3,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
#comp=compare_wham_models(list("run2"=run2,"run3"=run3),do.table=T,do.plot=F,calc.aic = T)
#run3=readRDS(paste0(getwd(),"/Run3/Run3.rds"))
run3$parList$N1_repars
exp(run3$parList$N1_repars[1])

#This provided identical fits to data and differ in likelihood only because the re contribute to the 
#likelihood in run3 so fit gets worse. Seems likely to be more stable

######################################
#Run4 add iid naa re
sub.dir = "Run4"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run3$input
newnaa=list(sigma="rec+1",recruit_model=2,cor="iid",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run4=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run4,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
#saveRDS(run4,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run3"=run3,"run4"=run4),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run4"))

######################################
#Run5 do run with only rec as re to make opt$objective more directly comparable.
sub.dir = "Run5"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run3$input
newnaa=list(sigma="rec",recruit_model=2,cor="iid",N1_model="iid-re")
input=set_NAA(input,newnaa)
run5=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run5,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
#saveRDS(run5,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run4"=run4,"run5"=run5),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run5"))

#had parm on bounds and no hessian. So fix block 1 age-3

######################################
#Run6 fix block 1 age-3. rec only
sub.dir = "Run6"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run5$input
sel=list(fix_pars=list(3:10,1:10,c(1:3,7:10),c(1:3,9:10)))
input=set_selectivity(input,selectivity = sel)
input$par$logit_selpars[1,3]=Inf #set this Inf i.e., 1
run6=fit_wham(input,do.osa=F,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run6,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run6,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))

######################################
#Run7 add naa re
sub.dir = "Run7"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run6$input
newnaa=list(sigma="rec+1",recruit_model=2,cor="iid",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run7=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run7,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run7,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run6"=run6,"run7"=run7),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run7"))
#improves fit based on likelihood and AIC, but pragmatically no difference. NAA sigma low. Doing little, even to retro.
######################################
#Run8 use logistic normal age comp likeli; change ess to 1 so sigmas are "interpretable".
sub.dir = "Run8"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run7$input
input$data$index_Neff=ifelse(input$data$index_Neff>0,1,input$data$index_Neff)
input$data$catch_Neff=ifelse(input$data$catch_Neff>0,1,input$data$catch_Neff)
input=set_age_comp(input,age_comp="logistic-normal-miss0")
run8=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run8,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run8,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run7"=run7,"run8"=run8),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run8"))
#not sure AIC is comparable here. NAA sigma much bigger than run7.
#OSA look better to my eye, but stats not so telling. Standard residuals for catch near perfect while BTS is garbage because
#model estimates log-normal sd huge for survey; basically saying ignore the survey age comp.
######################################
#Run9 use logistic normal age comp likeli but get rid of naa re.
sub.dir = "Run9"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run8$input
newnaa=list(sigma="rec",recruit_model=2,cor="iid",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run9=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run9,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run9,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run8"=run8,"run9"=run9),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run9"))
#much worse fit and retro. Odd that this doesn't hold true for multinomial.
#fit to fleet comp gets way worse and sd of log-norm comp way higher.
######################################
#Run10 Start run 8 and turn on estimation of log sd of observations of spring bts
sub.dir = "Run10"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run8$input
input$map$log_index_sig_scale <- factor(c(NA,1,2))
run10=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run10,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run10,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run8"=run8,"run10"=run10),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run10"))
#improved fit by ~28 aic units and best fit of those so far.
#Overall no real impact. Model is just basically ignoring all spring survey info.


######################################
#Run11 Start run 8 and add iid re on q
sub.dir = "Run11"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run8$input
catchability=list(re=c("none","iid","iid"))
input=set_q(input, catchability=catchability)
run11=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run11,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run11,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run8"=run8,"run11"=run11),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run11"))
#made residuals smaller, but really didn't do much. Mess with selectivity a bit next.

######################################
#Run12 Start run 8 and add iid re on spring selectivity
sub.dir = "Run12"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run8$input
select=list(re=c("none","none","iid","iid"))
input=set_selectivity(input, selectivity=select)
run12=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run12,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run12,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run8"=run8,"run12"=run12),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run12"))
#DNC and huge gradient, no hessian. 

######################################
#Run13 start run10 and add log-norm ar1 corr miss0 and 2dar1 naa corr to match SSRT run14; just a check
sub.dir = "Run13"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run10$input
input=set_age_comp(input,age_comp="logistic-normal-ar1-miss0")
newnaa=list(sigma="rec+1",recruit_model=2,cor="2dar1",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run13=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run13,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run13,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run10"=run10,"run13"=run13),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run13"))
#correlation across year and variance of naa on bounds. Not usable.
#but correlation among ages and using ar1 log-norm likelihood seem worth a shot. one more run

######################################
#Run14 start run10 and add log-norm ar1 corr miss0 and ar_a naa corr
sub.dir = "Run14"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run10$input
input=set_age_comp(input,age_comp="logistic-normal-ar1-miss0")
newnaa=list(sigma="rec+1",recruit_model=2,cor="ar1_a",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run14=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run14,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run14,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run10"=run10,"run14"=run14),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run14"))
#naa cor parameter across age is negative and not different from 0. So back to iid. retain ar1 of log-norm likelih

######################################
#Run15 start run10 and add log-norm ar1 corr miss0
sub.dir = "Run15"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run10$input
input=set_age_comp(input,age_comp="logistic-normal-ar1-miss0")
run15=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run15,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run15,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run10"=run10,"run15"=run15),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run15"))
#193 AIC units better than run10 but naa sigma way reduced and not doing much. Try one more without naa

######################################
#Run16 start run15 and get rid of naa
sub.dir = "Run16"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run15$input
newnaa=list(sigma="rec",recruit_model=2,cor="iid",N1_model="iid-re",decouple_recruitment=TRUE)
input=set_NAA(input,newnaa)
run16=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run16,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run16,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run15"=run15,"run16"=run16),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run16"))
#22 AIC units worse than run15. pragmatically little difference.


#####################################
#Run 17 - start run 10 and estimate sd of egg index just to see what model chooses among all index data
sub.dir = "Run17"
dir.create(file.path(getwd(), sub.dir), recursive = TRUE)
input=run10$input
input$map$log_index_sig_scale <- factor(c(1,2,3))
run17=fit_wham(input,do.osa=T,do.sdrep=T,do.retro=T,do.check = T)
plot_wham_output(mod=run17,dir.main=(file.path(getwd(), sub.dir)),out.type="html")
saveRDS(run17,file=paste(getwd(),sub.dir,paste0(sub.dir,".rds"),sep="/"))
comp=compare_wham_models(list("run10"=run10,"run17"=run17),do.table=T,do.plot=T,calc.aic = T,fdir=paste0(getwd(),"/Run17"))
#This does almost nothing and is not worth estimating.

######################################
#I'd probably go with Run10; maybe Run8 or run15. Can tinker with some other settings, but this is 
# a good basic start. Good news is that all of these tweaks don't matter and stuff is stable.
#do you want to fit fleet comp perfectly (run10) and have naa do something, or less perfect and
#naa do nothing (run 15 or 16)
######################################
#read in some models.
modnames=paste0("Run",c(6:11,15:17))
mod.list <- file.path(paste(paste(getwd(),modnames,sep="/"),paste0(modnames,".rds"),sep="/"))
mods <- lapply(mod.list, readRDS)
names(mods)=modnames
compare_wham_models(mods=mods,do.table=T,do.plot=T,calc.aic=T,calc.rho=T)
#run10=readRDS(paste0(getwd(),"/Run10/Run10.rds"))


##########get OSA stats for a few mods
#function to source all the WHAM functions from the Collaborations Repo Clone on my machine
source("/home/jderoba/Herring/Herring/2025 Assessment RT/Assessments/WHAM/GetWHAMrepofxns.R")

OSAstats=c()
for(m in 2:length(modnames)){ #first element of mods doesn't have osa turned on
  tempstats=getOSAstats(mods[[m]])
  tempstats$Model=modnames[m]
  OSAstats=rbind(tempstats,OSAstats)
}
Sig_N=OSAstats[(OSAstats$pval_agg < 0.05 | OSAstats$pval_age < 0.05) & !is.na(OSAstats$pval_age),c("Fleet","Model","pval_agg","pval_age")]
HiSD=OSAstats[(OSAstats$OSAsd_age>1 | OSAstats$OSAsd_agg>1) & !is.na(OSAstats$pval_age),c("Fleet","Model","OSAsd_agg","OSAsd_age")]
write.csv(OSAstats,file=paste0(getwd(),"/OSAstats.csv"),append=F,quote=F,row.names=F)


####################################################################
#state-space RT runs that Alex et al. did
foldnames=list.files(paste(getwd(),"Runs_SSRT",sep="/"),pattern="run")
foldnames=foldnames[!(foldnames %in% c("run_notes.docx","run20","run21","run29","run31",
                                       "run33"))]
modnames=gsub(pattern="run","m",foldnames)

mod.list <- file.path(paste(paste(getwd(),"Runs_SSRT",foldnames,sep="/"),paste0(modnames,".rds"),sep="/"))
mods <- lapply(mod.list, readRDS)
names(mods)=foldnames

get.nll=function(mod=NULL) {
 nll=mod$opt$objective
 return(nll)
}

nlls=lapply(mods,get.nll)
names(nlls)=foldnames
nlls=unlist(nlls)
sort(nlls) #run14 is by far the best nll although some of these probably aren't comparable.
##The word doc describing how each run is setup is not correct
plot_wham_output(mods$run14,dir.main=paste(getwd(),"Runs_SSRT","run14",sep="/"))
