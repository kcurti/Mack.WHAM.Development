
# load in vpa
library(readr)
asap <- read_csv("~/GitHub/Mack.WHAM.2021MT/2023.MT.ASAP/ASAP.files/ASAP_summary_Run9.MCMC.csv")
proj1 <- readRDS("~/GitHub/Mack.WHAM.2021MT/Runs/run1/m1.rds")


alpha = 0.05

years_full <- proj1$years_full
years <- proj1$years
tcol <- col2rgb('black')
tcol <- paste(rgb(tcol[1,],tcol[2,], tcol[3,], maxColorValue = 255), "55", sep = '')
if(class(proj1$sdrep)[1] == "sdreport"){
  std = summary(proj1$sdrep)
} else {
  std = proj1$sdrep
}
#par(mfrow=c(2,1), mar=c(1,1,1,1), oma = c(4,4,0,0))

ssb.ind <- which(rownames(std) == "log_SSB")
log.ssb <- std[ssb.ind,1]
ssb = exp(log.ssb)/1000
ssb.cv <- std[ssb.ind,2]
log.ssb.ci <- log.ssb + cbind(qnorm(1-alpha/2)*ssb.cv, -qnorm(1-alpha/2)*ssb.cv)
ssb.ci = exp(log.ssb.ci)/1000
no.ssb.ci <- all(is.na(ssb.ci))

res = 72
png("asap_wham_final.png",width=10,height=10,units="in",res=res)
par(mfrow=c(2,1), mar=c(1,1,1,1), oma = c(4,4,0,0))

plot(years_full, ssb, type='l', lwd=2, xlab="", ylab="", ylim=c(0,max(ssb.ci)), axes = FALSE)
axis(1, labels = FALSE)
axis(2)
box()
mtext(side = 2, "SSB (kmt)", outer = FALSE, line = 3)
grid(col = gray(0.7))
polygon(c(years_full,rev(years_full)), c(ssb.ci[,1],rev(ssb.ci[,2])), col = tcol, border = tcol, lwd = 1)
lines(asap$Year,(asap$SSB/1000), col ="red", lwd =2)
lines(asap$Year,(asap$SSB_95_hi/1000), col ="red", lwd =2, lty = 2)
lines(asap$Year,(asap$SSB_95_lo/1000), col ="red", lwd =2, lty =2)


#axis(1)

mod = proj1


n_ages = mod$env$data$n_ages
faa.ind <- which(rownames(std) == "log_FAA_tot")
log.faa <- matrix(std[faa.ind,1], length(years_full), n_ages)
faa.cv <- matrix(std[faa.ind,2], length(years_full), n_ages)
age.full.f <- apply(log.faa,1, function(x) max(which(x == max(x))))
full.f.ind = cbind(1:length(years_full), age.full.f)
#full.f.ind <- c(age.full.f[1], n_ages + cumsum(age.full.f[-1]))
log.full.f <- log.faa[full.f.ind]
full.f.cv <- faa.cv[full.f.ind]
log.f.ci <- log.full.f + cbind(qnorm(1-alpha/2)*full.f.cv, -qnorm(1-alpha/2)*full.f.cv)
full.f = exp(log.full.f)
no.f.ci <- all(is.na(log.f.ci))  
plot(years_full, full.f, type='l', lwd=2, col='black', xlab="", ylab="", ylim=c(0,max(exp(log.f.ci))), axes = FALSE)
axis(1)
axis(2)
box()
mtext(side = 1, "Year", outer = FALSE, line = 3)
mtext(side = 2, "Fully-selected F", outer = FALSE, line = 3)
grid(col = gray(0.7))
polygon(c(years_full,rev(years_full)), exp(c(log.f.ci[,1],rev(log.f.ci[,2]))), col = tcol, border = tcol, lwd = 1) 
lines(years,(asap$Freport), col ="red", lwd =2)
lines(years,(asap$Freport_95_lo), col ="red", lwd =2, lty =2)
lines(years,(asap$Freport_95_hi), col ="red", lwd =2, lty =2)

dev.off()


# stop here

age1_wham<-proj1$rep$pred_NAA[,1]



if(length(years_full) > length(years)) abline(v=tail(years,1), lty=2, lwd=1)
# F trend
n_ages = mod$env$data$n_ages
faa.ind <- which(rownames(std) == "log_FAA_tot")
log.faa <- matrix(std[faa.ind,1], length(years_full), n_ages)
faa.cv <- matrix(std[faa.ind,2], length(years_full), n_ages)
age.full.f <- apply(log.faa,1, function(x) max(which(x == max(x))))
full.f.ind = cbind(1:length(years_full), age.full.f)
#full.f.ind <- c(age.full.f[1], n_ages + cumsum(age.full.f[-1]))
log.full.f <- log.faa[full.f.ind]
full.f.cv <- faa.cv[full.f.ind]
log.f.ci <- log.full.f + cbind(qnorm(1-alpha/2)*full.f.cv, -qnorm(1-alpha/2)*full.f.cv)
full.f = exp(log.full.f)
no.f.ci <- all(is.na(log.f.ci))
if(!no.f.ci){ # have CI
  plot(years_full, full.f, type='l', lwd=2, col='black', xlab="", ylab="", ylim=c(0,max(exp(log.f.ci))), axes = FALSE)
  axis(1)
  axis(2)
  box()
  mtext(side = 1, "Year", outer = FALSE, line = 3)
  mtext(side = 2, "Fully-selected F", outer = FALSE, line = 3)
  grid(col = gray(0.7))
  lines(yearsP-1,(f), col ="red", lwd =1)
  polygon(c(years_full,rev(years_full)), exp(c(log.f.ci[,1],rev(log.f.ci[,2]))), col = tcol, border = tcol, lwd = 1)
} else { # CI all NA
  plot(years_full, full.f, type='l', lwd=2, col='black', xlab="", ylab="", ylim=c(0,max(full.f)), axes = FALSE)
  axis(1)
  axis(2)
  box()
  mtext(side = 1, "Year", outer = FALSE, line = 3)
  mtext(side = 2, "Fully-selected F", outer = FALSE, line = 3)
  grid(col = gray(0.7))
  # polygon(c(years,rev(years)), exp(c(log.f.ci[,1],rev(log.f.ci[,2]))), col = tcol, border = tcol, lwd = 1)
}
if(length(years_full) > length(years)) abline(v=tail(years,1), lty=2, lwd=1)
par(origpar)
}  #end function

plot.SSB.F.trend(mod = m14)


