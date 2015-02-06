 rm(list = ls(all = TRUE))
### Adjusted PROM expected values
#
# 2013-06-29, Szilárd Nemes
#
library(boot)
library(foreign)

## The work directory
setwd('Z:/SHPR/SZILARD/People/Szilard/Index-ci')
source('Z:/SHPR/SZILARD/People/Szilard/Index-ci/kodes/functions.r')
source('Z:/SHPR/SZILARD/People/Szilard/Index-ci/kodes/danish_eq5d.r')
## read in the data

prom.dat<- read.spss('promdat.sav',
                use.value.labels = TRUE,
                use.missings     = TRUE,
                to.data.frame    = TRUE)

prom.dat <- na.omit(prom.dat)


prom.dat$DisDat <- as.numeric(substr(prom.dat$datcpo, 1, 4))
prom.dat$eq5d0de <- with( prom.dat, eqDanish(eq10, eq20, eq30, eq40, eq50))
prom.dat$eq5d1de <- with( prom.dat, eqDanish(eq11, eq21, eq31, eq41, eq51))
head(prom.dat)
summary(prom.dat)

prom.dat <- prom.dat[prom.dat$DisDat=='2009', ]



###########
#
#   EQ-5D index
#
###########

eq <- data.frame(eq0 = prom.dat$eq5d0de, eq1 = prom.dat$eq5d1de, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eq <- eq[eq$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
res <- aggregate(eq[,c(1,2)], list( hospital = eq$hospital), mean)




### Based on averages
Hospitals <- sort(unique(eq$hospital))
     es <- vector('numeric', length(Hospitals))
    srm <- vector('numeric', length(Hospitals))
     ir <- vector('numeric', length(Hospitals))
ir.pass <- vector('numeric', length(Hospitals))
ir.mcii <- vector('numeric', length(Hospitals))

for (i in 1: length(Hospitals)){
hosp.dat <- eq[eq$hospital==Hospitals[i],]
  res$delta[i] <- with(hosp.dat, (mean(eq1)-mean(eq0)))
     res$es[i] <- with(hosp.dat, (mean(eq1)-mean(eq0))/sd(eq0))
    res$srm[i] <- with(hosp.dat, (mean(eq1)-mean(eq0))/sd(eq1-eq0))
     res$ir[i] <- with(hosp.dat, (mean(eq1)-mean(eq0))/(1-mean(eq0)))*100
res$ir.pass[i] <- with(hosp.dat, (mean(eq1)-mean(eq0))/0.9)*100
res$ir.mcii[i] <- with(hosp.dat, (mean(eq1)-mean(eq0))/0.14)*100

plot(i)
}
irEQ5D <- res




###########
#
#   EQ VAS
#
###########

eqvas <- data.frame(eqvas0 = prom.dat$halsvas0, eqvas1 = prom.dat$halsvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eqvas <- eqvas[eqvas$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
res <- aggregate(eqvas[,c(1,2)], list( hospital = eqvas$hospital), mean)




### Based on averages
Hospitals <- sort(unique(eqvas$hospital))
     es <- vector('numeric', length(Hospitals))
    srm <- vector('numeric', length(Hospitals))
     ir <- vector('numeric', length(Hospitals))
ir.pass <- vector('numeric', length(Hospitals))
ir.mcii <- vector('numeric', length(Hospitals))

for (i in 1: length(Hospitals)){
hosp.dat <- eqvas[eqvas$hospital==Hospitals[i],]
  res$delta[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0)))
     res$es[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0))/sd(eqvas0))
    res$srm[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0))/sd(eqvas1-eqvas0))
     res$ir[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0))/(100-mean(eqvas0)))*100
res$ir.pass[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0))/82)*100
res$ir.mcii[i] <- with(hosp.dat, (mean(eqvas1)-mean(eqvas0))/7)*100

plot(i)
}
irEQVAS <- res
res
colMeans(res[, c(2:9)])

###########
#
#   Pain VAS
#
###########

pain <- data.frame(smrtvas0 = prom.dat$smrtvas0, smrtvas1 = prom.dat$smrtvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
pain <- pain[pain$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
res <- aggregate(pain[,c(1,2)], list( hospital = pain$hospital), mean)




### Based on averages
Hospitals <- sort(unique(pain$hospital))
     es <- vector('numeric', length(Hospitals))
    srm <- vector('numeric', length(Hospitals))
     ir <- vector('numeric', length(Hospitals))
ir.pass <- vector('numeric', length(Hospitals))
ir.mcii <- vector('numeric', length(Hospitals))

for (i in 1: length(Hospitals)){
hosp.dat <- pain[pain$hospital==Hospitals[i],]
  res$delta[i] <- with(hosp.dat, (mean(smrtvas1)-mean(smrtvas0)))
     res$es[i] <- with(hosp.dat, (mean(smrtvas1)-mean(smrtvas0))/sd(smrtvas0))
    res$srm[i] <- with(hosp.dat, (mean(smrtvas1)-mean(smrtvas0))/sd(smrtvas1-smrtvas0))
     res$ir[i] <- with(hosp.dat, (mean(smrtvas1)-mean(smrtvas0))/(0-mean(smrtvas0)))*100

plot(i)
}
irPAIN <- res
res
colMeans(res[, c(2:7)])

ir <- NULL
 ir$eq5d <- irEQ5D$ir
ir$eqvas <- irEQVAS$ir
 ir$pain <- irPAIN$ir
 ir <- as.data.frame(ir)
 rownames(ir) <- irPAIN$hospital
 ir
 
 