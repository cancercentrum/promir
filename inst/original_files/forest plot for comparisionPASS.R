#### 2014-10-17
#
#  Forest Plot for refetenc evalue comparision
#
#


# 2013-06-29, Szilárd Nemes
#
rm(list = ls()); gc()
library(foreign)
library(boot)

## The work directory
setwd('Z:/SHPR/SZILARD/People/Szilard/Index-ci')
source('Z:/SHPR/SZILARD/People/Szilard/Index-ci/kodes/functionsEQ5D.r')
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



####################  PASS

########################## CI Functions

## Confidence Interval
# Delta
deltaConf <- function(pre, post, ref){
        y <- post - pre
        x <- ref
       IR <- (mean(post)-mean(pre))/(ref)
    sigma <- var(y)/(length(y)*ref^2)
       ci <- IR + c(-1, 1)*qnorm(0.975)*sqrt(sigma)
      return(c(IR, ci)*100)
      }
# Boot
ir.pass <-  function(Data, d, ref){
               Data <- Data[d,]
                 ir <- ((mean(Data[,2])- mean(Data[,1]))/(ref))*100
               return(ir)
                                         }

######################### EQ-VAS ################################################

eqvas <- data.frame(eqvas0 = prom.dat$halsvas0, eqvas1 = prom.dat$halsvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eqvas <- eqvas[eqvas$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]

Hospitals <- unique(eqvas$hospital)
passEQVAS <- matrix(NA, ncol = 6, nrow = 11)
rownames(passEQVAS) <- Hospitals
colnames(passEQVAS) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation
for (i in 1: length(Hospitals)){
hosp.dat <- eqvas[eqvas$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.pass , R = 1000, ref = 82)

###Best
  passEQVAS[i, 1] <- irBest$t0
passEQVAS[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
passEQVAS[i, 4:6] <- deltaConf(hosp.dat$eqvas0, hosp.dat$eqvas1, 82)

plot(i)
}

passEQVAS


######################### EQ-5D index ################################################

eq <- data.frame(eq0 = prom.dat$eq5d0, eq1 = prom.dat$eq5d1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eq <- eq[eq$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]

Hospitals <- unique(eq$hospital)
passEQ5D <- matrix(NA, ncol = 6, nrow = 11)
rownames(passEQ5D) <- Hospitals
colnames(passEQ5D) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation
for (i in 1: length(Hospitals)){
hosp.dat <- eq[eq$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.pass , R = 1000, ref = 0.9)

###Best
  passEQ5D[i, 1] <- irBest$t0
passEQ5D[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
passEQ5D[i, 4:6] <- deltaConf(hosp.dat$eq0, hosp.dat$eq1, 0.9)

plot(i)
}

passEQ5D

####################  MCII



######################### EQ-VAS ################################################

eqvas <- data.frame(eqvas0 = prom.dat$halsvas0, eqvas1 = prom.dat$halsvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eqvas <- eqvas[eqvas$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]

Hospitals <- unique(eqvas$hospital)
mciiEQVAS <- matrix(NA, ncol = 6, nrow = 11)
rownames(mciiEQVAS) <- Hospitals
colnames(mciiEQVAS) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation
for (i in 1: length(Hospitals)){
hosp.dat <- eqvas[eqvas$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.pass , R = 1000, ref = 7)

###Best
  mciiEQVAS[i, 1] <- irBest$t0
mciiEQVAS[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
mciiEQVAS[i, 4:6] <- deltaConf(hosp.dat$eqvas0, hosp.dat$eqvas1, 7)

plot(i)
}

mciiEQVAS


######################### EQ-5D index ################################################

eq <- data.frame(eq0 = prom.dat$eq5d0, eq1 = prom.dat$eq5d1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eq <- eq[eq$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]

Hospitals <- unique(eq$hospital)
mciiEQ5D <- matrix(NA, ncol = 6, nrow = 11)
rownames(mciiEQ5D) <- Hospitals
colnames(mciiEQ5D) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation
for (i in 1: length(Hospitals)){
hosp.dat <- eq[eq$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.pass , R = 1000, ref = 0.14)

###Best
  mciiEQ5D[i, 1] <- irBest$t0
mciiEQ5D[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
mciiEQ5D[i, 4:6] <- deltaConf(hosp.dat$eq0, hosp.dat$eq1, 0.14)

plot(i)
}

mciiEQ5D


setwd('Z:/SHPR/SZILARD/People/Szilard/Index-ci/September 2014/Figures')

tiff(filename = "ForestPlot_PASS_MCII.tif",
     width = 16,  height = 16, units= 'cm', pointsize = 10,
     compression = "lzw", bg = "white", res = 600,
     restoreConsole = TRUE, type =  "cairo")
  nf <- layout(matrix(c(1:6),2,3,byrow = TRUE), c(14,12,12, 14,12,12), c(20, 22, 22,20, 22, 22), TRUE)
    layout.show(nf)

par(mar=c(5, 1, 2, 0) + 0.1)
        plot.new()
# 1. Names Main
plot.window(ylim=c(0, 23), xlim=c(0, 1))
         text(0,  1, 'Falun',                    cex = 1.5, col = 'black', pos = 4)
         text(0,  3, 'Piteå',                    cex = 1.5, col = 'black', pos = 4)
         text(0,  5, 'Trelleborg',               cex = 1.5, col = 'black', pos = 4)
         text(0,  7, 'Ortho Center Stockholm ',  cex = 1.5, col = 'black', pos = 4)
         text(0,  9, 'Motola',                   cex = 1.5, col = 'black', pos = 4)
         text(0, 11, 'Capio S:t Göran',          cex = 1.5, col = 'black', pos = 4)
         text(0, 13, 'Hässleholm-Kristianstad ', cex = 1.5, col = 'black', pos = 4)
         text(0, 15, 'Bollnäs',                  cex = 1.5, col = 'black', pos = 4)
         text(0, 17, 'Capio Ortopediska Huset',  cex = 1.5, col = 'black', pos = 4)
         text(0, 19, 'Uddevalla',                cex = 1.5, col = 'black', pos = 4)
         text(0, 21, 'Falköping',               cex = 1.5, col = 'black', pos = 4)

################ PASS #############

#############
# 1. EQ-VAS #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(15, 45) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference
segments(y0 = 0.5, y1 = 22, x = 28.02066, x1 = 28.02066, col = 'grey', lwd = 2, lty = 3)

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = passEQVAS[i,'dLC'],
         x1 = passEQVAS[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = passEQVAS[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
# mtext(side = 1, line = 3.0, 'Improvement Index', cex = 1.25)
mtext(side = 3, line = .5, 'EQ VAS', cex = 1.25)
             }

#############
# 2. EQ-5D  #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(30, 55) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference
segments(y0 = 0.5, y1 = 22, x = 41.88946, x1 = 41.88946, col = 'grey', lwd = 2, lty = 3)

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = passEQ5D[i,'dLC'],
         x1 = passEQ5D[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = passEQ5D[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
mtext(side = 1, line = 3.0, 'IR PASS', cex = 1.25, adj = 0)
mtext(side = 3, line = .5, 'EQ-5D index', cex = 1.25)
             }

################ MCII #############
par(mar=c(5, 1, 2, 0) + 0.1)
        plot.new()
# 1. Names Main
plot.window(ylim=c(0, 23), xlim=c(0, 1))
         text(0,  1, 'Falun',                    cex = 1.5, col = 'black', pos = 4)
         text(0,  3, 'Piteå',                    cex = 1.5, col = 'black', pos = 4)
         text(0,  5, 'Trelleborg',               cex = 1.5, col = 'black', pos = 4)
         text(0,  7, 'Ortho Center Stockholm ',  cex = 1.5, col = 'black', pos = 4)
         text(0,  9, 'Motola',                   cex = 1.5, col = 'black', pos = 4)
         text(0, 11, 'Capio S:t Göran',          cex = 1.5, col = 'black', pos = 4)
         text(0, 13, 'Hässleholm-Kristianstad ', cex = 1.5, col = 'black', pos = 4)
         text(0, 15, 'Bollnäs',                  cex = 1.5, col = 'black', pos = 4)
         text(0, 17, 'Capio Ortopediska Huset',  cex = 1.5, col = 'black', pos = 4)
         text(0, 19, 'Uddevalla',                cex = 1.5, col = 'black', pos = 4)
         text(0, 21, 'Falköping',               cex = 1.5, col = 'black', pos = 4)

################ PASS #############

#############
# 1. EQ-VAS #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(230, 500) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference
segments(y0 = 0.5, y1 = 22, x = 328.2420, x1 = 328.2420, col = 'grey', lwd = 2, lty = 3)

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = mciiEQVAS[i,'dLC'],
         x1 = mciiEQVAS[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = mciiEQVAS[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
# mtext(side = 1, line = 3.0, 'Improvement Index', cex = 1.25)
#mtext(side = 3, line = .5, 'EQ VAS', cex = 1.25)
             }

#############
# 2. EQ-5D  #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(190, 340) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference
segments(y0 = 0.5, y1 = 22, x =  269.2894, x1 =  269.2894, col = 'grey', lwd = 2, lty = 3)

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = mciiEQ5D[i,'dLC'],
         x1 = mciiEQ5D[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = mciiEQ5D[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
mtext(side = 1, line = 3.0, 'IR MCII', cex = 1.25, adj = 0)
#mtext(side = 3, line = .5, 'EQ-5D index', cex = 1.25)
             }
         dev.off()

