

# 2015-01-12, Szilárd Nemes
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



#################### Absolute Difference

########################## CI Functions

## Confidence Interval
# Delta
deltaConf <- function(pre, post, best){
        y <- post - pre
        x <- best - pre
       IR <- (mean(post)-mean(pre))/(best-mean(pre))
    sigma <- 1/(length(y)*mean(x)^2)*
             (IR^2*var(x)+var(y) - 2*IR*cov(x,y))
       ci <- IR + c(-1, 1)*qnorm(0.975)*sqrt(sigma)
      return(c(IR, ci)*100)
      }
# Boot
ir.best <-
function(Data, d, best){
      Data <- Data[d,]
      ir <- (mean(Data[,2])- mean(Data[,1]))/(best-mean(Data[,1]))*100
               return(ir)
                                         }

######################### EQ-VAS ################################################

eqvas <- data.frame(eqvas0 = prom.dat$halsvas0, eqvas1 = prom.dat$halsvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eqvas <- eqvas[eqvas$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
                                         
Hospitals <- unique(eqvas$hospital)
ResultsEQVAS <- matrix(NA, ncol = 6, nrow = 11)
rownames(ResultsEQVAS) <- Hospitals
colnames(ResultsEQVAS) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation 
for (i in 1: length(Hospitals)){
hosp.dat <- eqvas[eqvas$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.best , R = 1000, best =100)

###Best
  ResultsEQVAS[i, 1] <- irBest$t0
ResultsEQVAS[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
ResultsEQVAS[i, 4:6] <- deltaConf(hosp.dat$eqvas0, hosp.dat$eqvas1, 100)

plot(i)
}

ResultsEQVAS


######################### EQ-5D index ################################################

eq <- data.frame(eq0 = prom.dat$eq5d0, eq1 = prom.dat$eq5d1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
eq <- eq[eq$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
                                         
Hospitals <- unique(eq$hospital)
ResultsEQ5D <- matrix(NA, ncol = 6, nrow = 11)
rownames(ResultsEQ5D) <- Hospitals
colnames(ResultsEQ5D) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation 
for (i in 1: length(Hospitals)){
hosp.dat <- eq[eq$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.best , R = 1000, best = 1)

###Best
  ResultsEQ5D[i, 1] <- irBest$t0
ResultsEQ5D[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
ResultsEQ5D[i, 4:6] <- deltaConf(hosp.dat$eq0, hosp.dat$eq1, 1)

plot(i)
}

ResultsEQ5D


######################### Pain VAS ################################################

pain <- data.frame(pain0 = prom.dat$smrtvas0, pain1 = prom.dat$smrtvas1, hospital = factor(prom.dat$klincpo), index = as.numeric(factor(prom.dat$klincpo)))
pain <- pain[pain$hospital %in% names(which(table(prom.dat$klincpo)>250)), ]
                                         
Hospitals <- unique(pain$hospital)
ResultsPAIN <- matrix(NA, ncol = 6, nrow = 11)
rownames(ResultsPAIN) <- Hospitals
colnames(ResultsPAIN) <- c('bIR', 'bLC', 'bUC', 'dIR', 'dLC', 'dUC')


### CI Calculation 
for (i in 1: length(Hospitals)){
hosp.dat <- pain[pain$hospital==Hospitals[i],]
### Best
irBest <- boot(hosp.dat , statistic = ir.best , R = 1000, best = 0)

###Best
  ResultsPAIN[i, 1] <- irBest$t0
ResultsPAIN[i, 2:3] <- boot.ci(irBest, type='perc')$perc[4:5]
ResultsPAIN[i, 4:6] <- deltaConf(hosp.dat$pain0, hosp.dat$pain1, 0)

plot(i)
}

ResultsPAIN




setwd('Z:/SHPR/SZILARD/People/Szilard/Index-ci/September 2014/Figures')

tiff(filename = "ForestPlot.tif",
     width = 24,  height = 12, units= 'cm', pointsize = 10,
     compression = "lzw", bg = "white", res = 600,
     restoreConsole = TRUE, type =  "cairo")
  nf <- layout(matrix(c(1:4),1,4,byrow = FALSE), c(14,12, 12, 12), c(20, 20, 20, 20), TRUE)
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

#############
# 2. EQ-VAS #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(35, 65) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference              
segments(y0 = 0.5, y1 = 22, x = 50.35736, x1 = 50.35736, col = 'grey', lwd = 2, lty = 3)  

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){ 
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = ResultsEQVAS[i,'dLC'],
         x1 = ResultsEQVAS[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = ResultsEQVAS[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
# mtext(side = 1, line = 3.0, 'Improvement Index', cex = 1.25)
mtext(side = 3, line = .5, 'EQ VAS', cex = 1.25)
             }

#############
# 2. EQ-5D  #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(50, 80) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference              
segments(y0 = 0.5, y1 = 22, x = 64.7669, x1 = 64.7669, col = 'grey', lwd = 2, lty = 3)  

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){ 
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = ResultsEQ5D[i,'dLC'],
         x1 = ResultsEQ5D[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = ResultsEQ5D[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
mtext(side = 1, line = 3.0, 'IR', cex = 1.25)
mtext(side = 3, line = .5, 'EQ-5D index', cex = 1.25)
             }

#############
# 2. Pain   #
#############
plot.new()
plot.window(ylim=c(0, 23), xlim=c(70, 90) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
## general reference              
segments(y0 = 0.5, y1 = 22, x = 78.57760, x1 = 78.57760, col = 'grey', lwd = 2, lty = 3)  

## Confidence Interval
pos <- seq(1, 21, by = 2)
for (i in 1: length(pos)){ 
segments(y0 = pos[i],
         y1 = pos[i],
         x0 = ResultsPAIN[i,'dLC'],
         x1 = ResultsPAIN[i,'dUC'],
         col = 'darkgrey', lwd = 3)
## Points
points(y = pos[i], x = ResultsPAIN[i, 'dIR'], pch = 18, cex = 2.5, col = 'black')
# mtext(side = 1, line = 3.0, 'Improvement Index', cex = 1.25)
mtext(side = 3, line = .5, 'Pain VAS', cex = 1.25)
             }           
         dev.off()

