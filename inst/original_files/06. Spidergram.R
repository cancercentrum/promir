rm(list = ls(all = TRUE)); gc()
#
#  Spidergram for concomitent comparison
#
#
library(fmsb)


source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/01. the danish_eq5d.R')
source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/functions.R')
load("Z:\\SHPR\\SZILARD\\People\\Szilard\\Improvement Index\\Data\\promdata2009.RData")

promData$eq5d0de <- with(promData, eqDanish(eq10, eq20, eq30, eq40, eq50))
promData$eq5d1de <- with(promData, eqDanish(eq11, eq21, eq31, eq41, eq51))



dat <- matrix(NA, ncol = 7, nrow = 5)
Hospitals <- names(table(promData$klinpri))
colnames(dat) <- Hospitals
rownames(dat) <- c('max', 'min', 'EQ-5D index', 'EQ-VAS', 'Pain VAS')
dat[1, ] <- 100
dat[2, ] <- 0
dat
for( i in 1: length(Hospitals)){
 hospDat <- filter(promData, klinpri == Hospitals[i])
dat['EQ-5D index', i] <- with(hospDat, ir(eq5d0de, eq5d1de,     1))
     dat['EQ-VAS', i] <- with(hospDat, ir(halsvas0, halsvas1, 100))
   dat['Pain VAS', i] <- with(hospDat, ir(smrtvas0, smrtvas1,   0))
                               }
dat <- as.data.frame(dat)
dat



setwd('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Figures')
Labels  <- c('Falun',
             'Trelleborg',
             'Enköping',
             'Hässleholm-\nKristianstad',
             'Capio Ortopediska \nHuset',
             'Ortho Center \nStockholm',
             'Aleris Specialvård Motola')


 tiff(filename = "polar.tif",
     width = 16,  height = 16, units= 'cm', pointsize = 10,
     compression = "lzw",
     bg = "white", res = 600, , restoreConsole = TRUE,
     type =  "cairo")

radarchart(dat,
           axistype = 1,
           seg = 5,
           pty = 18,
           cex = 2,
           plwd = 2,
           cglty = 1,
           cglcol = 'grey',
           axislabcol = 'black',
           centerzero = TRUE,
           vlabels = Labels,
           pcol = 'black',
           plty = c(1, 3, 6))
legend('topleft', legend = c('EQ-5D index', 'EQ VAS', 'Pain VAS'),
                         col = 'black',
                         lwd = 2,
                         lty = c(1, 3, 6),
                         bty = 'n')
  dev.off()
