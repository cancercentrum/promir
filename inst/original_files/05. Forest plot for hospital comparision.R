rm(list = ls(all = TRUE)); gc()
#
#  Forestplot for hospital comparison
#
#
library(dplyr)
library(corrplot)

source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/01. the danish_eq5d.R')
source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/functions.R')
load("Z:\\SHPR\\SZILARD\\People\\Szilard\\Improvement Index\\Data\\promdata2009.RData")

promData$eq5d0de <- with(promData, eqDanish(eq10, eq20, eq30, eq40, eq50))
promData$eq5d1de <- with(promData, eqDanish(eq11, eq21, eq31, eq41, eq51))

Hospitals <- names(table(promData$klinpri))
### EQ-5D

 eq5dRes <- matrix(NA, nrow = length(Hospitals), ncol = 3)
eqvasRes <- matrix(NA, nrow = length(Hospitals), ncol = 3)
colnames(eq5dRes) <- c('IR',
                       'LL',
                       'UL')
rownames(eq5dRes) <- Hospitals
colnames(eqvasRes) <- colnames(eq5dRes)
rownames(eqvasRes) <- rownames(eq5dRes)


 riketEQ5D <- with(promData, deltaConf(eq5d0de, eq5d1de, 0.9))
riketEQVAS <- with(promData, deltaConf(halsvas0, halsvas1, 82))

Hospitals <- names(table(promData$klinpri))

for( i in 1: length(Hospitals)){
 hospDat <- filter(promData, klinpri == Hospitals[i])
 eq5dRes[i, ] <- with(hospDat, deltaConf(eq5d0de, eq5d1de, 0.9))
eqvasRes[i,] <- with(hospDat, deltaConf(halsvas0, halsvas1, 82))
                              }
eq5dRes
eqvasRes







setwd('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Figures')

tiff(filename = "ForestPlot Hospital Comparison.tif",
     width = 24,  height = 12, units= 'cm', pointsize = 10,
     compression = "lzw", bg = "white", res = 600,
     restoreConsole = TRUE, type =  "cairo")
  nf <- layout(matrix(c(1:3),1,3,byrow = TRUE), c(8,12,12), c(14, 14, 14), FALSE)
    layout.show(nf)

par(mar=c(5, 1, 2, 0.1) + 0.1)
        plot.new()
# 1. Names Main
plot.window(ylim=c(0, 7.5), xlim=c(0, 1))
         text(0,  1, 'Falun',                        cex = 1.75, col = 'black', pos = 4)
         text(0,  2, 'Telleborg',                    cex = 1.75, col = 'black', pos = 4)
         text(0,  3, 'Enk\0x02C6ping',                     cex = 1.75, col = 'black', pos = 4)
         text(0,  4, 'Hassleholdm-Kristianstad ',    cex = 1.75, col = 'black', pos = 4)
         text(0,  5, 'Capio Ortopediska Huset',      cex = 1.75, col = 'black', pos = 4)
         text(0,  6, 'Ortho Center Stockholm',       cex = 1.75, col = 'black', pos = 4)
         text(0,  7, 'Aleris Specialv\0xC2rd Motola ',   cex = 1.75, col = 'black', pos = 4)
         


#############
# 1. EQ-5D index #
#############
plot.new()
plot.window(ylim=c(0, 7.5), xlim=c(60, 100) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
y = c(seq(0.5, 7.1, len = 10), seq(7.1, 0.5, len =10))
x = c(rep(riketEQ5D[2], 10), rep(riketEQ5D[3], 10))
polygon(x ,y , col = 'grey91', border = 'white')
segments(y0 = 0.5, y1 = 7.1, x0 = riketEQ5D[1], x1 = riketEQ5D[1], col = 'black')
for (i in 1:7) {
segments(x0 = eq5dRes[i, 'LL'], y0 = i , x1 = eq5dRes[i, 'UL'], col = 'black', lwd = 2)
points(eq5dRes[i, 'IR'], i, pch = 16, cex = 2, col = 'grey13')
}
mtext('Improvemet Ratio', side = 1, line =  3, cex = 1.25)
mtext('EQ-5D index',      side = 3, line = -1, cex = 1.25)

#############
# 1. EQ-VAS #
#############
plot.new()
plot.window(ylim=c(0, 7.5), xlim=c(50, 110) )
axis(1, lwd = 3, cex.axis = 1.5, font = 2)
y2 = c(seq(0.5, 7.1, len = 10), seq(7.1, 0.5, len =10))
x2 = c(rep(riketEQVAS[3], 10), rep(riketEQVAS[2], 10))
polygon(x2 ,y2 , col = 'grey91', border = 'white')
segments(y0 = 0.5, y1 = 7.1, x0 = riketEQVAS[1], x1 = riketEQVAS[1], col = 'black')
for (i in 1:7) {
segments(x0 = eqvasRes[i, 'LL'], y0 = i , x1 = eqvasRes[i, 'UL'], col = 'black', lwd = 2)
points(eqvasRes[i, 'IR'], i, pch = 16, cex = 2, col = 'grey13')
}
mtext('Improvemet Ratio', side = 1, line =  3, cex = 1.25)
mtext('EQ-VAS',      side = 3, line = -1, cex = 1.25)

         dev.off()

