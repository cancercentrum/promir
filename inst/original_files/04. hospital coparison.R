rm(list = ls(all = TRUE))
gc()
# Differences between genders
library(dplyr)
library(corrplot)

source("Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/01. the danish_eq5d.R")
source("Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/functions.R")
load("Z:\\SHPR\\SZILARD\\People\\Szilard\\Improvement Index\\Data\\promdata2009.RData")

promData$eq5d0de <- with(promData, eqDanish(eq10, eq20, eq30, eq40, eq50))
promData$eq5d1de <- with(promData, eqDanish(eq11, eq21, eq31, eq41, eq51))

Hospitals <- names(table(promData$klinpri))
### EQ-5D

eq5dRes <- matrix(NA, nrow = length(Hospitals), ncol = 6)
eqvasRes <- matrix(NA, nrow = length(Hospitals), ncol = 6)
colnames(eq5dRes) <- c("Pre-op", "Post-op", "Improvement Ratio", "Change Score", "Cohen effect size", "Stand Resp Mean")
rownames(eq5dRes) <- Hospitals
colnames(eqvasRes) <- colnames(eq5dRes)
rownames(eqvasRes) <- rownames(eq5dRes)


for (i in 1:length(Hospitals)) {
    hospDat <- filter(promData, klinpri == Hospitals[i])
    ## EQ-5D
    eq5dRes[i, "Pre-op"] <- with(hospDat, mean(eq5d0de))
    eq5dRes[i, "Post-op"] <- with(hospDat, mean(eq5d1de))
    eq5dRes[i, "Improvement Ratio"] <- with(hospDat, ir(eq5d0de, eq5d1de, 0.9))
    eq5dRes[i, "Change Score"] <- with(hospDat, mean(eq5d1de - eq5d0de))
    eq5dRes[i, "Cohen effect size"] <- with(hospDat, es(eq5d0de, eq5d1de))
    eq5dRes[i, "Stand Resp Mean"] <- with(hospDat, srm(eq5d0de, eq5d1de))
    
    ## EQ-VAS
    eqvasRes[i, "Pre-op"] <- with(hospDat, mean(halsvas0))
    eqvasRes[i, "Post-op"] <- with(hospDat, mean(halsvas1))
    eqvasRes[i, "Improvement Ratio"] <- with(hospDat, ir(halsvas0, halsvas1, 82))
    eqvasRes[i, "Change Score"] <- with(hospDat, mean(halsvas1 - halsvas0))
    eqvasRes[i, "Cohen effect size"] <- with(hospDat, es(halsvas0, halsvas1))
    eqvasRes[i, "Stand Resp Mean"] <- with(hospDat, srm(halsvas0, halsvas1))
    
    
}
eq5dRes
eqvasRes
cor(eq5dRes)
cor(eqvasRes)


col1 <- colorRampPalette(c("grey14", "white", "grey14"))(100)
corrplot.mixed(cor(eq5dRes), uppe = "ellipse", lower = "number", col = col1, title = "EQ-5D index")
corrplot.mixed(cor(eq5dRes), uppe = "ellipse", lower = "number", col = col1, title = "EQ-VAS") 
