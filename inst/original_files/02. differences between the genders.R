 rm(list = ls(all = TRUE)); gc()
#
#  Differences between genders
#
#
library(dplyr)
source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/01. the danish_eq5d.R')
source('Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/functions.R')
load("Z:\\SHPR\\SZILARD\\People\\Szilard\\Improvement Index\\Data\\promdata2009.RData")

## Danish EQ-5D index

promData$eq5d0de <- with(promData, eqDanish(eq10, eq20, eq30, eq40, eq50))
promData$eq5d1de <- with(promData, eqDanish(eq11, eq21, eq31, eq41, eq51))
head(promData)



females <- filter(promData, kön == 'kvinna')
  males <- filter(promData, kön != 'kvinna')

### EQ-5D index   PASS
with(females, deltaConf(eq5d0de, eq5d1de, 0.9))
  with(males, deltaConf(eq5d0de, eq5d1de, 0.9))

FemalesEQ5D <- with(females, irSummary(eq5d0de, eq5d1de, 0.9))
  MalesEQ5D <- with(males, irSummary(eq5d0de, eq5d1de, 0.9))

sum(females$eq5d1de>0.9); mean(females$eq5d1de>0.9)
sum(males$eq5d1de>0.9); mean(males$eq5d1de>0.9)


irTest(FemalesEQ5D, MalesEQ5D)

### EQ-VAS    PASS
with(females, deltaConf(halsvas0, halsvas1, 82))
  with(males, deltaConf(halsvas0, halsvas1, 82))

FemalesEQVAS <- with(females, irSummary(halsvas0, halsvas1, 82))
  MalesEQVAS <- with(males, irSummary(halsvas0, halsvas1, 82))



irTest(FemalesEQVAS, MalesEQVAS)

#### EQ-VAS vs EQ-5D

irTest(FemalesEQ5D, FemalesEQVAS)
irTest(MalesEQ5D, MalesEQVAS)
 
