 rm(list = ls(all = TRUE)); gc()

### EQ-5D prediction
#
# File nr1. Data preparation
#   !!!!!!!!     Select data from 2009!!!!!!!!!!
# Select patients with only one operation
# and complete data.


# Libraries
library(foreign)            # read in spss
library(dplyr)              # data management




## The PROM Data
prom <- read.spss('Z:/SHPR/SZILARD/People/Szilard/EQ5D prediction/Data/Dispens\0x2030r.sav',
                 use.value.labels = FALSE,
                    to.data.frame = TRUE,
                     use.missings = to.data.frame)


prom <- select(prom, person, sida, k\0x02C6n,
                    charkat0, smrtvas0,halsvas0,
                    eq10, eq20, eq30, eq40, eq50, eq5d0,
                    eq11, eq21, eq31, eq41, eq51, eq5d1,
                    charkat1, smrtvas1,halsvas1,
                    eq11, eq21, eq31, eq41, eq51, eq5d1)
## Missing data coding for charkat0 is worng, NA is a level coded as space
table(prom$charkat0, useNA = 'ifany')
table(prom$charkat1, useNA = 'ifany')
prom$charkat0[prom$charkat0 == levels(prom$charkat0)[1]] <- NA
prom$charkat1[prom$charkat1 == levels(prom$charkat1)[1]] <- NA
prom <- droplevels(prom)
table(prom$charkat0, useNA = 'ifany')
table(prom$charkat1, useNA = 'ifany')

## The operation data base

oper <- read.spss('Z:/SHPR/SZILARD/People/Szilard/EQ5D prediction/Data/Prim\0x2030roperation.sav',
                 use.value.labels = FALSE,
                    to.data.frame = TRUE,
                     use.missings = to.data.frame)

oper <- select(oper, Person, Sida, OppAr,DiaGrp, Snitt,ProtGrp, KlinPri, ASA)
names(oper) <- tolower(names(oper))
head(oper)


preData <- merge(prom, oper, by = c('person', 'sida'))
preData <- filter(preData, oppar == 2009)
preData <- na.omit(preData)
doubleOperations <- preData$person[duplicated(preData$person)]

preData <- filter(preData, !person %in% doubleOperations)
head(preData)
summary(preData)
dim(preData)



gc()

### Data for the prediction of the INDEX

promData <- filter(preData, diagrp ==1,
                                   snitt %in% c(1, 2, 4),
                                   protgrp!=4)
 promData <- droplevels(promData)
 summary(promData)
 dim(promData)


promData <- select(promData, klinpri, k\0x02C6n,
                    charkat0, smrtvas0,halsvas0, smrtvas1,halsvas1,
                    eq10, eq20, eq30, eq40, eq50, eq5d0,
                    eq11, eq21, eq31, eq41, eq51, eq5d1)
## remove empty spaces in gender 
promData$k\0x02C6n  <- factor(gsub(" ", "", promData$k\0x02C6n, fixed = TRUE))
## more than 200 operations
promData <- promData[promData$klinpri %in% names(which(table(promData$klinpri)>200)), ]


head(promData)
summary(promData)


rm(list=setdiff(ls(), "promData"))


