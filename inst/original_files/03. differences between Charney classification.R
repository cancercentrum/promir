rm(list = ls(all = TRUE))
gc()
# Differences between genders
library(dplyr)
library(psych)
source("Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/01. the danish_eq5d.R")
source("Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Code/functions.R")
load("Z:\\SHPR\\SZILARD\\People\\Szilard\\Improvement Index\\Data\\promdata2009.RData")

## Danish EQ-5D index

promData$eq5d0de <- with(promData, eqDanish(eq10, eq20, eq30, eq40, eq50))
promData$eq5d1de <- with(promData, eqDanish(eq11, eq21, eq31, eq41, eq51))
head(promData)


promData2 <- select(promData, halsvas0, halsvas1, eq5d0de, eq5d1de, charkat0)
promData2 <- mutate(promData2, deltaEQ5D = eq5d1de - eq5d0de, deltaEQVAS = halsvas1 - halsvas0)
describeBy(promData2, group = "charkat0")


############ Trend tests 1. EQ-5D 1.1 Preop
fit.eq5d0 <- aov(eq5d0de ~ charkat0, data = promData2)
summary(fit.eq5d0, split = list(charkat0 = list(linear = 1, quadratic = 2)))
summary(fit.eq5d0, split = list(charkat0 = list(linear = 1, nonlinear = 2)))

## 1.2 Postop
fit.eq5d1 <- aov(eq5d1de ~ charkat0, data = promData2)
summary(fit.eq5d1, split = list(charkat0 = list(linear = 1, quadratic = 2)))
summary(fit.eq5d1, split = list(charkat0 = list(linear = 1, nonlinear = 2)))


### 1. EQ-VAS 1.1 Preop
fit.eqvas0 <- aov(halsvas0 ~ charkat0, data = promData2)
summary(fit.eqvas0, split = list(charkat0 = list(linear = 1, quadratic = 2)))
summary(fit.eqvas0, split = list(charkat0 = list(linear = 1, nonlinear = 2)))

## 1.2 Postop
fit.eqvas1 <- aov(halsvas1 ~ charkat0, data = promData2)
summary(fit.eqvas1, split = list(charkat0 = list(linear = 1, quadratic = 2)))
summary(fit.eqvas1, split = list(charkat0 = list(linear = 1, nonlinear = 2)))





classA <- filter(promData2, charkat0 == "A")
classB <- filter(promData2, charkat0 == "B")
classC <- filter(promData2, charkat0 == "C")

########## EQ-5D index PASS

# 1. Confidence Intervals
with(classA, deltaConf(eq5d0de, eq5d1de, 0.9))
with(classB, deltaConf(eq5d0de, eq5d1de, 0.9))
with(classC, deltaConf(eq5d0de, eq5d1de, 0.9))

# 2. test for trends
eq5d <- c(with(classC, ir(eq5d0de, eq5d1de, 0.9)), with(classB, ir(eq5d0de, eq5d1de, 0.9)), with(classA, ir(eq5d0de, eq5d1de, 0.9)))

eq5dSDS <- c(with(classC, irSD(eq5d0de, eq5d1de, 0.9)), with(classB, irSD(eq5d0de, eq5d1de, 0.9)), with(classA, irSD(eq5d0de, eq5d1de, 0.9)))

irTrend(eq5d, eq5dSDS)



######### EQ-VAS PASS

# 1. Confidence Intervals
with(classA, deltaConf(halsvas0, halsvas1, 82))
with(classB, deltaConf(halsvas0, halsvas1, 82))
with(classC, deltaConf(halsvas0, halsvas1, 82))

# 2. test for trends
eqvas <- c(with(classA, ir(halsvas0, halsvas1, 82)), with(classB, ir(halsvas0, halsvas1, 82)), with(classC, ir(halsvas0, halsvas1, 82)))

eqvasSDS <- c(with(classA, irSD(halsvas0, halsvas1, 82)), with(classB, irSD(halsvas0, halsvas1, 82)), with(classC, irSD(halsvas0, halsvas1, 
    82)))

irTrend(eqvas, eqvasSDS)


################################################################################ 


############## Forest Plot #########################

setwd("Z:/SHPR/SZILARD/People/Szilard/Improvement Index/Figures")

tiff(filename = "CharneyClassification2015-01-20x.tif", width = 20, height = 12, units = "cm", pointsize = 10, compression = "lzw", bg = "white", 
    res = 600, restoreConsole = TRUE, type = "cairo")
nf <- layout(matrix(c(1:5), 1, 5, byrow = TRUE), c(1.5, 0.75, 0.5, 0.5, 4), c(5, 5, 5, 5, 5), FALSE)
layout.show(nf)

## 1. Classes

par(mar = c(5, 0, 2, 0) + 0.1)
plot.new()
plot.window(ylim = c(0, 4), xlim = c(0, 1))
text(0, 1.5, "EQ-5D Index", cex = 2, col = "black", pos = 4)
text(0, 3.5, "EQ-VAS", cex = 2, col = "black", pos = 4)


## 2. Variables
plot.new()
plot.window(ylim = c(0, 4), xlim = c(0, 1))
# EQ5D
text(0, 1.5, "Class A", cex = 1.5, col = "black", pos = 4)
text(0, 1, "Class B", cex = 1.5, col = "black", pos = 4)
text(0, 0.5, "Class C", cex = 1.5, col = "black", pos = 4)

# EQ-VAS
text(0, 3.5, "Class A", cex = 1.5, col = "black", pos = 4)
text(0, 3, "Class B", cex = 1.5, col = "black", pos = 4)
text(0, 2.5, "Class C", cex = 1.5, col = "black", pos = 4)
mtext("Charnley", side = 3, line = -2, cex = 1.25)
## 3. Pre-op values
plot.new()
plot.window(ylim = c(0, 4), xlim = c(0, 1))
# EQ5D
text(0, 1.5, "0.58", cex = 1.5, col = "black", pos = 4)
text(0, 1, "0.59", cex = 1.5, col = "black", pos = 4)
text(0, 0.5, "0.53", cex = 1.5, col = "black", pos = 4)

# EQ-VAS
text(0, 3.5, "62.72", cex = 1.5, col = "black", pos = 4)
text(0, 3, "59.62", cex = 1.5, col = "black", pos = 4)
text(0, 2.5, "55.26", cex = 1.5, col = "black", pos = 4)
mtext("Pre", side = 3, line = -2, cex = 1.25)
## 4. Post-op values
plot.new()
plot.window(ylim = c(0, 4), xlim = c(0, 1))
# EQ5D
text(0, 1.5, "0.87", cex = 1.5, col = "black", pos = 4)
text(0, 1, "0.81", cex = 1.5, col = "black", pos = 4)
text(0, 0.5, "0.78", cex = 1.5, col = "black", pos = 4)

# EQ-VAS
text(0, 3.5, "82.61", cex = 1.5, col = "black", pos = 4)
text(0, 3, "77.05", cex = 1.5, col = "black", pos = 4)
text(0, 2.5, "72.47", cex = 1.5, col = "black", pos = 4)
mtext("Post", side = 3, line = -2, cex = 1.25)
## 5. Forest Plot
plot.new()

plot.window(ylim = c(0, 4), xlim = c(45, 115))
axis(1, lwd = 3, at = seq(50, 110, by = 10), cex.axis = 2, font = 2)
####### EQ-5D A
segments(x0 = 87.688, y0 = 1.5, x1 = 94.52889, y1 = 1.5, lwd = 2, col = "black")
points(91.10844, 1.5, pch = 15, cex = 3, col = "grey")
# B
segments(x0 = 64.10418, y0 = 1, x1 = 78.30694, y1 = 1, lwd = 2, col = "black")
points(71.20556, 1, , pch = 15, cex = 3, col = "grey")
# C
segments(x0 = 62.38479, y0 = 0.5, x1 = 69.92422, y1 = 0.5, lwd = 2, col = "black")
points(66.1545, 0.5, pch = 15, cex = 3, col = "grey")

text(x = 60, y = 0.1, expression(chi[1]^2 == 70))
text(x = 67, y = 0.1, paste(", p < 0.0001"))
text(x = 60, y = 0.22, paste("Trend test:"))


####### EQ-VAS A
segments(x0 = 97.41983, y0 = 3.5, x1 = 108.91872, y1 = 3.5, lwd = 2, col = "black")
points(103.16928, 3.5, pch = 15, cex = 3, col = "grey")
# B
segments(x0 = 67.63961, y0 = 3, x1 = 88.08589, y1 = 3, lwd = 2, col = "black")
points(77.86275, 3, , pch = 15, cex = 3, col = "grey")
# EQ VAS
segments(x0 = 58.73929, y0 = 2.5, x1 = 69.98458, y1 = 2.5, lwd = 2, col = "black")
points(64.36194, 2.5, pch = 15, cex = 3, col = "grey")
text(x = 60, y = 2.1, expression(chi[1]^2 == 253))
text(x = 67, y = 2.1, paste(", p < 0.0001"))
text(x = 60, y = 2.22, paste("Trend test:"))

mtext("Improvement Ratio", side = 1, line = 2.95, cex = 1.25)
dev.off() 
