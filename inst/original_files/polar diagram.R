 rm(list = ls(all = TRUE))
### Plaor diagram
#
# 2013-06-29, Szilárd Nemes
#
library(fmsb)

## The work directory
setwd('Z:/SHPR/SZILARD/People/Szilard/Index-ci')

dat <- read.table('polar.txt', header = TRUE, row.names = 1, sep = '\t')

Labels  <- c('Falun',
             'Capio S:t Göran',
             'Trelleborg',
             'Uddevalla',
             'Bollnäs',
             'Hässleholm-\nKristianstad',
             'Falköping',
             'Motala',
             'Piteå',
             'Capio\nOrtopediska Huset',
             'Ortho\nCenter Stockholm')
 tiff(filename = "polar.tif",
     width = 16,  height = 16, units= 'cm', pointsize = 10,
     compression = "lzw",
     bg = "white", res = 600, , restoreConsole = TRUE,
     type =  "cairo")

radarchart(dat,
           axistype = 1,
           seg = 5,
           plty = 1,
           pty = 18,
           cex = 2,
           plwd = 2,
           cglty = 1,
           cglcol = 'grey',
           axislabcol = 'black',
           centerzero = TRUE,
           vlabels = Labels,
           pcol = c('black', 'royalblue', 'darkred'))
legend('bottomright', legend = c('EQ-5D index', 'EQ VAS', 'Pain VAS'),
                         col = c('black', 'royalblue', 'darkred'),
                         lwd = 2,
                         lty = 1,
                         pch = 18,
                         bty = 'n')
  dev.off()
