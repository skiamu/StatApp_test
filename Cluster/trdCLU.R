# 00 --- preliminars ----
setwd('/Users/mowythebest/Desktop/StatApp_test')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot, kmeansCompare
load("/Users/mowythebest/Desktop/StatApp_test/Gaussianity/mcshapiro.test.RData")

cleanName <- function(x){
  x <- gsub("\\(.*$", "",  x)  # drop everything after the '('
  x <-  sub("\\s+$",  "",  x)  # drop the final space
  x <- gsub(",",      "",  x)  # drop the ','
  x <- gsub(" ",      ".", x)  # substitute the ' ' with '.'
}

# Data:
load("ReadData/data.RData")

# Libraries

library(reshape2)                       # dcast
library(dplyr)                          # %>%
library(ggplot2)                        # ggplot
library(GGally)                         # ggpairs # http://stackoverflow.com/questions/21716567/use-ggpairs-to-create-this-plot
# REM: ggpairs does NOT accept column names with spaces
library(fmsb)                           # radarchart

# 01 --- Setting the working dataframe ----

# set indicators
myInd <- c("Export value index (2000 = 100)",
    "Import value index (2000 = 100)",
    "Cost to export (US$ per container)",
    "Cost to import (US$ per container)",
    "Documents to export (number)",
    "Documents to import (number)",
    "Exports of goods and services (% of GDP)",
    "Imports of goods and services (% of GDP)",
    "Time to export (days)",
    "Time to import (days)")
# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
TradeDF <- getIndicators(myYear = myYear, myInd = myInd, agg=F)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
trdDC <- getCntInd(TradeDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(trdDC) <- cleanName(colnames(trdDC))

# stdize data
trdDC_s <- data.frame(scale(trdDC))

# 02 --- K-means clustering ####

# how many cluster?
kmeansCompare(trdDC_s,maxCl=20, n=3)

# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(trdDC_s,7, showSp=T, showMap=T, showMeans=T, mac=T)
# COMMENT : 
#   cluster 1 : low red tape and no significative growth of the trade
#   cluster 2 : high import and exports of good and services
#   cluster 3 : normal red tape  and trading is growing
#   cluster 4 : worst red tape and growth of the import
#   cluster 5 : heavy red tape and growth of the import
#   cluster 6 : medium red tape and a lot of growth of the imports
#   cluster 7 : Heavy red tape but strong growth of the trades

set.seed(2000)
kmTrd <- kmeans(trdDC_s, 7, nstart = 100)
# 03 --- FDA ####

fda.Trd   <-         fda(trdDC_s, kmTrd$cluster, as.character(1:7), 4)
aercv.Trd <- crossValFDA(trdDC_s, kmTrd$cluster, as.character(1:7), 4)
