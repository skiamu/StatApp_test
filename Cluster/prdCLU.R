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
myInd <- c("Agriculture, value added (% of GDP)",
           "Industry, value added (% of GDP)",
           "Services, etc., value added (% of GDP)",
           "Manufacturing, value added (% of GDP)",
           "Trade (% of GDP)")
# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
PrdDF <- getIndicators(myYear = myYear, myInd = myInd, agg=F)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
PrdDC <- getCntInd(PrdDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(PrdDC) <- cleanName(colnames(PrdDC))

# stdize data
PrdDC_s <- data.frame(scale(PrdDC))

# 02 --- K-means clustering ####

# how many cluster?
kmeansCompare(PrdDC_s,maxCl=20, n=3)

# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(PrdDC_s,3, showSp=F, showMap=T, showMeans=T, mac=T)
# COMMENT : 
#   cluster 1 : Agricolture
#   cluster 2 : Industry, Manufactering and Trade
#   cluster 3 : Trade and Services

set.seed(2000)
kmPrd <- kmeans(PrdDC_s, 3, nstart = 100)
# 03 --- FDA ####

fda.Prd   <-         fda(PrdDC_s, kmPrd$cluster, as.character(1:3), 2)
aercv.Prd <- crossValFDA(PrdDC_s, kmPrd$cluster, as.character(1:3), 2)
