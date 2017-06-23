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
myInd <- c("Cost of business start-up procedures (% of GNI per capita)",
           "Start-up procedures to register a business (number)",
           "Time required to start a business (days)",
           "Time to prepare and pay taxes (hours)",
           "Total tax rate (% of commercial profits)")
# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
EtsDF <- getIndicators(myYear = myYear, myInd = myInd, agg=F)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
EtsDC <- getCntInd(EtsDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(EtsDC) <- cleanName(colnames(EtsDC))

# stdize data
EtsDC_s <- data.frame(scale(EtsDC))

# 02 --- K-means clustering ####

# how many cluster?
kmeansCompare(EtsDC_s,maxCl=20, n=3)

# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(EtsDC_s,3, showSp=F, showMap=T, showMeans=T, mac=T)
# COMMENT : 
#   cluster 1 : Cost to start a business and tax rate and time to prepare them High
#   cluster 2 : Procedure and time to start a business and red tape to pay taxes high
#   cluster 3 : Easy to start a busness

set.seed(2000)
kmEts <- kmeans(EtsDC_s, 2, nstart = 100)
# 03 --- FDA ####

fda.Ets   <-         fda(EtsDC_s, kmEts$cluster, as.character(1:3), 1)
aercv.Ets <- crossValFDA(EtsDC_s, kmEts$cluster, as.character(1:3), 1)
