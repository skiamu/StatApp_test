# 00 --- Set working directory, import libraries, functions and data #####

# Setting Working Directory:
setwd('/Users/mowythebest/Desktop/StatApp_test')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot, kmeansCompare
source("/Users/mowythebest/Desktop/StatApp_test/Cluster/function_fda.R")

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

# Functions
load("/Users/mowythebest/Desktop/StatApp_test/Gaussianity/mcshapiro.test.RData")

# 01 --- Setting the working dataframe ----

# Indicators of this topic: "Access to electricity (% of population)" ?, 
#                           "Fixed telephone subscriptions (per 100 people)",
#                           "Mobile cellular subscriptions (per 100 people)",
#                           "Urban population growth (annual %)",
#                           "Urban population growth (annual %)",
#                           "Internet users (per 100 people)".
# I have to decide what to do with Access because is not so common have it

# Set indicators:
myInd <- c(#"Access to electricity (% of population)",
  "Population growth (annual %)",
  "Fixed telephone subscriptions (per 100 people)",
  "Mobile cellular subscriptions (per 100 people)",
  "Urban population growth (annual %)",
  "Internet users (per 100 people)"
)

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
TeleDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F)

# shorten the IndicatorName
colnames(TeleDF) <- cleanName(colnames(TeleDF))

# TeleMatrix = Matrix Country-Indicators created from TeleDF
TeleMatrix <- getCntInd(TeleDF, myYear, dropNA = T, showCnt = T)
TeleMatrixStd <- data.frame(scale(TeleMatrix))
# 02 --- K-mean method ####

cluster5.k <- kmeans(TeleMatrixStd, centers=5) # Centers: fixed number of clusters

# 03 --- Fisher Discriminant analysis ####
a <- fda(TeleMatrixStd, cluster5.k$cluster,c('1','2','3','4','5'), 3)
c <- crossValFDA(TeleMatrixStd, cluster5.k$cluster,c('1','2','3','4','5'), 3)
c
