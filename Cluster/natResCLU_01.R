# cluster on natural resources

# 00 preliminars ----
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot, kmeansCompare
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
myInd <- c('Mineral rents (% of GDP)',
           'Oil rents (% of GDP)',
           'Coal rents (% of GDP)',
           'Natural gas rents (% of GDP)',
           'Forest rents (% of GDP)')

# set years
myYear = c(2000:2012)

# extract the data
nrDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F) %>%
  unifCnt(showInd = F)
# fix 2010
nrDC <- getCntInd(nrDF,2010)
# shorten the IndicatorName
colnames(nrDC) <- cleanName(colnames(nrDC))

# stdize data
nrDC_s <- data.frame(scale(nrDC))

# 02 clustering

# have a look at the data --> scatter plot
sp <- ggpairs(nrDC, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth")) # 
x11(); sp

# CHOICE : use the stdize data
# REM    : it changes a lot the things!

# hierarchical clustering
plotClusterHierarchical(nrDC_s)

# COMMENT : there is no evident cluster structure [???]

# k-menas clustering
# how many cluster?
kmeansCompare(nrDC_s,maxCl=20, n=3)
# COMMENT : 4 
# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(nrDC_s,4, showSp=T, showMap=T, showMeans=T)
# COMMENT : 
#   memo      : the variable are rents as percentage of the GDP, don't forget when interpreting
#   cluster 1 : rents from nat res low [most]
#   cluster 2 : high oil and gas [ARA,IRA,KAZ,ALG,LYB,BOL,ANG,GAB,CON]
#   cluster 3 : high coal and min [MON]
#   cluster 4 : high forest and mineral [Dem Rep Congo,CHI,ETH,...]

