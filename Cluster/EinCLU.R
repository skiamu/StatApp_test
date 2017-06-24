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
myInd <- c("Foreign direct investment, net inflows (BoP, current US$)",
           "GDP at market prices (current US$)",
           "GDP per capita (current US$)",
           "GDP growth (annual %)",
           "Inflation, GDP deflator (annual %)",
           "Inflation, consumer prices (annual %)")
# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
EinDF <- getIndicators(myYear = myYear, myInd = myInd, agg=F)
EinDF <- filter(EinDF, EinDF$CountryName != "USA") # Usa crea biplot troppo largo
# TeleMatrix = Matrix Country-Indicators created from TeleDF
EinDC <- getCntInd(EinDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(EinDC) <- cleanName(colnames(EinDC))

# stdize data
EinDC_s <- data.frame(scale(EinDC))

# 02 --- K-means clustering ####

# how many cluster?
kmeansCompare(EinDC_s,maxCl=20, n=3)

# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(EinDC_s,5, showSp=F, showMap=T, showMeans=T, mac=T)
# COMMENT : 
#   cluster 1 : Poor Economies 
#   cluster 2 : Poor Economies with good GDP growth but significative presence of inflation
#   cluster 3 : High GDP and high foreign investments
#   cluster 4 : Countries with strong inflation but not so poor
#   cluster 5 : High GDP growth and per capita and significative presence of foreign investments

set.seed(2000)
kmEin <- kmeans(EinDC_s, 5, nstart = 100)
# 03 --- FDA ####

fda.Ein   <-         fda(EinDC_s, kmEin$cluster, as.character(1:5), 2)
aercv.Ein <- crossValFDA(EinDC_s, kmEin$cluster, as.character(1:5), 2)
