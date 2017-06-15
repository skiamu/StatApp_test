# cluster on agriculture

# 00 preliminars ----
#setwd('/Users/mowythebest/Desktop/StatApp_test')
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
                                        # kmeansCompare, radarTopic
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
myInd <- c(
  'Agricultural land (% of land area)',
  'Permanent cropland (% of land area)',
  'Arable land (% of land area)',
  'Forest area (% of land area)',
  
  'Agriculture, value added (% of GDP)',
  'Cereal yield (kg per hectare)'
)
# set years
myYear = c(2000:2011)

# extract the data
agrDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F) %>%
  unifCnt(showInd = F)
# fix 2010
agrDC <- getCntInd(agrDF,2010)
# shorten the IndicatorName
colnames(agrDC) <- cleanName(colnames(agrDC))

# stdize data
agrDC_s <- data.frame(scale(agrDC))

nCluAgr <- 5

set.seed(2000)
kmAgr <- kmeans(agrDC_s, nCluAgr, nstart = 100) # see agrCLU_02 to see why we choose 5 clusters
#pCluAgr  <- plotClusterMap(kmAgr$cluster, nCluAgr)
#pCluAgrM <- plotClusterMap(kmAgr$cluster, nCluAgr, mac=T)
#x11(); pCluAgr

pRadAgr <- radarTopic(agrDC_s,kmAgr)
#x11(); pRadAgr 

cluAgr <- data.frame(
  Cluster=1:nCluAgr,
  Description=c('agriculture is an important component in the GDP',
                'high forest area',
                'high extension of permanent cropland (coffee, gum, etc)',
                'high agricultural land',
                'efficient agriculture'),
  NumCountries=kmAgr$size
)

save(nCluAgr,cluAgr,kmAgr,agrDC_s,file = "ReadData/agrData.RData")
