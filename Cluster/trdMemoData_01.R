# cluster on Trade

# 00 preliminars ----
#setwd('/Users/mowythebest/Desktop/StatApp_test')
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
source('Cluster/function_fda.R')        # fda, fdaPred

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

# 01 save material for the shiny app ####

# set indicators
# Set indicators:
myInd.Trd <- c("Export value index (2000 = 100)",
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

# TrdDF = Trade DataFrame
TrdDF <- getIndicators(myYear = myYear, myInd = myInd.Trd, agg = F)

# TrdMatrix = Matrix Country-Indicators created from TrdDF
DC.Trd <- getCntInd(TrdDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(DC.Trd) <- cleanName(colnames(DC.Trd))

# stdize data
sc <- scale(DC.Trd)
DCs.Trd  <- data.frame(sc)
mean.Trd <- attributes(sc)$'scaled:center'
var.Trd  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Trd <- 7
set.seed(2000) # so the interpretation doesn't change
km.Trd <- kmeans(DCs.Trd, nClu.Trd, nstart = 100) # see TrdcomCLU to see why we choose 5 clusters

# comments on clusters
clu.Trd <- data.frame( 
  Cluster=1:nClu.Trd,
  Description=c("Low red tape and no significative growth of the trade",
                "High import and exports of good and services",
                "Normal red tape  and trading is growing",
                "Worst red tape and growth of the import",
                "Heavy red tape and growth of the import",
                "Medium red tape and a lot of growth of the imports",
                "Heavy red tape but strong growth of the trades"),
  NumCountries=km.Trd$size
)

# fda
nFiComp.Trd <- 4
fda.Trd   <-         fda(DCs.Trd, km.Trd$cluster, as.character(1:7), nFiComp.Trd)
aercv.Trd <- crossValFDA(DCs.Trd, km.Trd$cluster, as.character(1:7), nFiComp.Trd)
# technical details
recap.Trd <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using Fisher discriminant analysis (',
                   nFiComp.Trd,'components ) with an Apparent Error Rate',
                   round(aercv.Trd,3),'(via Cross Validation)')

# save
save(nClu.Trd,clu.Trd,km.Trd,
     DCs.Trd,mean.Trd,var.Trd,myInd.Trd,
     fda.Trd,
     recap.Trd,
     file = "ReadData/TrdData.RData")
