# cluster on Production

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
myInd.Prd <- c("Agriculture, value added (% of GDP)",
               "Industry, value added (% of GDP)",
               "Services, etc., value added (% of GDP)",
               "Manufacturing, value added (% of GDP)",
               "Trade (% of GDP)")

# Set the year:
myYear <- c(2010)

# PrdDF = Trade DataFrame
PrdDF <- getIndicators(myYear = myYear, myInd = myInd.Prd, agg = F)

# PrdMatrix = Matrix Country-Indicators created from PrdDF
DC.Prd <- getCntInd(PrdDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(DC.Prd) <- cleanName(colnames(DC.Prd))

# stdize data
sc <- scale(DC.Prd)
DCs.Prd  <- data.frame(sc)
mean.Prd <- attributes(sc)$'scaled:center'
var.Prd  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Prd <- 3
set.seed(2000) # so the interpretation doesn't change
km.Prd <- kmeans(DCs.Prd, nClu.Prd, nstart = 100) # see PrdcomCLU to see why we choose 5 clusters

# comments on clusters
clu.Prd <- data.frame( 
  Cluster=1:nClu.Prd,
  Description=c("Economy based on Industry, Manufacturing and Trade",
                "Economy based on Agriculture",
                "Economy based on Services and Trade"),
  NumCountries=km.Prd$size
)

# fda
nFiComp.Prd <- 2
fda.Prd   <-         fda(DCs.Prd, km.Prd$cluster, as.character(1:3), nFiComp.Prd)
aercv.Prd <- crossValFDA(DCs.Prd, km.Prd$cluster, as.character(1:3), nFiComp.Prd)

# knn
aercv.Prd <- crossValKNN(DCs.Prd, km.Prd$cluster, as.character(1:3))
# technical details
recap.Prd <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using k-nearest neighbour,
                   with an Apparent Error Rate',
                   round(aercv.Prd,3),'(via Cross Validation)')

# save
save(nClu.Prd,clu.Prd,km.Prd,
     DCs.Prd,mean.Prd,var.Prd,myInd.Prd,
     fda.Prd,
     recap.Prd,
     file = "ReadData/PrdData.RData")
