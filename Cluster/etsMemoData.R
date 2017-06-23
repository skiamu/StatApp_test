# cluster on Ease to start an activity

# 00 preliminars ----
setwd('/Users/mowythebest/Desktop/StatApp_test')
#setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

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
myInd.Ets <- c("Cost of business start-up procedures (% of GNI per capita)",
               "Start-up procedures to register a business (number)",
               "Time required to start a business (days)",
               "Time to prepare and pay taxes (hours)",
               "Total tax rate (% of commercial profits)")

# Set the year:
myYear <- c(2010)

# EtsDF = Trade DataFrame
EtsDF <- getIndicators(myYear = myYear, myInd = myInd.Ets, agg = F)

# shorten the IndicatorName
colnames(EtsDF) <- cleanName(colnames(EtsDF))

# EtsMatrix = Matrix Country-Indicators created from EtsDF
DC.Ets <- getCntInd(EtsDF, myYear, dropNA = T, showCnt = T)

# stdize data
sc <- scale(DC.Ets)
DCs.Ets  <- data.frame(sc)
mean.Ets <- attributes(sc)$'scaled:center'
var.Ets  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Ets <- 3
set.seed(2000) # so the interpretation doesn't change
km.Ets <- kmeans(DCs.Ets, nClu.Ets, nstart = 100) # see EtscomCLU to see why we choose 5 clusters

# comments on clusters
clu.Ets <- data.frame( 
  Cluster=1:nClu.Ets,
  Description=c("Cost to start a business and tax rate and time to prepare them High",
                "Procedure and time to start a business and red tape to pay taxes high",
                "Easy to start a busness"),
  NumCountries=km.Ets$size
)

# fda
nFiComp.Ets <- 2
fda.Ets   <-         fda(DCs.Ets, km.Ets$cluster, as.character(1:3), nFiComp.Ets)
aercv.Ets <- crossValFDA(DCs.Ets, km.Ets$cluster, as.character(1:3), nFiComp.Ets)
# technical details
recap.Ets <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using Fisher discriminant analysis (',
                   nFiComp.Ets,'components) with an Apparent Error Rate',
                   aercv.Ets,'(via Cross Validation)')

# save
save(nClu.Ets,clu.Ets,km.Ets,
     DCs.Ets,mean.Ets,var.Ets,myInd.Ets,
     fda.Ets,
     recap.Ets,
     file = "ReadData/EtsData.RData")
