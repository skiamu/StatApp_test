# cluster on Telecommunications

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
myInd.Tel <- c(
  "Population growth (annual %)",
  "Fixed telephone subscriptions (per 100 people)",
  "Mobile cellular subscriptions (per 100 people)",
  "Urban population growth (annual %)",
  "Internet users (per 100 people)"
)

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
TeleDF <- getIndicators(myYear = myYear, myInd = myInd.Tel, agg = F)

# shorten the IndicatorName
colnames(TeleDF) <- cleanName(colnames(TeleDF))

# TeleMatrix = Matrix Country-Indicators created from TeleDF
DC.Tel <- getCntInd(TeleDF, myYear, dropNA = T, showCnt = T)

# stdize data
sc <- scale(DC.Tel)
DCs.Tel  <- data.frame(sc)
mean.Tel <- attributes(sc)$'scaled:center'
var.Tel  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Tel <- 5
set.seed(2000) # so the interpretation doesn't change
km.Tel <- kmeans(DCs.Tel, nClu.Tel, nstart = 100) # see telecomCLU to see why we choose 5 clusters

# comments on clusters
clu.Tel <- data.frame( 
  Cluster=1:nClu.Tel,
  Description=c("Countries with the most advanced telecommunication system and a strong growth of the population(abs value and urban)",
                "Countries with an advanced telecommunication sector but low growth of the population",
                "Countries with a normal telecommunication system but without a growth of the population",
                "Countries with a worst telecommunications sector but with a strong growth of the population",
                "Countries with a bad telecommunication sector and a medium-high growth of the population"),
  NumCountries=km.Tel$size
)

# fda
nFiComp.Tel <- 3
fda.Tel   <-         fda(DCs.Tel, km.Tel$cluster, as.character(1:5), nFiComp.Tel)
aercv.Tel <- crossValFDA(DCs.Tel, km.Tel$cluster, as.character(1:5), nFiComp.Tel)
# technical details
recap.Tel <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using Fisher discriminant analysis (',
                   nFiComp.Tel,'components) with an Apparent Error Rate',
                   aercv.Tel,'(via Cross Validation)')

# save
save(nClu.Tel,clu.Tel,km.Tel,
     DCs.Tel,mean.Tel,var.Tel,myInd.Tel,
     fda.Tel,
     recap.Tel,
     file = "ReadData/TelData.RData")
