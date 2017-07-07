# cluster on Economic Indicators

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
myInd.Ein <- c("Foreign direct investment, net inflows (BoP, current US$)",
               "GDP at market prices (current US$)",
               "GDP per capita (current US$)",
               "GDP growth (annual %)",
               "Inflation, GDP deflator (annual %)",
               "Inflation, consumer prices (annual %)")

# Set the year:
myYear <- c(2010)

# EinDF = Trade DataFrame
EinDF <- getIndicators(myYear = myYear, myInd = myInd.Ein, agg = F)
EinDF <- filter(EinDF, EinDF$CountryName != "USA")

# EinMatrix = Matrix Country-Indicators created from EinDF
DC.Ein <- getCntInd(EinDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(DC.Ein) <- cleanName(colnames(DC.Ein))

# stdize data
sc <- scale(DC.Ein)
DCs.Ein  <- data.frame(sc)
mean.Ein <- attributes(sc)$'scaled:center'
var.Ein  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Ein <- 5
set.seed(2000) # so the interpretation doesn't change
km.Ein <- kmeans(DCs.Ein, nClu.Ein, nstart = 100) # see EincomCLU to see why we choose 5 clusters

# comments on clusters
clu.Ein <- data.frame( 
  Cluster=1:nClu.Ein,
  Description=c("Poor Economies",
                "Poor Economies and strong inflation, but good GDP growth",
                "High GDP and high foreign investments",
                "Strong inflation and good GDP indicators",
                "High GDP growth, GDP per capita and foreign investments"
                ),
  NumCountries=km.Ein$size
)

# fda
nFiComp.Ein <- 3
fda.Ein   <-         fda(DCs.Ein, km.Ein$cluster, as.character(1:5), nFiComp.Ein)
aercv.Ein <- crossValFDA(DCs.Ein, km.Ein$cluster, as.character(1:5), nFiComp.Ein)

# knn
aercv.Ein <- crossValKNN(DCs.Ein, km.Ein$cluster, as.character(1:5))

# technical details
recap.Ein <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using k-nearest neighbour,
                   with an Apparent Error Rate',
                   round(aercv.Ein,3),'(via Cross Validation)')

# save
save(nClu.Ein,clu.Ein,km.Ein,
     DCs.Ein,mean.Ein,var.Ein,myInd.Ein,
     fda.Ein,
     recap.Ein,
     file = "ReadData/EinData.RData")
