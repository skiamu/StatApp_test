# cluster on agriculture

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
myInd.Agr <- c(
  'Agricultural land (% of land area)',
  'Permanent cropland (% of land area)',
  'Arable land (% of land area)',
  'Forest area (% of land area)',
  
  'Agriculture, value added (% of GDP)',
  'Cereal yield (kg per hectare)'
)
# set years 
myYear = c(2000:2011) # to have a selection of countries constant at least on these years

# extract the data
DF.Agr <- getIndicators(myYear = myYear, myInd = myInd.Agr, agg = F) %>%
  unifCnt(showInd = F)

# fix 2010
DC.Agr <- getCntInd(DF.Agr,2010)

# shorten the IndicatorName
colnames(DC.Agr) <- cleanName(colnames(DC.Agr))

# stdize data
sc <- scale(DC.Agr)
DCs.Agr  <- data.frame(sc)
mean.Agr <- attributes(sc)$'scaled:center'
var.Agr  <- attributes(sc)$'scaled:scale'

# cluster analysis
nClu.Agr <- 5
set.seed(2000) # so the interpretation doesn't change
km.Agr <- kmeans(DCs.Agr, nClu.Agr, nstart = 100) # see agrCLU_02 to see why we choose 5 clusters

# comments on clusters
clu.Agr <- data.frame( 
  Cluster=1:nClu.Agr,
  Description=c('Agriculture is an important component in the GDP',
                'High forest area',
                'High extension of permanent cropland (coffee, gum, etc)',
                'High agricultural land',
                'Efficient agriculture'),
  NumCountries=km.Agr$size
)

# fda
nFiComp.Agr <- 3
fda.Agr   <-         fda(DCs.Agr, km.Agr$cluster, as.character(1:5), nFiComp.Agr)
aercv.Agr <- crossValFDA(DCs.Agr, km.Agr$cluster, as.character(1:5), nFiComp.Agr)
aercv.Agr
# knn
aercv.Agr <- crossValKNN(DCs.Agr, km.Agr$cluster, as.character(1:5))
aercv.Agr
# technical details
recap.Agr <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using k-nearest neighbour,
                   with an Apparent Error Rate',
                   round(aercv.Agr,3),'(via Cross Validation)')

# save
save(nClu.Agr,clu.Agr,km.Agr,
     DCs.Agr,mean.Agr,var.Agr,myInd.Agr,
     fda.Agr,
     recap.Agr,
     file = "ReadData/agrData.RData")

