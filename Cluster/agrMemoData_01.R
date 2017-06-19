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
myIndAgr <- c(
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
agrDF <- getIndicators(myYear = myYear, myInd = myIndAgr, agg = F) %>%
  unifCnt(showInd = F)

# fix 2010
agrDC <- getCntInd(agrDF,2010)

# shorten the IndicatorName
colnames(agrDC) <- cleanName(colnames(agrDC))

# stdize data
sc <- scale(agrDC)
agrDC_s <- data.frame(sc)
meanAgr <- attributes(sc)$'scaled:center'
varAgr  <- attributes(sc)$'scaled:scale'

# cluster analysis
nCluAgr <- 5
set.seed(2000) # so the interpretation doesn't change
kmAgr <- kmeans(agrDC_s, nCluAgr, nstart = 100) # see agrCLU_02 to see why we choose 5 clusters

# comments on clusters
cluAgr <- data.frame( 
  Cluster=1:nCluAgr,
  Description=c('agriculture is an important component in the GDP',
                'high forest area',
                'high extension of permanent cropland (coffee, gum, etc)',
                'high agricultural land',
                'efficient agriculture'),
  NumCountries=kmAgr$size
)

# fda
fdaAgr <- fda(agrDC_s, kmAgr$cluster, 3)

kmAgr$cluster['Italy']
fdaPred(fdaAgr$fComp,fdaAgr$center,agrDC_s['Italy',])
kmAgr$cluster['Albania']
fdaPred(fdaAgr$fComp,fdaAgr$center,agrDC_s["Albania",])
kmAgr$cluster['Algeria']
fdaPred(fdaAgr$fComp,fdaAgr$center,agrDC_s["Algeria",])
kmAgr$cluster['Antigua']
fdaPred(fdaAgr$fComp,fdaAgr$center,agrDC_s["Antigua",])
kmAgr$cluster['Argentina']
fdaPred(fdaAgr$fComp,fdaAgr$center,agrDC_s["Argentina",])
# HERE AERCV HIGH OR THERE IS SOMETHING THAT DOESN'T WORK

# save
save(nCluAgr,cluAgr,kmAgr,
     agrDC_s,meanAgr,varAgr,myIndAgr,
     fdaAgr,
     file = "ReadData/agrData.RData")
