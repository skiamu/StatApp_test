# cluster on agriculture

# 00 preliminars ----
#setwd('/Users/mowythebest/Desktop/StatApp_test')
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
source('Cluster/fda.R')                 # fda

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
sc <- scale(agrDC)

agrDC_s <- data.frame(sc)
meanAgr <- attributes(sc)$'scaled:center'
varAgr  <- attributes(sc)$'scaled:scale'

nCluAgr <- 5

set.seed(2000)
kmAgr <- kmeans(agrDC_s, nCluAgr, nstart = 100) # see agrCLU_02 to see why we choose 5 clusters

cluAgr <- data.frame(
  Cluster=1:nCluAgr,
  Description=c('agriculture is an important component in the GDP',
                'high forest area',
                'high extension of permanent cropland (coffee, gum, etc)',
                'high agricultural land',
                'efficient agriculture'),
  NumCountries=kmAgr$size
)

# FDA
#fdaAgr <- fda(agrDC_s, kmAgr$cluster, 1:nCluAgr, 3, length(myInd))
# giusto x farlo andare in attesa del fix
fdaAgr <- fda(TeleMatrixStd, cluster5.k$cluster, 1:5,3,5)

# find the years in which a certain state has all the features
findYears <- function(ind,cnt){
  temp  <- getIndicators(myInd = ind, myCnt = cnt)
  temp1 <- getIndYear(temp,'Italy')
  dc    <- temp1[ , colSums(is.na(temp1)) == 0]
  yearOk <- colnames(dc)
  return(list(years=yearOk,dc=dc))
}

#test <- findYears(myInd,'Italy')

findValues <- function(dc,year){
  return(dc[,as.character(year)])
}

#val <- findValues(test$dc,2000)

std <- function(val,mm,vv){
  return((val-mm)/vv)
}

#valS <- std(val,meanAgr,varAgr)





myIndAgr <- myInd

save(nCluAgr,cluAgr,kmAgr,
     agrDC_s,meanAgr,varAgr,myIndAgr,
     fdaAgr,
     file = "ReadData/agrData.RData")



