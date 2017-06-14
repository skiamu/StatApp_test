# cluster on agriculture

# 00 preliminars ----
setwd('/Users/mowythebest/Desktop/StatApp_test')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot, kmeansCompare
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

# 02 clustering

# have a look at the data --> scatter plot
sp <- ggpairs(agrDC, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth")) # 
x11(); sp

# CHOICE : use the stdize data
# REM    : it changes a lot the things!

# hierarchical clustering
plotClusterHierarchical(agrDC_s)

# COMMENT : there is no evident cluster structure [???]

# k-menas clustering
# how many cluster?
kmeansCompare(agrDC_s,maxCl=20, n=3)
# COMMENT : 4 o 5?
# set the seed so the cluster 1 remain cluster 1 
set.seed(2000)
kmeansPlot(agrDC_s,4, showSp=T, showMap=T, showMeans=T)
# COMMENT : 
#   cluster 1 : high forest area and high agr yield [RUS,BRA,NZE,(equatorial africa), scandinavian area]
#   cluster 2 : high permanent crop (coffee, gum), high agr val add (agr is important in the GDP)
#               and middle forest area [TUN, countries in the Guinea gulf, islands in south-east asia]
#   cluster 3 : high arable land and yield []
#   cluster 4 : advanced big countries
set.seed(2000)
kmeansPlot(agrDC_s,5, showSp=T, showMap=T, showMeans=T)
# COMMENT : 
#   cluster 1 : see above
#   cluster 2 : see cl3 above
#   cluster 3 : see cl2 above
#   cluster 4 : high arable and agr land [IND, SPA, UKR]
#   cluster 5 : high cereal yield and agr land [USA, EUR, SAF, ARA, CHI]

# There is a spike for each variable?
# is it due to the fact that the ditribution is centered in 0 with some outliers?
# how should we deal with that?

set.seed(2000)
kmAgr <- kmeans(agrDC_s, 5, nstart = 100)
pCluAgr <- plotClusterMap(kmAgr$cluster, 5)
x11(); pCluAgr

radarTopic <- function(dc,km,cntRad=NULL,mac=F){
  if(mac) {colClIn=NULL; colorCl=colorClMac}
  dc$cluster <- sapply(row.names(dc), function(x) as.character(km$cluster[x]))
  meansCl <- dc %>% group_by(cluster) %>% summarize_each(funs(mean))
  if(!is.null(cntRad)){ # put a country in cntRad to plot it on the radarplot
    meansCl <- rbind(meansCl,dc[cntRad,])
    meansCl[cntRad,'cluster'] <- cntRad 
  }
  radarchart(meansCl[,!names(meansCl) %in% c('cluster')] , axistype=0 , maxmin=F,
             #custom polygon
             pcol=colorCl , pfcol=colClIn , plwd=4 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
             #custom labels
             vlcex=0.8 )
  #legend(x=0, y=-1.3, xjust = 0.5, horiz=T, legend = meansCl$cluster, bty = "n", pch=20 , col=colorCl , text.col = "grey", cex=1.2, pt.cex=3)
  return(recordPlot())
}

pRadAgr <- radarTopic(agrDC_s,kmAgr)
x11(); pRadAgr 

# find similar countries
simCnt <- function(dc,cnt,n){ # find the n most similar countries to cnt
  return(sort(as.matrix(dist(dc))[cnt,])[2:(n+1)])
}

simCnt(agrDC_s,'Italy',3)

cluAgr <- data.frame(
  Cluster=1:nClu,
  Description=c('agriculture is an important component in the GDP',
                'high forest area',
                'high extension of permanent cropland (coffee, gum, etc)',
                'high agricultural land',
                'efficient agriculture'),
  NumCountries=kmAgr$size
)

getClu <- function(cnt,km) {return(km$cluster[cnt])}

getClu('Italy',kmAgr)

test <- as.data.frame(t(getClu(names(simCnt(agrDC_s,'Italy',3)),kmAgr)))


test











