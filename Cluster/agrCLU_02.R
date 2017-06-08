# cluster on agriculture

# 00 preliminars ----
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")

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

