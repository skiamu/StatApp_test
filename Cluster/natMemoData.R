# cluster on Natural resources

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

# Set indicators:
myInd.Nat <- c('Mineral rents (% of GDP)',
           'Oil rents (% of GDP)',
           'Coal rents (% of GDP)',
           'Natural gas rents (% of GDP)',
           'Forest rents (% of GDP)')

# Set the year:
myYear <- c(2010)

# NatDF = Trade DataFrame
NatDF <- getIndicators(myYear = myYear, myInd = myInd.Nat, agg = F)
NatDF <- filter(NatDF, NatDF$CountryName != "Mongolia")

# NatMatrix = Matrix Country-Indicators created from NatDF
DC.Nat <- getCntInd(NatDF, myYear, dropNA = T, showCnt = T)

# shorten the IndicatorName
colnames(DC.Nat) <- cleanName(colnames(DC.Nat))

# stdize data
sc <- scale(DC.Nat)
DCs.Nat  <- data.frame(sc)
mean.Nat <- attributes(sc)$'scaled:center'
var.Nat  <- attributes(sc)$'scaled:scale'

# cluster analysis
#x11(); kmeansCompare(DCs.Nat)
nClu.Nat <- 5
set.seed(2000) # so the interpretation doesn't change
km.Nat <- kmeans(DCs.Nat, nClu.Nat, nstart = 100) # see NatcomCLU to see why we choose 5 clusters
#km.Nat$size
#x11(); plotClusterMap(km.Nat$cluster, nClu.Nat)
#x11(); radarTopic(DCs.Nat, km.Nat)

# comments on clusters
clu.Nat <- data.frame( 
  Cluster=1:nClu.Nat,
  Description=c("Forest rents important in the GDP",
                "Coal rents important in the GDP",
                "Oil and gas rents important in the GDP",
                "Natural resources do not represent a big part in the GDP",
                "Mineral rents important in the GDP"),
  NumCountries=km.Nat$size
)

# fda
nFiComp.Nat <- 4
fda.Nat   <-         fda(DCs.Nat, km.Nat$cluster, as.character(1:nClu.Nat), nFiComp.Nat)
aercv.Nat <- crossValFDA(DCs.Nat, km.Nat$cluster, as.character(1:nClu.Nat), nFiComp.Nat)
# technical details
recap.Nat <- paste('Data have been standardized, cluster analysis via kmneans performed in 2010, prediction using Fisher discriminant analysis (',
                   nFiComp.Nat,'components) with an Apparent Error Rate',
                   aercv.Nat,'(via Cross Validation)')

# save
save(nClu.Nat,clu.Nat,km.Nat,
     DCs.Nat,mean.Nat,var.Nat,myInd.Nat,
     fda.Nat,
     recap.Nat,
     file = "ReadData/NatData.RData")
