# run App_05
#setwd('/Users/mowythebest/Desktop/StatApp_test')
#setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')
setwd( "C:/Users/Leona/Documents/StatApp_test")

# what to install:
# 1) reshape2
# 2) dplyr
# 3) ggplot2
# 4) fmsb
# 5) shiny
# 6) maps

load("ReadData/data.RData")
load("ReadData/agrData.RData")
load("ReadData/NatData.RData")
load("ReadData/telData.RData")
load("ReadData/TrdData.RData")
load("ReadData/PrdData.RData")
load("ReadData/EtsData.RData")
load("ReadData/EinData.RData")
load("ReadData/prediction.RData") 

source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
                                        # kmeansCompare, radarTopic

source("Graphs&Plots/plotPred.R")       # plot10yPred, checkPred
source('Cluster/function_fda.R')        # fda, fdaPred
source('function_std.R')                # std

library(dplyr)                          # %>%
library(ggplot2)                        # ggplot
library(fmsb)                           # radarchart
library(shiny)                          # runApp
# library(ggthemes)                      # theme_economist() maybe
select <- dplyr::select 


runApp('Shiny/App_05.R')
