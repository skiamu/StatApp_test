# run App_05
setwd('/Users/mowythebest/Desktop/StatApp_test')
#setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

load("ReadData/data.RData")
load("ReadData/agrData.RData")
load("ReadData/telData.RData")
load("ReadData/TrdData.RData")
load("ReadData/PrdData.RData")
load("ReadData/EtsData.RData")
load("ReadData/EinData.RData")

source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
                                        # kmeansCompare, radarTopic
source('Cluster/function_fda.R')        # fda, fdaPred
source('function_std.R')                # std

library(dplyr)                          # %>%
library(ggplot2)                        # ggplot
library(fmsb)                           # radarchart
library(shiny)                          # runApp

runApp('Shiny/App_05.R')
