# run App_05
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc')

load("ReadData/data.RData")
load("ReadData/agrData.RData")

source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot,
                                        # kmeansCompare, radarTopic

library(dplyr)                          # %>%
library(ggplot2)                        # ggplot
library(fmsb)                           # radarchart
library(shiny)                          # runApp

runApp('Shiny/App_05.R')
