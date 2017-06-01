# to run the shiny app

# 01 run the following instructions downloading the the packages requested (change wd) ----
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")

load("ReadData/data.RData")
source('function_extract.R')

library(shiny)
library(rworldmap) # maybe something else is necessary?
library(ggplot2)

# put the function in a source file, put the vectors in the load file
plotCnt <- function(myCnt) {
  map.world <- map_data(map="world")
  map.world$print <- ifelse(map.world$region %in% myCnt,1,0)
  gg <- ggplot()  + 
    geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=print)) + 
    scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar") + 
    coord_equal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  return(gg)
} # maybe there is not corr 1-to-1 with the name of our countries
topics <- c('Agriculture','Natural Resources','Trade','Productivity','Telecom')
lev <- function(x) ifelse(x<=3,'BAD',ifelse(x<=7,'MEDIUM','GOOD'))

# 02 --- run the apps
runApp('Shiny/App_04.R') # the one that starts selecting the country

# the app is only a skeleton, it is far from being definitive
# this work it is finalized to understand what could be done and what it's not feasible in the time we have
