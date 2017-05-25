# PCA on the agricultural topic
# last update: 25-05
# Leo

# 00 --- Set working directory, import libraries, functions and data #####

# Setting Working Directory:
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
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


# 01 --- Setting the working dataframe ----

# ---- ---- set indicators
myInd <- c(
  'Agricultural land (% of land area)',
  'Permanent cropland (% of land area)',
  'Arable land (% of land area)',
  'Forest area (% of land area)',
  
  'Agriculture, value added (% of GDP)',
  'Cereal yield (kg per hectare)'
  )
# ---- ---- related indicators not included
# ---- problem in the time depending due to normalization
#'Crop production index (2004-2006 = 100)'
#'Livestock production index (2004-2006 = 100)'
# ---- same information of some selected indicators
#'Agriculture, value added (annual % growth)',
#'Agriculture, value added (current US$)',
#'Forest area (sq. km)',
#'Arable land (hectares per person)',
#'Arable land (hectares)',
#'Agricultural land (sq. km)',
#'Land area (sq. km)',
#'Land under cereal production (hectares)',
#'Forest rents (% of GDP)',
#'Cereal production (metric tons)',
# --- too few countries
#'Population living in areas where elevation is below 5 meters (% of total population)'
#'Land area where elevation is below 5 meters (% of total land area)',
#'Child employment in agriculture (% of economically active children ages 7-14)',
#'Child employment in agriculture, female (% of female economically active children ages 7-14)',
#'Child employment in agriculture, male (% of male economically active children ages 7-14)',
#'Agricultural raw materials exports (% of merchandise exports)',
#'Agricultural raw materials imports (% of merchandise imports)',
#'Agricultural irrigated land (% of total agricultural land)',
#'Employment in agriculture (% of total employment)',
#'Employment in agriculture, female (% of female employment)',
#'Employment in agriculture, male (% of male employment)',
#'Fertilizer consumption (% of fertilizer production)',
#'Fertilizer consumption (kilograms per hectare of arable land)',
# --- only 1990,2000,2005,2008,2010
#'Agricultural methane emissions (% of total)',                                   
#'Agricultural methane emissions (thousand metric tons of CO2 equivalent)',       
#'Agricultural nitrous oxide emissions (% of total)',                             
#'Agricultural nitrous oxide emissions (thousand metric tons of CO2 equivalent)',
# --- only 2014,2012,2007,2002,...
#'Average precipitation in depth (mm per year)',
#'Annual freshwater withdrawals, agriculture (% of total freshwater withdrawal)',
# --- end in 2009
#'Agricultural machinery, tractors',
#'Agricultural machinery, tractors per 100 sq. km of arable land',
# --- duplicates
#'Agriculture, value added (constant 2005 US$)',
#'Agriculture, value added (constant LCU)',
#'Agriculture, value added (current LCU)',
#'Agriculture value added per worker (constant 2005 US$)',

# ---- ---- set years
myYear = c(2000:2011)

# ---- ---- extract the data
# agrDF = agricultural DataFrame Indicator-like, drop the countries to get a 3D full matrix
agrDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F) %>%
  unifCnt(showInd = F)
# 150 countries selected # length(unique(agrDF$CountryName))
# Significant countries out:
# Somalia, Serbia, Libya, Israel, Iraq, Eritrea, Canada, Angola, Afghanistan, ....
#                                                  ^
#                                                  |
#                                       pb with 'Agr, val add'

# 02 ---- Analysis for a fixed year: 2010 ----
fixedYear <- 2010
# agrDC = perform a dcast on agrDF to get a DataFrame Country-Indicators for 2010
agrDC <- getCntInd(agrDF, fixedYear, dropNA = T, showCnt = T)
# shorten the IndicatorName
colnames(agrDC) <- cleanName(colnames(agrDC))

# Scatterplot - look at the data
sp <- ggpairs(agrDC, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth")) # 
x11(); sp
# COMMENT: the order of measures of the variables differs a lot, 
#          it will be necessary to standardize the data

# Boxplot - standardize the data?
agrDC_s <- data.frame(scale(agrDC))
bxp   <- ggplot(stack(agrDC),   aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of raw data')
bxp_s <- ggplot(stack(agrDC_s), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of standardize data') 
x11(); multiplot(bxp + coord_flip(),bxp_s + coord_flip() ,cols = 2)
# COMMENT: the 'cropland' indicator has a lot of outlier, but it is expected since its definition:
#          'Permanent cropland' is land cultivated with crops that occupy the land
#          for long periods and need not be replanted after each harvest, such as cocoa, 
#          coffee, and rubber

# -----> CHOICE : standardize the data

# perform PCA
agrPCA <- prcomp(agrDC_s)
summary(agrPCA)
# COMMENT: the first 2 PC explain the 56% of the total variability
#          not really happy with that
#          considering the first 3 component should be optimal (74% of the var explained) 

# A possible way to display the cumulated variance
#cumVar <- data.frame(cumulated.variance=(agrPCA$sdev^2)/sum(agrPCA$sdev^2),
#                     num.PC=c(1:length(agrPCA$sdev)),
#                     x=rep(' ',length(agrPCA$sdev)))
#library(scales)
#vp <- ggplot(cumVar,aes(x = x, y = cumulated.variance,fill = num.PC,
#                        label = round(cumulated.variance*100,2), width = .1)) + 
#  geom_bar(position = "fill",stat = "identity") +
#  scale_y_continuous(labels = percent_format()) +
#  geom_text(size = 3, position = position_stack(vjust = 0.5), colour = 'white') +
#  coord_flip()
#x11(); vp

# loadings
x11(); par(mar = c(1,4,0,2), mfrow = c(5,1)); for(i in 1:5) barplot(agrPCA$rotation[,i], ylim = c(-1, 1), las=1); title("Loadings", outer=TRUE)
# COMMENT: 
#          PC.1 : contrast between (forest area) and (agricultural land, arable land)
#          PC.2 : contrast (agricultural value added) and (cereal yield)
#          PC.3 : permanent cropland 

# Boxplot of the PC
bxp_pc <- ggplot(stack(data.frame(agrPCA$x)), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of principal components') 
x11(); multiplot(bxp_s,bxp_pc,cols = 1)
# COMMENT: PC1, PC2 are not so dominant w.r.t.the others
#          PC3 is still important due to outliers (countries with high permanent cropland)

# Biplot
bip <- PCbiplot(agrPCA) + ggtitle('Biplot') + 
  xlab(do.call(paste,c("PC1 (",as.list(round(agrPCA$sdev[1]^2/sum(agrPCA$sdev^2)*100,2)),"%)",sep=""))) + 
  ylab(do.call(paste,c("PC2 (",as.list(round(agrPCA$sdev[2]^2/sum(agrPCA$sdev^2)*100,2)),"%)",sep="")))
x11(); bip
# COMMENT: 
#          1) this plot should be watched taking into account that PC3 is significative
#          2) top-right   : efficient agriculture
#             top-left    : agriculture is a significant part in GDP
#             bottom-right: vast forest land  (in % of land area)
#             bottom-left : vast agricultural (in % of land area)
#          3) vast permanent cropland cannot be seen easily with a biplot,
#             since it is maily represented in the PC3. 
#             however I am expecting they are in the middle

# 03 ---- Analysis over years: 2000-2011 (the code is far from being nice) ----
#
# COMMENT: the indicators are quite stable in these years

# get 3D matrix
agr3D <- get3D(agrDF, myYear)
# decide what to show
showSum <- F
showLoad <- F
showBi <- T
graphics.off()
nPC <- 4 # cycle over the first 4 PCs
for (j in 1:nPC){
  for (i in 1:length(myYear)) {
    y <- myYear[i]
    colnames(agr3D[[i]]) <- cleanName(colnames(agr3D[[i]]))
    dc_s <- data.frame(scale(agr3D[[i]]))
    pca  <- prcomp(dc_s)
    if(showSum){print(y); print(summary(pca))}
    
    if(showLoad){
      x11(); 
      par(mar = c(1,4,0,2), mfrow = c(5,1)); 
      for(i in 1:5) barplot(pca$rotation[,i], ylim = c(-1, 1), las=1); 
      title(paste("Loadings in",y), outer=TRUE)
    }
    
    s <- paste('c',y,sep="")
    assign(s, pca$rotation[,j])
      
    if(showBi){
      bip[[i]] <- PCbiplot(pca) + ggtitle(paste("Biplot in",y)) + 
        xlab(do.call(paste,c("PC1 (",as.list(round(pca$sdev[1]^2/sum(pca$sdev^2)*100,2)),"%)",sep=""))) + 
        ylab(do.call(paste,c("PC2 (",as.list(round(pca$sdev[2]^2/sum(pca$sdev^2)*100,2)),"%)",sep="")))
      #x11(); print(bip[[i]])
    }
    
  }

  dfLoadPC1 <- cbind.data.frame(c2000,c2001,c2002,c2003,c2004,c2005,
                                c2006,c2007,c2008,c2009,c2010,c2011)
  colnames(dfLoadPC1) <- c(2000:2011)
  dfLoadPC1$ind <- row.names(dfLoadPC1) 
  melted = melt(dfLoadPC1, id.vars="ind")
  melted$variable <- as.numeric(as.character(melted$variable))
  
  p <- ggplot(melted, aes(x=variable,y=abs(value),color=ind)) + geom_point() +geom_line() + ggtitle(paste('Loadings of PC',j,sep=''))
  #p <- ggplot(melted, aes(x=variable,y=value,color=ind)) + geom_point() +geom_line() + ggtitle(paste('Loadings of PC',j,sep=''))

  s <- paste('PC',j,sep="")
  assign(s, p)
}
x11(); multiplot(bip[[1]],bip[[2]],bip[[3]],bip[[4]],bip[[5]],bip[[6]],
                 bip[[7]],bip[[8]],bip[[9]],bip[[10]],bip[[11]],bip[[12]], cols = 4)

x11(); multiplot(PC1,PC2,PC3,PC4,cols = 2)  
  

