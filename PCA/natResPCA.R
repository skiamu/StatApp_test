# PCA on the natural resources topic
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
myInd <- c('Mineral rents (% of GDP)',
           'Oil rents (% of GDP)',
           'Coal rents (% of GDP)',
           'Natural gas rents (% of GDP)',
           'Forest rents (% of GDP)')
# sum of the previous indicators: 'Total natural resources rents (% of GDP)'

# ---- ---- set years
myYear <- c(2000:2012)

# ---- ---- extract the data
# nrDF = natural resources DataFrame Indicator-like, drop the countries to get a 3D full matrix
nrDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F) %>%
  unifCnt(showInd = F)
# 168 countries selected # length(unique(agrDF$CountryName))
# Significant countries out:
# Sudan, Serbia, Syria, Afghanistan, Iraq ....

# 02 ---- Analysis for a fixed year: 2010 ----
fixedYear <- 2010
# nrDC = perform a dcast on nrDF to get a DataFrame Country-Indicators for 2010
nrDC <- getCntInd(nrDF, fixedYear, dropNA = T, showCnt = T)
# shorten the IndicatorName
colnames(nrDC) <- cleanName(colnames(nrDC))

# Scatterplot - look at the data
sp <- ggpairs(nrDC, upper = list(continuous = wrap("cor", size = 10)), lower = list(continuous = "smooth")) # 
x11(); sp
# COMMENT: the order o measures doesn't differ a lot, but a standardization will be necessary
#          the distribution has a huge peak in 0 with several outliers
#          but we are interest in those outliers

# Boxplot - standardize the data?
nrDC_s <- data.frame(scale(nrDC))
bxp   <- ggplot(stack(nrDC),   aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of raw data')
bxp_s <- ggplot(stack(nrDC_s), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of standardize data') 
x11(); multiplot(bxp + coord_flip(),bxp_s + coord_flip() ,cols = 2)
# COMMENT: better but there are still several outliers

# -----> CHOICE : standardize the data

# perform PCA
nrPCA <- prcomp(nrDC_s)
summary(nrPCA)
# COMMENT: the first 2 PC explain the 54% of the total variability
#          not really happy with that
#          considering the first 3 component should be optimal (74% of the var explained) 

# loadings
x11(); par(mar = c(1,4,0,2), mfrow = c(5,1)); for(i in 1:5) barplot(nrPCA$rotation[,i], ylim = c(-1, 1), las=1); title("Loadings", outer=TRUE)
# COMMENT: 
#          PC.1 : contrast between (gas and oil) and (forest)
#          PC.2 : coal and minerals
#          PC.3 : forest

# Boxplot of the PC
bxp_pc <- ggplot(stack(data.frame(nrPCA$x)), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of principal components') 
x11(); multiplot(bxp_s,bxp_pc,cols = 1)
# COMMENT: dominance of outliers

# Biplot
bip <- PCbiplot(nrPCA) + ggtitle('Biplot') + 
  xlab(do.call(paste,c("PC1 (",as.list(round(agrPCA$sdev[1]^2/sum(agrPCA$sdev^2)*100,2)),"%)",sep=""))) + 
  ylab(do.call(paste,c("PC2 (",as.list(round(agrPCA$sdev[2]^2/sum(agrPCA$sdev^2)*100,2)),"%)",sep="")))
x11(); bip
# COMMENT: 
#          1) cloud of countries with few natural resources around (0,0)
#          2) left:         high oil and gas resuorces
#          3) top-right:    high forest (should consider the PC3)
#          4) bottom-right: high minerals and coals

# 03 ---- Analysis over years: 2000-2011 (the code is far from being nice) ----
#
# COMMENT: the indicators are quite stable in these years,
#          but there is a change in the importance of coal and minerals around 2003:
#          in 2003 coal and minerals switch in the PC1
#          before 2003: coal is more important in the PC1
#          after 2003:  mineral become more important in the PC1
#          opposite situation in PC2
#          

# get 3D matrix
nr3D <- get3D(nrDF, myYear)
# decide what to show
showSum <- F
showLoad <- T
showBi <- T
graphics.off()
nPC <- 4 # cycle over the first 4 PCs
for (j in 1:nPC){
  for (i in 1:length(myYear)) {
    y <- myYear[i]
    colnames(nr3D[[i]]) <- cleanName(colnames(nr3D[[i]]))
    dc_s <- data.frame(scale(nr3D[[i]]))
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
                                c2006,c2007,c2008,c2009,c2010,c2011,c2012)
  colnames(dfLoadPC1) <- c(2000:2012)
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
# 



