# PCA on the Trade topic: GOOD DIM REDUCTION
# last update: 25-05
# Mowy

# 00 --- Set working directory, import libraries, functions and data #####

# Setting Working Directory:
setwd('/Users/mowythebest/Desktop/StatApp_test')

# Functions:
source('Filters/functionsFullMatrix.R')
source("function_extract.R")
source("outlier.R")
source("PCA/PCA_function.R")

# Data:
load("ReadData/data.RData")

# Libraries
library(dplyr)

# 01 --- Setting the working dataframe ----

# REMARK: HOW I SELECT INDICATORS BEFORE APPLING PCA
# Indicators of this topic: 
# "Export value index (2000 = 100)",
# "Import value index (2000 = 100)",
# "Merchandise trade (% of GDP)",
# "Cost to export (US$ per container)",
# "Cost to import (US$ per container)",
# "Documents to export (number)",
# "Documents to import (number)",
# "Exports of goods and services (% of GDP)",
# "Imports of goods and services (% of GDP)",
# "Time to export (days)",
# "Time to import (days)",
# "Trade (% of GDP)",
# "Service exports (BoP, current US$)",
# "Service imports (BoP, current US$)",
# "Trade in services (% of GDP)"
# TOGLIERE ULTIME TRE MIGLIORA VAR SPIEGATA però è analisi diversa
# ha anche senso lavorare solo con indicatori contrasto imp exp

# I want to keep the fullest M indicators
M <- 400
# Countries with more than Tind indicators in a specified year
# Select all the Topics directly or inderectly realted to telecommunication
# Remark: Has any meaning to consider the inderected in the PCA? or they affect only regression?
df <- extract2DmatrixWithFullestIndicators(Indicators,
                                           M,
                                           viewFlag=FALSE,
                                           Tind = 400)

# Set countries:
df <- filter(df, df$CountryName != "USA") # Usa crea biplot troppo largo
myCnt <- df$CountryName %>% unique()
# Set indicators:
myInd <- c("Export value index (2000 = 100)",
           "Import value index (2000 = 100)",
           "Merchandise trade (% of GDP)",
           "Cost to export (US$ per container)",
           "Cost to import (US$ per container)",
           "Documents to export (number)",
           "Documents to import (number)",
           "Exports of goods and services (% of GDP)",
           "Imports of goods and services (% of GDP)",
           "Time to export (days)",
           "Time to import (days)",
           "Trade (% of GDP)")
           # "Service exports (BoP, current US$)",
           # "Service imports (BoP, current US$)",
           # "Trade in services (% of GDP)")

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
TradeDF <- getIndicators(myYear = myYear, myCnt = myCnt, myInd = myInd, ind = df)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
TradeMatrix <- getCntInd(TradeDF, myYear, dropNA = T, showCnt = T)
colnames(TradeMatrix) <- c(
"Exp ind",
"Imp ind",
"Merch trade",
"Cost exp",
"Cost imp",
"Doc exp",
"Doc imp",
"Exp goods&services",
"Imp goods&services",
"Time exp",
"Time imp",
"Trade")
# "Exp ser",
# "imp serv",
# "trade serv")
#TradeMatrix <- find_outlier(TradeMatrix, remove = T) 
# Remark: THERE ARE OUTLIERS: working without them means a 72,21% covered by the first compo, the second is 
#                             almost the same of with outilers. The loadings are the same!
#                             working with them means 64,21% covered by the first, the others the same of no
#                             outlier. Same Loadings. The boxplot of the second has a lot of outliers.

# 02 --- Pre-analysis ----

# Scatter Plots Std
x11()
plot(TradeMatrix)

# Boxplot Std
x11()
boxplot(TradeMatrix)

# We compute the standardized variables
TradeMatrixStd <- data.frame(scale(TradeMatrix))

# Scatter Plots Std
x11()
plot(TradeMatrixStd)

# Boxplot Std
x11()
boxplot(TradeMatrixStd)

graphics.off()


# 03 --- PCA on std data ----

TradePCA <- princomp(TradeMatrixStd, scores=T)
TradePCA
summary(TradePCA)

# Loadings 

TradeLoad <- TradePCA$loadings
TradeLoad

# graphical representation of the loadings of the first three principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(4,1))
for(i in 1:4) barplot(TradeLoad[,i],las=2, ylim = c(-1, 1))

# Interpretation of the loadings 15 compo:
# + difificle ma possibile
# Interpretation of the loadings 12 compo:
# - difficile
# Interpretation of the loadings - anche le due di trade:
# la più sensata di interpr
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(TradePCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(TradeMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(TradePCA$sdev^2)/sum(TradePCA$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(TradeMatrixStd),labels=1:ncol(TradeMatrixStd),las=2)

# The first four PC explains more than 80% of the total variability.

# Scores: repr of the original data in the space PC1 PC2
TradeScores <- TradePCA$scores  
x11()
layout(matrix(c(1,2),2))
boxplot(TradeMatrixStd, las=2, col='red', main='Original variables')
TradeScores <- data.frame(TradeScores)
boxplot(TradeScores, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(TradePCA, scale=0, cex=.7)

#Interpretaz per clustering
# x11()
# PCbiplot(prcomp(scale(TeleMatrix)))

# 04 --- PCA along different years ----

# Without acces to electricity it work super good!

myInd <- c("Export value index (2000 = 100)",
           "Import value index (2000 = 100)",
           "Merchandise trade (% of GDP)",
           "Cost to export (US$ per container)",
           "Cost to import (US$ per container)",
           "Documents to export (number)",
           "Documents to import (number)",
           "Exports of goods and services (% of GDP)",
           "Imports of goods and services (% of GDP)",
           "Time to export (days)",
           "Time to import (days)",
           "Trade (% of GDP)")
# "Service exports (BoP, current US$)",
# "Service imports (BoP, current US$)",
# "Trade in services (% of GDP)")

#mettere o togliere questi ultimi tre cambia proprio analisi
# keep only the countries with at least Tind indicators in 2010
myCnt <- df$CountryName %>% unique() 
# years
myYears <- c(2006:2010)

# filter Indicators
indF <- getIndicators(myYear = myYears, 
                      myCnt = myCnt, 
                      myInd = myInd)

indFull <- unifCnt(indF) # it's Indicators-like with the correspondent 3D matrix is Full

# get 3D matrices
indFull3D <- get3D(indFull, myYears) # full

for (i in 1:length(myYears)) {
  # We compute the standardized variables
  TradeMatrixStd <- data.frame(scale(indFull3D[[i]]))
  
  TradePCA <- princomp(TradeMatrixStd, scores=T)
  TradePCA
  summary(TradePCA)
  # To obtain the rows of the summary: -standard deviation of the components TelePCA$sd
  #                                    -proportion of variance explained by each PC TelePCA^2/sum(TelePCA^2)
  #                                    -cumulative proportion of explained variance cumsum(TelePCA$sd^2)/sum(TelePCA$sd^2)
  
  # Loadings 
  
  TradeLoad <- TradePCA$loadings
  TradeLoad
  
  # graphical representation of the loadings of the first three principal components
  x11()
  par(mar = c(1,4,0,2), mfrow = c(3,1))
  for(j in 1:2) barplot(TradeLoad[,j], ylim = c(-1, 1), las=1)
  
  # Interpretation of the loadings:
  # First PCs: mean of actual communication (neg) VS mean of population idicators (pos)
  # Second PCs: Population growth and new technlogies (all neg) (havier the growth)
  
  # Explained variance
  x11()
  layout(matrix(c(2,3,1,3),2,byrow=T))
  barplot(TradePCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
  abline(h=1, col='blue')
  barplot(sapply(TradeMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
  plot(cumsum(TradePCA$sdev^2)/sum(TradePCA$sde^2), type='b', axes=F, xlab='number of components',
       ylab='contribution to the total variance', ylim=c(0,1))
  box()
  axis(2,at=0:10/10,labels=0:10/10)
  axis(1,at=1:ncol(TradeMatrixStd),labels=1:ncol(TradeMatrixStd),las=2)
  x11()
  biplot(TradePCA, scale=0, cex=.7)
}
# From 1995 to 2010 (maybe I can extent more) loosing some countries the loadings and the var explained
# are always the same!
