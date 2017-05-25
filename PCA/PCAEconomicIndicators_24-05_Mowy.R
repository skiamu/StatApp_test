# PCA on the Economic indicators topic
# last update: 24-05
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
# Indicators of this topic: "Foreign direct investment, net inflows (BoP, current US$)", 
#                           "GDP at market prices (current US$)",
#                           "GDP per capita (current US$)",
#                           "GDP growth (annual %)",
#                           "Inflation, GDP deflator (annual %)",
#                           "Price level ratio of PPP conversion factor (GDP) to market exchange rate",
#                           "Inflation, consumer prices (annual %)"
# Remove Price ebecause same info of infliatio costumer prices
# After Scatter plots WATCH OUT! foreign investment, gdp at mkt price and inflation with gdp deflator are linearly dep

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
myInd <- c("Foreign direct investment, net inflows (BoP, current US$)",
           "GDP at market prices (current US$)",
           "GDP per capita (current US$)",
           "GDP growth (annual %)",
           "Inflation, GDP deflator (annual %)",
           "Inflation, consumer prices (annual %)")

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
EcoIndDF <- getIndicators(myYear = myYear, myCnt = myCnt, myInd = myInd, ind = df)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
EcoIndMatrix <- getCntInd(EcoIndDF, myYear, dropNA = T, showCnt = T)
colnames(EcoIndMatrix) <- c("Foreign investment",
                 "GDP",
                 "GDP per capita",
                 "GDP growth",
                 "Inflation, GDP deflator",
                 "Inflation mkt price")
#EcoIndMatrix <- find_outlier(EcoIndMatrix, remove = T) 
# Remark: THERE ARE OUTLIERS: working without them means a 72,21% covered by the first compo, the second is 
#                             almost the same of with outilers. The loadings are the same!
#                             working with them means 64,21% covered by the first, the others the same of no
#                             outlier. Same Loadings. The boxplot of the second has a lot of outliers.

# 02 --- Pre-analysis ----

# Scatter Plots Std
x11()
plot(EcoIndMatrix)

# Boxplot Std
x11()
boxplot(EcoIndMatrix)

# We compute the standardized variables
EcoIndMatrixStd <- data.frame(scale(EcoIndMatrix))

# Scatter Plots Std
x11()
plot(EcoIndMatrixStd)

# Boxplot Std
x11()
boxplot(EcoIndMatrixStd)

graphics.off()


# 03 --- PCA on std data ----

EcoIndPCA <- princomp(EcoIndMatrixStd, scores=T)
EcoIndPCA
summary(EcoIndPCA)

# Loadings 

EcoIndLoad <- EcoIndPCA$loadings
EcoIndLoad

# graphical representation of the loadings of the first three principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(EcoIndLoad[,i], ylim = c(-1, 1), las=1)

# Interpretation of the loadings:
# First PC: GDP + foreign indicator VS inflations
# Second PC: Countries with high GDP and Inflation, but trustuble for foregn investments
# Third PC: High Gdp per capita

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(EcoIndPCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(EcoIndMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(EcoIndPCA$sdev^2)/sum(EcoIndPCA$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(EcoIndMatrixStd),labels=1:ncol(EcoIndMatrixStd),las=2)

# The first two PC explains more than 82.87% of the total variability.

# Scores: repr of the original data in the space PC1 PC2
EcoIndScores <- EcoIndPCA$scores  
x11()
layout(matrix(c(1,2),2))
boxplot(EcoIndMatrixStd, las=2, col='red', main='Original variables')
EcoIndScores <- data.frame(EcoIndScores)
boxplot(EcoIndScores, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(EcoIndPCA, scale=0, cex=.7)

# INTERPRETAZIONE: the more on the right the more inflation
#                  the more on the left the more strong and growing GDP and foregn investments
#                  the lower the more higher gdp and inflation but still trustable
# In alto a sx scandinavi e svizzera e hong kong 
# in bassino a super sx Paesi con economie più avanzate (ci sarebbe super a sx USA) (USA China Canada Australia, jappone Germania)
# super in basso a dx fortix inflaz
# al centro in basso paesi con alto gdp ma economia debole per cittadini(alta inflaz): Russia brasile india
# al centro poco sopra paesi altix pil percapite middle east
# al centro più in alto leggerm verso dx quelle più debole : asia e africa e più vado a dx più economie complicate ciclo inflation
# al centro più in alto ma verso sx paesi europei e altri tipo israele che sono forti ma senza eccellere troppo
# x11()
# PCbiplot(prcomp(scale(TeleMatrix)))

# 04 --- PCA along different years ----

# Without acces to electricity it work super good!

myInd <- c("Foreign direct investment, net inflows (BoP, current US$)",
           "GDP at market prices (current US$)",
           "GDP per capita (current US$)",
           "GDP growth (annual %)",
           "Inflation, GDP deflator (annual %)")

# keep only the countries with at least Tind indicators in 2010
myCnt <- df$CountryName %>% unique() 
# years
myYears <- c(1995:2010)

# filter Indicators
indF <- getIndicators(myYear = myYears, 
                      myCnt = myCnt, 
                      myInd = myInd)

indFull <- unifCnt(indF) # it's Indicators-like with the correspondent 3D matrix is Full

# get 3D matrices
indFull3D <- get3D(indFull, myYears) # full

for (i in 1:length(myYears)) {
  # We compute the standardized variables
  EcoIndMatrixStd <- data.frame(scale(indFull3D[[i]]))
  
  EcoIndPCA <- princomp(EcoIndMatrixStd, scores=T)
  EcoIndPCA
  summary(EcoIndPCA)
  # To obtain the rows of the summary: -standard deviation of the components TelePCA$sd
  #                                    -proportion of variance explained by each PC TelePCA^2/sum(TelePCA^2)
  #                                    -cumulative proportion of explained variance cumsum(TelePCA$sd^2)/sum(TelePCA$sd^2)
  
  # Loadings 
  
  EcoIndLoad <- EcoIndPCA$loadings
  EcoIndLoad
  
  # graphical representation of the loadings of the first three principal components
  x11()
  par(mar = c(1,4,0,2), mfrow = c(3,1))
  for(j in 1:2) barplot(EcoIndLoad[,j], ylim = c(-1, 1), las=1)
  
  # Interpretation of the loadings:
  # First PCs: mean of actual communication (neg) VS mean of population idicators (pos)
  # Second PCs: Population growth and new technlogies (all neg) (havier the growth)
  
  # Explained variance
  x11()
  layout(matrix(c(2,3,1,3),2,byrow=T))
  barplot(EcoIndPCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
  abline(h=1, col='blue')
  barplot(sapply(EcoIndMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
  plot(cumsum(EcoIndPCA$sdev^2)/sum(EcoIndPCA$sde^2), type='b', axes=F, xlab='number of components',
       ylab='contribution to the total variance', ylim=c(0,1))
  box()
  axis(2,at=0:10/10,labels=0:10/10)
  axis(1,at=1:ncol(EcoIndMatrixStd),labels=1:ncol(EcoIndMatrixStd),las=2)
  x11()
  biplot(EcoIndPCA, scale=0, cex=.7)
}
# From 1995 to 2010 (maybe I can extent more) loosing some countries the loadings and the var explained
# are always the same!
