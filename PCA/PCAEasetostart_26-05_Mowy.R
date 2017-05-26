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
myCnt <- df$CountryName %>% unique()
# Set indicators:
myInd <- c(#"Cost to export (US$ per container)",
           # "Cost to import (US$ per container)", 
           # "Documents to export (number)",
           # "Documents to import (number)",
           # "Time to export (days)",
           # "Time to import (days)",
           "Cost of business start-up procedures (% of GNI per capita)",
           "Start-up procedures to register a business (number)",
           "Time required to start a business (days)",
           "Time to prepare and pay taxes (hours)",
           "Total tax rate (% of commercial profits)")

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
StartEcoDF <- getIndicators(myYear = myYear, myCnt = myCnt, myInd = myInd, ind = df)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
StartEcoMatrix <- getCntInd(StartEcoDF, myYear, dropNA = T, showCnt = T)
colnames(StartEcoMatrix) <- c(#"Cost exp ($ container)",
                          # "Cost imp ($ container)",
                          # "Docu exp",
                          # "Doc imp",
                          # "Time exp",
                          # "Time imp",
                          "Cost start-up proced (%GNI capita)",
                          "Start-up procedures",
                          "Time start a business (days)",
                          "Time prepare/pay taxes (hours)",
                          "Total tax rate (%profits)")
#TradeMatrix <- find_outlier(TradeMatrix, remove = T) 


# 02 --- Pre-analysis ----

# Scatter Plots Std
x11()
plot(StartEcoMatrix)

# Boxplot Std
x11()
boxplot(StartEcoMatrix)

# We compute the standardized variables
StartEcoMatrixStd <- data.frame(scale(StartEcoMatrix))

# Scatter Plots Std
x11()
plot(StartEcoMatrixStd)

# Boxplot Std
x11()
boxplot(StartEcoMatrixStd)

graphics.off()


# 03 --- PCA on std data ----

StartEcoPCA <- princomp(StartEcoMatrixStd, scores=T)
StartEcoPCA
summary(StartEcoPCA)

# Loadings 

StartEcoLoad <- StartEcoPCA$loadings
StartEcoLoad

# graphical representation of the loadings of the first three principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(StartEcoLoad[,i],las=2, ylim = c(-1, 1))
# ANALISI SENZA TRADE
# I comp= ind e services vs manifact
# II comp = manufact e serv (small ind) VS agr
# ANALISI CON TRADE
# I comp= ind&agr vs manifact&trade
# II comp = ind&servVS agr
# III comp = serv contro trade

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(StartEcoPCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(StartEcoMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(StartEcoPCA$sdev^2)/sum(StartEcoPCA$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(StartEcoMatrixStd),labels=1:ncol(StartEcoMatrixStd),las=2)
# analisi senza trade
# The first two PC explains more than 80% of the total variability.
#analisi con trade: prendi 3 compo hai 90%

# Scores: repr of the original data in the space PC1 PC2
StartEcoScores <- StartEcoPCA$scores  
x11()
layout(matrix(c(1,2),2))
boxplot(StartEcoMatrixStd, las=2, col='red', main='Original variables')
StartEcoScores <- data.frame(StartEcoScores)
boxplot(StartEcoScores, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(StartEcoPCA, scale=0, cex=.7)
#ANALISI SENZA TRADE
# the upper the more devenlop agricolture
# the more left the more industrial and serv
# the more right the more manufact
# the lower the more serv and ind and manifact
# so upper : African, west asia, nepal
# left upp middle devenlop african and asian and the lower the most devenlop of africa and asia
# between serv and ind China and switz
# most of the OECD chenter down and right down between manifuct and serv or manufact and agric
# ANALISI CON TRADE
# the right the more agric ind the left the more manif and trade
# the upper the more ind and serv the lower the more agr
# African and west asia agric
# manif: develpt asia and luxemburg
# trade all europe
# prevalence serv: euro and south korea and a lot of trade and ind (south korea exaclty in the 0)
# ind and a lor of serv: China and middle east
# from ind to agric: venez indonesia and all african

# x11()
# PCbiplot(prcomp(scale(TeleMatrix)))

# 04 --- PCA along different years ----

# Without acces to electricity it work super good!

myInd <- c("Agriculture, value added (% of GDP)",
           "Industry, value added (% of GDP)",
           "Services, etc., value added (% of GDP)",
           "Manufacturing, value added (% of GDP)")

#mettere o togliere questi ultimi tre cambia proprio analisi
# keep only the countries with at least Tind indicators in 2010
myCnt <- df$CountryName %>% unique() 
# years
myYears <- c(2000:2009)

# filter Indicators
indF <- getIndicators(myYear = myYears, 
                      myCnt = myCnt, 
                      myInd = myInd)

indFull <- unifCnt(indF) # it's Indicators-like with the correspondent 3D matrix is Full

# get 3D matrices
indFull3D <- get3D(indFull, myYears) # full

for (i in 1:length(myYears)) {
  # We compute the standardized variables
  ProdMatrixStd <- data.frame(scale(indFull3D[[i]]))
  
  ProdPCA <- princomp(ProdMatrixStd, scores=T)
  ProdPCA
  summary(ProdPCA)
  # To obtain the rows of the summary: -standard deviation of the components TelePCA$sd
  #                                    -proportion of variance explained by each PC TelePCA^2/sum(TelePCA^2)
  #                                    -cumulative proportion of explained variance cumsum(TelePCA$sd^2)/sum(TelePCA$sd^2)
  
  # Loadings 
  
  TradeLoad <- ProdPCA$loadings
  TradeLoad
  
  # graphical representation of the loadings of the first three principal components
  # x11()
  # par(mar = c(1,4,0,2), mfrow = c(3,1))
  # for(j in 1:2) barplot(TradeLoad[,j], ylim = c(-1, 1), las=1)
  # 
  # # Interpretation of the loadings:
  # # First PCs: mean of actual communication (neg) VS mean of population idicators (pos)
  # # Second PCs: Population growth and new technlogies (all neg) (havier the growth)
  # 
  # # Explained variance
  # x11()
  # layout(matrix(c(2,3,1,3),2,byrow=T))
  # barplot(ProdPCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
  # abline(h=1, col='blue')
  # barplot(sapply(ProdMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
  # plot(cumsum(ProdPCA$sdev^2)/sum(ProdPCA$sde^2), type='b', axes=F, xlab='number of components',
  #      ylab='contribution to the total variance', ylim=c(0,1))
  # box()
  # axis(2,at=0:10/10,labels=0:10/10)
  # axis(1,at=1:ncol(ProdMatrixStd),labels=1:ncol(ProdMatrixStd),las=2)
  x11()
  biplot(ProdPCA, main=myYears[i], scale=0, cex=.7)
}
# From 2000 to 2010  posso davvero tornare molto indietro, cmq sballano tanto ma i cluster gli stessi