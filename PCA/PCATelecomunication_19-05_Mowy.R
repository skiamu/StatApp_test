# PCA on the telecommunication topic
# last update: 19-05
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
# 01 --- Setting the working dataframe ----

# REMARK: HOW I SELECT INDICATORS BEFORE APPLING PCA
# Indicators of this topic: "Access to electricity (% of population)", 
#                           "Population, total",
#                           "Population growth (annual %)",
#                           "Fixed telephone subscriptions (per 100 people)",
#                           "Mobile cellular subscriptions (per 100 people)",
#                           "Urban population growth (annual %)",
#                           "Urban population growth (annual %)",
#                           "Internet users (per 100 people)",
#                           "Secure Internet servers (per 1 million people)",
#                           "Fixed broadband subscriptions".

# Excluding the last: two indicators I neglect 2 countries (Important? Kosovo, Somalia)
#                     one indicator I neglect 5 (Important?: Central African Republic, Chad, Eritrea)                 
#                     no one I neglect 9 (Important?: Haiti, Ivory Coast, Sierra Leone, Gaza)

# After Scatter plots I exclude "Fixed broadband subscriptions"  because linerly correlated with the others (good also for the countries excluded)
# The same for "Population Total", "Secure Internet servers (per 1 million people)".

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
myInd <- c("Access to electricity (% of population)",
           "Population growth (annual %)",
           "Fixed telephone subscriptions (per 100 people)",
           "Mobile cellular subscriptions (per 100 people)",
           "Urban population growth (annual %)",
           "Urban population growth (annual %)",
           "Internet users (per 100 people)"
           )

# Set the year:
myYear <- c(2010)

# TeleDF = Telecomunication DataFrame
TeleDF <- getIndicators(myYear = myYear, myCnt = myCnt, myInd = myInd, ind = df
#                    clear_name = T,
                    )

# TeleMatrix = Matrix Country-Indicators created from TeleDF
TeleMatrix <- getCntInd(TeleDF, myYear, dropNA = T, showCnt = T)

# w <- find_outlier(TeleMatrix, remove = T) Remark: THERE ARE OUTLIERS

# 02 --- Pre-analysis ----

# Scatter Plots Std
x11()
plot(TeleMatrix)

# Boxplot Std
x11()
boxplot(TeleMatrix)

# We compute the standardized variables
TeleMatrixStd <- data.frame(scale(TeleMatrix))

# Scatter Plots Std
x11()
plot(TeleMatrixStd)

# Boxplot Std
x11()
boxplot(TeleMatrixStd)

# IT IS CLEAR THE NEED TO STANDARDIZE THE VALUES
# TOO MUCH OUTLIERS IN THE FIFTH VARIABLE

# 03 --- PCA on original data ----

TelePCA <- princomp(TeleMatrixStd, scores=T)
TelePCA
summary(TelePCA)
# To obtain the rows of the summary: -standard deviation of the components TelePCA$sd
#                                    -proportion of variance explained by each PC TelePCA^2/sum(TelePCA^2)
#                                    -cumulative proportion of explained variance cumsum(TelePCA$sd^2)/sum(TelePCA$sd^2)

# Loadings 

TeleLoad <- TelePCA$loadings
TeleLoad

# graphical representation of the loadings of the first three principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(3,1))
for(i in 1:3) barplot(TeleLoad[,i], ylim = c(-1, 1), las=1)

# Interpretation of the loadings:
# First PCs: mean of the strict communication VS the population idicators
# Second PCs: Population growth and new technlogies
# Third PCs: I have still to find an idea to interpret it

#################################### Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(TelePCA$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(TeleMatrixStd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(TelePCA$sdev^2)/sum(TelePCA$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(TeleMatrixStd),labels=1:ncol(TeleMatrixStd),las=2)

# The first two PC explains more than 82.87% of the total variability. 
# Da interpretare
# Scores: repr of the original data in the space PC1 PC2
TeleScores <- TelePCA$scores  

layout(matrix(c(1,2),2))
boxplot(TelePCA, las=2, col='red', main='Original variables')
TeleScores <- data.frame(TeleScores)
boxplot(TeleScores, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(TelePCA, scale=0, cex=.7)