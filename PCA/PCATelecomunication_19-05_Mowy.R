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

# TeleMatrix <- find_outlier(TeleMatrix, remove = T) 
# Remark: THERE ARE OUTLIERS: working without them means a 72,21% covered by the first compo, the second is 
#                             almost the same of with outilers. The loadings are the same!
#                             working with them means 64,21% covered by the first, the others the same of no
#                             outlier. Same Loadings. The boxplot of the second has a lot of outliers.

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
graphics.off()

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
for(i in 1:2) barplot(TeleLoad[,i], ylim = c(-1, 1), las=1)

# Interpretation of the loadings:
# First PCs: mean of actual communication (neg) VS mean of population idicators (pos)
# Second PCs: Population growth and new technlogies (all neg) (havier the growth)

# Explained variance
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

# Scores: repr of the original data in the space PC1 PC2
TeleScores <- TelePCA$scores  

layout(matrix(c(1,2),2))
boxplot(TeleMatrixStd, las=2, col='red', main='Original variables')
TeleScores <- data.frame(TeleScores)
boxplot(TeleScores, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(TelePCA, scale=0, cex=.7)

# INTERPRETAZIONE: the more on the right the more the growth of the pop overtakes the actual telecom
#                  the more on the left the more the actual telcom satisfy the actual and the next pop
#                  the upper the more neg growth than advanced tech or small growth and small difussion of advanced tech
#                  the lower the more advanced communication and growth are present (and if growth is neg the comm are super diffused)
# Middle east is super advanced but the growth of the pop is still more
# Lituania and Latvia No gorwth of the pop and communication are advanced
# Cuba and Samoa is like upper in the discription of before  (no left or right)
# African States are really alot of pop growth and no communications
# West Europa and Hong kong China are good tech e no much growth of pop
# east europa like Lituania and latvia but less advanced tech and more growth
# latino american countrie on the 0 of every compo
#
# x11()
# PCbiplot(prcomp(scale(TeleMatrix)))