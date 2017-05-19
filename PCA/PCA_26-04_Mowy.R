# A try to perform PCA: add merge and filtering on it
# last update: 26-04
# Mowy

# INPUTS 
# 00 ------- Set working directory and import libraries#####
setwd('/Users/mowythebest/Desktop/StatApp_test')
# 01. PCA on telecomunication indicators ####
ctrindPCA <- cast(dfind0, CountryName + CountryCode ~ IndicatorName, value = "Value")
# Boxplot
x11()
boxplot(ctrindPCA[,3:ncol(ctrindPCA)])
x11(width=10, height=7)
par(mfrow=c(2,3))
boxplot(ctrindPCA[,3])
boxplot(ctrindPCA[,4])
boxplot(ctrindPCA[,5])
boxplot(ctrindPCA[,6])
boxplot(ctrindPCA[,7])
boxplot(ctrindPCA[,8])
# We observe a lot of outliers and to much incoherent units of measure

########################################## PCA on original data 
pc.ctrindPCA <- princomp(ctrindPCA[,3:ncol(ctrindPCA)], scores=T)
pc.ctrindPCA

summary(pc.ctrindPCA)
# To obtain the rows of the summary:
# standard deviation of the components
#pc.ctrindPCA$sd
# proportion of variance explained by each PC
#pc.ctrindPCA^2/sum(pc.ctrindPCA$sd^2)
# cumulative proportion of explained variance
#cumsum(pc.ctrindPCA$sd^2)/sum(pc.ctrindPCA$sd^2)

######################################### Loadings 
#(recall: coefficients of the linear combination of the original variables that defines each principal component)

load.ctrindPCA <- pc.ctrindPCA$loadings
load.ctrindPCA

# graphical representation of the loadings of the first six principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(load.ctrindPCA[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: adjusted savings
# Second PCs: Total population (neg)
# Second PCs: Land area and Surface area effect
# Second PCs: Land area vs Surface area
# Second PCs: Population density 
# Last PCs: Population growth
# The loadings reflect the previous observation: the first PC is represented by the adjusted savings, 
# the second by the Total population, etc. (units of meausure effect)

#################################### Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.ctrindPCA, las=2, main='Principal components')
barplot(sapply(ctrindPCA[,3:ncol(ctrindPCA)],sd)^2, las=2, main='Original Variables', ylab='Variances')
plot(cumsum(pc.ctrindPCA$sd^2)/sum(pc.ctrindPCA$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ctrindPCA),labels=1:ncol(ctrindPCA),las=2)

# The first PC (var. adjusted savings) explains more than 99.99% of the total variability. 
# This is due to the masking effect of that variable over the others

############################## Scores: repr of the original data in the space PC1 PC2
scores.ctrindPCA <- pc.ctrindPCA$scores  

layout(matrix(c(1,2),2))
boxplot(ctrindPCA[, 3:ncol(ctrindPCA)], las=2, col='red', main='Original variables')
scores.ctrindPCA <- data.frame(scores.ctrindPCA)
boxplot(scores.ctrindPCA, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(pc.ctrindPCA, scale=0, cex=.7)

graphics.off()

# 03. Principal component analysis but on the standardized variables ----

# We compute the standardized variables
ctrindPCA.sd <- scale(ctrindPCA[,3:ncol(ctrindPCA)])
ctrindPCA.sd <- data.frame(ctrindPCA.sd)

pc.ctrindPCA <- princomp(ctrindPCA.sd, scores=T)
pc.ctrindPCA
summary(pc.ctrindPCA)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.ctrindPCA, las=2, main='Componenti principali')
abline(h=1, col='blue')
barplot(sapply(ctrindPCA.sd,sd)^2,las=2, main='Variabili originarie', ylim=c(0,6), ylab='Variances') #?????
plot(cumsum(pc.ctrindPCA$sde^2)/sum(pc.ctrindPCA$sde^2), type='b', axes=F, xlab='numero di componenti', ylab='contributo alla varianza totale', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ctrindPCA.sd),labels=1:ncol(ctrindPCA.sd),las=2)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 PCs

# loadings
load.ctrindPCA <- pc.ctrindPCA$loadings
load.ctrindPCA
# se aiuta l'interpretazione posso cambiare segno
x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.ctrindPCA[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# Interpretation of the loadings:
# The first PC represents an average of the first two and last two variables, taken with very similar (negative) weights. 
# The sencond PC contrasts the population density and the population growth.

# High PC1 (in val assoluto): the value of the first and last two variables is high
# Low PC1: the value of the first and last two variables is low
# High PC2: large density less growth
# Low PC2: high density and high growth

# scores: repr of the original data in the space PC1 PC2
scores.ctrindPCA <- pc.ctrindPCA$scores
scores.ctrindPCA

x11()
plot(scores.ctrindPCA[,1:2])

x11()
layout(matrix(c(1,2),2))
boxplot(ctrindPCA.sd, las=2, col='red', main='Original variables')
scores.ctrindPCA <- data.frame(scores.ctrindPCA)
boxplot(scores.ctrindPCA, las=2, col='red', main='Principal components')

x11()
layout(matrix(c(1,2),1))
plot(ctrindPCA.sd[,'m100'],ctrindPCA.sd[,'Marathon'],type="n",xlab="m100",ylab="Marathon", asp=1)
text(ctrindPCA.sd[,'m100'],ctrindPCA.sd[,'Marathon'],dimnames(runrec)[[1]],cex=.7)
plot(-scores.ctrindPCA[,1],-scores.ctrindPCA[,2],type="n",xlab="-pc1",ylab="-pc2", asp=1)
text(-scores.ctrindPCA[,1],-scores.ctrindPCA[,2],dimnames(ctrindPCA)[[1]],cex=.7)

x11()
biplot(pc.ctrindPCA)
