# 00 --- Set working directory, import libraries, functions and data #####

# Setting Working Directory:
setwd('/Users/mowythebest/Desktop/StatApp_test')

# Functions:
# User defined functions:
source('function_extract.R')           # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("Filters/functionsFullMatrix.R") # extract2DmatrixWithFullestIndicators, fullness
source("Filters/filtering_functions.R") # extract2DmatrixWithFullestIndicators, fullness
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...

# Data:
load("ReadData/data.RData")

# Libraries
library(dplyr)
library(mvtnorm)
library(rgl)
library(car)
library(MASS)

# 01 --- Setting the working dataframe ----

# Indicators of this topic: "Access to electricity (% of population)" ?, 
#                           "Fixed telephone subscriptions (per 100 people)",
#                           "Mobile cellular subscriptions (per 100 people)",
#                           "Urban population growth (annual %)",
#                           "Urban population growth (annual %)",
#                           "Internet users (per 100 people)".
# I have to decide what to do with Access because is not so common have it

# Set indicators:
myInd <- c(#"Access to electricity (% of population)",
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
TeleDF <- getIndicators(myYear = myYear, myInd = myInd, agg = F)

# TeleMatrix = Matrix Country-Indicators created from TeleDF
TeleMatrix <- getCntInd(TeleDF, myYear, dropNA = T, showCnt = T)

# 02 --- Aggregate Hierarchical Clustering ####
x11()
pairs(TeleMatrix)
dev.off()

# compute the dissimilarity matrix of the data: Euclidean metrics (then other metrics)
telecom.e <- dist(TeleMatrix, method='euclidean')
telecom.m <- dist(TeleMatrix, method='manhattan')
telecom.c <- dist(TeleMatrix, method='canberra')

x11()
par(mfrow=c(1,3))
image(1:199,1:199,as.matrix(telecom.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
image(1:199,1:199,as.matrix(telecom.m), main='metrics: Manhattan', asp=1, xlab='i', ylab='j')
image(1:199,1:199,as.matrix(telecom.c), main='metrics: Canberra', asp=1, xlab='i', ylab='j')

# in fact, the data are never ordered according to (unknown) labels
misc <- sample(199)
TeleMatrix <- TeleMatrix[misc,]

telecom.e <- dist(TeleMatrix, method='euclidean')
telecom.m <- dist(TeleMatrix, method='manhattan')
telecom.c <- dist(TeleMatrix, method='canberra')

x11()
par(mfrow=c(1,3))
image(1:199,1:199,as.matrix(telecom.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j' )
image(1:199,1:199,as.matrix(telecom.m), main='metrics: Manhattan', asp=1, xlab='i', ylab='j' )
image(1:199,1:199,as.matrix(telecom.c), main='metrics: Canberra', asp=1, xlab='i', ylab='j' )

graphics.off()
# Neglect Camberra

# we now aim to perform hierarchical clustering of the dataset telecom by Euclidean distance
telecom.es <- hclust(telecom.e, method='single')
telecom.ea <- hclust(telecom.e, method='average')
telecom.ec <- hclust(telecom.e, method='complete')
telecom.ew <- hclust(telecom.e, method='ward.D2')
# we now aim to perform hierarchical clustering of the dataset telecom by Manhattan distance
telecom.ms <- hclust(telecom.m, method='single')
telecom.ma <- hclust(telecom.m, method='average')
telecom.mc <- hclust(telecom.m, method='complete')
telecom.mw <- hclust(telecom.m, method='ward.D2')

# if we wanted more detailed information on euclidean-complete clustering:
names(telecom.ec)
telecom.ec$merge  # order of aggregation of statistical units / clusters
telecom.ec$height # distance at which we have aggregations
telecom.ec$order  # ordering that allows to avoid intersections in the dendrogram

# plot of the dendrograms
x11()
par(mfrow=c(2,4))
plot(telecom.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.ms, main='manhattan-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.ma, main='manhattan-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

dev.off()
# Neglect single and average

# plot dendrograms (2 clusters)
x11()
par(mfrow=c(2,2))
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ec, k=2)
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=2)
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mc, k=2)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=2)
dev.off()

x11()
par(mfrow=c(2,2))
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ec, k=3)
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=3)
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mc, k=3)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=3)
dev.off()

x11()
par(mfrow=c(2,2))
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ec, k=4)
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=4)
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mc, k=4)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=4)
dev.off()

x11()
par(mfrow=c(2,2))
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ec, k=5)
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=5)
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mc, k=5)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=5)
dev.off()

x11()
par(mfrow=c(2,2))
plot(telecom.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ec, k=6)
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=6)
plot(telecom.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mc, k=6)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=6)
dev.off()

# 2,5 fanno schifo, 3,4,5 le migliori peò no manhattan complete, in 6 rimane solo ward


# Fix k=3 clusters:
cluster3.ec <- cutree(telecom.ec, k=3) # euclidean-complete
cluster3.ew <- cutree(telecom.ew, k=3) # euclidean-ward
cluster3.mw <- cutree(telecom.mw, k=3) # Manhattan-ward
# Fix k=4 clusters:
cluster4.ec <- cutree(telecom.ec, k=4) # euclidean-complete
cluster4.ew <- cutree(telecom.ew, k=4) # euclidean-ward
cluster4.mw <- cutree(telecom.mw, k=4) # Manhattan-ward
# Fix k=5 clusters:
cluster5.ec <- cutree(telecom.ec, k=5) # euclidean-complete
cluster5.ew <- cutree(telecom.ew, k=5) # euclidean-ward
cluster5.mw <- cutree(telecom.mw, k=5) # Manhattan-ward
# Fix k=6 clusters:
cluster6.ew <- cutree(telecom.ew, k=6) # euclidean-ward
cluster6.mw <- cutree(telecom.mw, k=6) # Manhattan-ward

# Let's give a mark to the algorithms: did they aggregate coherently with the dissimilarity matrix or not?
# compute the cophenetic matrices 
coph.ec <- cophenetic(telecom.ec)
coph.ew <- cophenetic(telecom.ew)
coph.mw <- cophenetic(telecom.es)

# compute cophenetic coefficients
ec <- cor(telecom.e, coph.ec)
ew <- cor(telecom.e, coph.ew)
mw <- cor(telecom.m, coph.mw)
c("Eucl-Compl"=ec,"Eucl-Ward."=ew, "Manh-Ward"=mw)


x11()
plot(TeleMatrix, col=cluster3.ec+1, pch=19)
x11()
plot(TeleMatrix, col=cluster3.ew+1, pch=19)
x11()
plot(TeleMatrix, col=cluster3.mw+1, pch=19)
x11()
plot(TeleMatrix, col=cluster4.ec+1, pch=19)
x11()
plot(TeleMatrix, col=cluster4.ew+1, pch=19)
x11()
plot(TeleMatrix, col=cluster4.mw+1, pch=19)
x11()
plot(TeleMatrix, col=cluster5.ec+1, pch=19)
x11()
plot(TeleMatrix, col=cluster5.ew+1, pch=19)
x11()
plot(TeleMatrix, col=cluster5.mw+1, pch=19)
x11()
plot(TeleMatrix, col=cluster6.ew+1, pch=19)
x11()
plot(TeleMatrix, col=cluster6.mw+1, pch=19)
graphics.off()
# ew4 migliore sembra

table(cluster3.ec) 
table(cluster3.ew) # ha stati suddivisi in modo sensato
table(cluster3.mw) # ha stati suddivisi in modo sensato
table(cluster4.ec) # ha stati suddivisi in modo sensato forse
table(cluster4.ew)
table(cluster4.mw) # la migliore per ora
table(cluster5.ec) # ha stati suddivisi in modo sensato
table(cluster5.ew)
table(cluster5.mw)
table(cluster6.ew)
table(cluster6.mw)

# Da gerarchico 4 clusters mw esce vincitore, forse 5 ec non male con 4ec
# 03 ---K-mean method ####

cluster3.k <- kmeans(TeleMatrix, centers=3) # Centers: fixed number of clusters
cluster4.k <- kmeans(TeleMatrix, centers=4) # Centers: fixed number of clusters
cluster5.k <- kmeans(TeleMatrix, centers=5) # Centers: fixed number of clusters
cluster6.k <- kmeans(TeleMatrix, centers=6) # Centers: fixed number of clusters


cluster4.k$cluster      # labels of clusters
cluster4.k$centers      # centers of the clusters

x11()
plot(TeleMatrix, col = cluster3.k$cluster+1)
x11()
plot(TeleMatrix, col = cluster4.k$cluster+1)
x11()
plot(TeleMatrix, col = cluster5.k$cluster+1)
# Sembra 4 migliore

open3d()
plot3d(TeleMatrix, size=3, col=cluster3.k$cluster+1, aspect = F) 
points3d(cluster3.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrix, size=3, col=cluster4.k$cluster+1, aspect = F) 
points3d(cluster4.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrix, size=3, col=cluster5.k$cluster+1, aspect = F) 
points3d(cluster5.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrix, size=3, col=cluster6.k$cluster+1, aspect = F) 
points3d(cluster6.k$centers, pch = 4, cex = 2, lwd = 4)

# to choose k: evaluate the variability between the groups wrt the variability withing the groups
b <- w <- NULL
for(k in 1:10){
  
  telecom.k <- kmeans(TeleMatrix, k)
  w <- c(w, sum(telecom.k$wit))
  b <- c(b, telecom.k$bet)
  
}

x11()
matplot(1:10, b/(w+b), pch='', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, b/(w+b), type='b', lwd=2)

x11()
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)

# nonostante questi ris numerici che spingerebbero peravere più cluster pox, 4-5 perfetti per interpretazione