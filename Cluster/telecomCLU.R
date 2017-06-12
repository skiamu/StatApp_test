# 00 --- Set working directory, import libraries, functions and data #####

# Setting Working Directory:
setwd('/Users/mowythebest/Desktop/StatApp_test')

# User defined functions:
source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...
source("outlier.R")                     # 
source("Graphs&Plots/extra_ggplot.R")   # multiplot, PCbiplot
source("Graphs&Plots/cluster_plot.R")   # plotClusterMap, plotClusterHierarchical, kmeansPlot, kmeansCompare
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
library(fmsb)                           # radarchart

# Functions
load("/Users/mowythebest/Desktop/Ingegneria_matematica/5.2_Applied_Statistics/funzioni_esame/mcshapiro.test.RData")
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

# shorten the IndicatorName
colnames(TeleDF) <- cleanName(colnames(TeleDF))

# TeleMatrix = Matrix Country-Indicators created from TeleDF
TeleMatrix <- getCntInd(TeleDF, myYear, dropNA = T, showCnt = T)
TeleMatrixStd <- data.frame(scale(TeleMatrix))
# 02 --- Aggregate Hierarchical Clustering ####
x11()
pairs(TeleMatrixStd)

dev.off()

# compute the dissimilarity matrix of the data: Euclidean metrics (then other metrics)
telecom.e <- dist(TeleMatrixStd, method='euclidean')
telecom.m <- dist(TeleMatrixStd, method='manhattan')
telecom.c <- dist(TeleMatrixStd, method='canberra')

x11()
par(mfrow=c(1,3))
image(1:199,1:199,as.matrix(telecom.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
image(1:199,1:199,as.matrix(telecom.m), main='metrics: Manhattan', asp=1, xlab='i', ylab='j')
image(1:199,1:199,as.matrix(telecom.c), main='metrics: Canberra', asp=1, xlab='i', ylab='j')

# in fact, the data are never ordered according to (unknown) labels
misc <- sample(199)
TeleMatrixStd <- TeleMatrixStd[misc,]

telecom.e <- dist(TeleMatrixStd, method='euclidean')
telecom.m <- dist(TeleMatrixStd, method='manhattan')
telecom.c <- dist(TeleMatrixStd, method='canberra')

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
# Neglect single, complete and average

# plot dendrograms (2 clusters)
x11()
par(mfrow=c(1,2))
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=2)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=2)
dev.off()

x11()
par(mfrow=c(1,2))
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=3)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=3)
dev.off()

x11()
par(mfrow=c(1,2))
plot(telecom.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.ew, k=4)
plot(telecom.mw, main='manhattan-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(telecom.mw, k=4)
dev.off()

# Fix k=3 clusters:
cluster3.ew <- cutree(telecom.ew, k=3) # euclidean-ward
cluster3.mw <- cutree(telecom.mw, k=3) # Manhattan-ward
# Fix k=4 clusters:
cluster4.ew <- cutree(telecom.ew, k=4) # euclidean-ward
cluster4.mw <- cutree(telecom.mw, k=4) # Manhattan-ward
# Fix k=5 clusters:
cluster5.ew <- cutree(telecom.ew, k=5) # euclidean-ward
cluster5.mw <- cutree(telecom.mw, k=5) # Manhattan-ward

# Let's give a mark to the algorithms: did they aggregate coherently with the dissimilarity matrix or not?
# compute the cophenetic matrices 
coph.ew <- cophenetic(telecom.ew)
coph.mw <- cophenetic(telecom.es)

# compute cophenetic coefficients
ew <- cor(telecom.e, coph.ew)
mw <- cor(telecom.m, coph.mw)
c("Eucl-Ward."=ew, "Manh-Ward"=mw)

x11()
plot(TeleMatrixStd, col=cluster3.ew+1, pch=19)
x11()
plot(TeleMatrixStd, col=cluster3.mw+1, pch=19)
x11()
plot(TeleMatrixStd, col=cluster4.ew+1, pch=19)
x11()
plot(TeleMatrixStd, col=cluster4.mw+1, pch=19)
x11()
plot(TeleMatrixStd, col=cluster5.ew+1, pch=19)
x11()
plot(TeleMatrixStd, col=cluster5.mw+1, pch=19)
graphics.off()
# ew4 migliore sembra

table(cluster3.ew) # ha stati suddivisi in modo sensato
table(cluster3.mw) # ha stati suddivisi in modo sensato
table(cluster4.ew)
table(cluster4.mw) # la migliore per ora
table(cluster5.ew)
table(cluster5.mw)

# fanno schifo: per capirci, prima ho usato dati non standardizzati e veniva che:
# Da gerarchico 4 clusters mw esce vincitore, forse 5 ec non male con 4ec
# nel momento in cui però sensatamente standardizzo fanno davvero schifo
# 03 --- K-mean method ####

cluster3.k <- kmeans(TeleMatrixStd, centers=3) # Centers: fixed number of clusters
cluster4.k <- kmeans(TeleMatrixStd, centers=4) # Centers: fixed number of clusters
cluster5.k <- kmeans(TeleMatrixStd, centers=5) # Centers: fixed number of clusters
cluster6.k <- kmeans(TeleMatrixStd, centers=6) # Centers: fixed number of clusters


cluster4.k$cluster      # labels of clusters
cluster4.k$centers      # centers of the clusters

x11()
plot(TeleMatrixStd, col = cluster3.k$cluster+1)
x11()
plot(TeleMatrixStd, col = cluster4.k$cluster+1)
x11()
plot(TeleMatrixStd, col = cluster5.k$cluster+1)
# Sembra 4 migliore
library(rgl)
open3d()
plot3d(TeleMatrixStd, size=3, col=cluster3.k$cluster+1, aspect = F) 
points3d(cluster3.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrixStd, size=3, col=cluster4.k$cluster+1, aspect = F) 
points3d(cluster4.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrixStd, size=3, col=cluster5.k$cluster+1, aspect = F) 
points3d(cluster5.k$centers, pch = 4, cex = 2, lwd = 4)
open3d()
plot3d(TeleMatrixStd, size=3, col=cluster6.k$cluster+1, aspect = F) 
points3d(cluster6.k$centers, pch = 4, cex = 2, lwd = 4)

# to choose k: evaluate the variability between the groups wrt the variability withing the groups
b <- w <- NULL
for(k in 1:10){
  
  telecom.k <- kmeans(TeleMatrixStd, k)
  w <- c(w, sum(telecom.k$wit))
  b <- c(b, telecom.k$bet)
  
}

x11()
matplot(1:10, b/(w+b), pch='', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, b/(w+b), type='b', lwd=2)

x11()
matplot(1:10, w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2)

# nonostante questi ris numerici che spingerebbero per avere 5-6 cluster, ritengo che 4-5 perfetti per interpretazione

#dal momemnto che ha senso standardizzare non ha senso preservare risultati gerarchico aggregato perchè funziona bene se non std
#scelgo guindi clusterizzazione mediante k-means, i particolare con 5 cluster l'interpretazione è chiara e teoricamente sensata
# 04 --- Plot cluster ####
set.seed(2000)
kmeansPlot(TeleMatrixStd,5, showSp=, showMap=T, showMeans=T)
# 1 paesi poverissimi con telecomm scarsissime e futura crescita popolaz alta ma non attualmente exploitabile
# 2 paesi con telecomunic avanzatix ma crescità pop non prosperosa (invest finance)
# 3 paesi avanzatissimi con anche futura crescita popo (ha senso investire industrialmente, ma competiz altissima)
# 4 paesi aanzati ma non cosi tecnologici ma con crescita popo schifo ==> c'è bisogno convenzioni per favorire lo sviluppo 
# 5 paesi con telecom da schifo ma buona futura crescita popo e non cosi poveri
# 05 --- Existence of a difference in the mean value of the clusters distributions ####

p  <- 6
n1 <- table(cluster5.k$cluster)[1]
n2 <- table(cluster5.k$cluster)[2]
n3 <- table(cluster5.k$cluster)[3]
n4 <- table(cluster5.k$cluster)[4]
n5 <- table(cluster5.k$cluster)[5]

# Verify gaussianity
mcshapiro.test(TeleMatrixStd[cluster5.k$cluster=='1',])
mcshapiro.test(TeleMatrixStd[cluster5.k$cluster=='2',])
mcshapiro.test(TeleMatrixStd[cluster5.k$cluster=='3',])
mcshapiro.test(TeleMatrixStd[cluster5.k$cluster=='4',])
mcshapiro.test(TeleMatrixStd[cluster5.k$cluster=='5',])

# the distributions are normal as ARF is short to study