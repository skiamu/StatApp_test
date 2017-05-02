# test script function "PC"
source("PCA_function.R")

# dataset lab 3
X <- read.table('record_mod.txt', header=T)
X[,4:7] <- X[,4:7]*60


# PCA on covariance matrix
pc_data1<-PC(X,method="eigen",scaled=F,graph=T,rm.na=T,print.results=T)
# PCA on correlation matrix
pc_data1<-PC(X,method="eigen",scaled=F,graph=T,rm.na=T,print.results=T)
