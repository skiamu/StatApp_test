# fdaPred is a function that perform predict a cluster by Fisher linear discriminant analysis:
# we assume: 1) same cost of misclassifying, 
#            2) same covariance matrix, 
#            3) prior prob proportional to the number of elements
# Warning: pass the data standardized

fdaPred <- function(data, clusters, clustersLabels, numComp, numVar, predDataStd){
  
  # -- INPUT: 
  #    - data          : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given
  #    - clustersLabels: labels of the clusters [vector of strings]
  #    - numComp       : number of fisher components  
  #    - numVar        : number of variables
  #    - predDataStd   : obs to discriminate [standardized]
  # -- OUTPUT:
  #    - cluster : cluster for the observation to discriminate
  
  # ingridient to compute the Matrix Sp^(-1/2)
  m <-  colMeans(data)
  g <- length(clustersLabels)
  p <- numVar
  s <- min(g-1,p)
  nTot <- 0
  Sp <- 0
  B <- 0
  mcol <- array(0, c(g, p))
  for(i in 1:g){
    indeces <- which(clusters==clustersLabels[i])
    n <- length(indeces)
    nTot=nTot+n
    cov  <-  cov(data[indeces,])
    mcol[i,] <- colMeans(data[indeces,])
    Sp = Sp + (n-1)*cov
    B= B + n * cbind(mcol - m) %*% rbind(mcol - m)
  }
  B  = B/nTot  
  Sp = Sp/(nTot-g)
  
  # Matrix Sp^(-1/2)
  val.Sp <- eigen(Sp)$val
  vec.Sp <- eigen(Sp)$vec
  invSp=0
  for(i in 1:numComp){ invSp = invSp + 1/sqrt(val.Sp[i])*vec.Sp[,i]%*%t(vec.Sp[,i])}
  spec.dec <- eigen(invSp %*% B %*% invSp)
  
  # computation of the Fisher components
  a <- array(0, c(p,numComp))
  for(i in 1:numComp){a[,i] <- invSp %*% spec.dec$vec[,i]}
  
  # discrimination
  cc<- mcol%*%a
  ccPred=predDataStd%*%a
  dist<-array(1,g)
  for(i in 1:g){dist[i]=sqrt(sum((ccPred-cc[i,])^2))}
  # assign to the nearest mean
  cluster=which.min(dist)
  return(cluster)
}