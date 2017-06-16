# fda is a function that perform the Fisher linear discriminant analysis:
# we assume: 1) same cost of misclassifying, 
#            2) same covariance matrix, 
#            3) prior prob proportional to the number of elements

fda <- function(data, clusters, clustersLabels, numComp, numVar){
  
  # -- INPUT: 
  #    - data          : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given
  #    - clustersLabels: labels of the clusters [vector of strings]
  #    - numComp       : number of fisher components  
  #    - numVar        : number of variables
  # -- OUTPUT:
  #    - a  : matrix in which the cloumns are the components
  #    - cc : centres of the Fisher analysis
  
  # ingridient to compute the Matrix Sp^(-1/2)
  m <- colMeans(data)
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
    mcol[i,] <- colMeans(data[indeces,])
    B= B + n * cbind(mcol - m) %*% rbind(mcol - m)
  }
  B  = B/nTot
  for(i in 1:p){
    cov  <-  cov(data[indeces,])
    Sp = Sp + (n-1)*cov
  }
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
  return(list(FCompo=a, centres=cc))
}