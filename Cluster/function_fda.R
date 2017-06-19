# function to perform Fisher Discriminant Analysis (FDA)

# fda is a function that perform the Fisher linear discriminant analysis:
# we assume: 1) same cost of misclassifying, 
#            2) same covariance matrix, 
#            3) prior prob proportional to the number of elements

fda <- function(X, clusters, nFishComp){
  
  # -- INPUT: 
  #    - X             : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given (result from kmeans)
  #    - nFishComp     : number of fisher components
  # -- OUTPUT:
  #    - a  : matrix in which the cloumns are the components
  #    - cc : centres of the Fisher analysis
  
  p      <- length(colnames(X))   # dimension of the features vector
  labels <- unique(clusters) # labels of the clusters
  g      <- length(labels)        # number of groups (cluster)
  s      <- min(g-1,p)
  nTot   <- length(clusters)
  m      <- colMeans(X)
  mcol   <- array(0, c(g, p))
  B      <- 0
  Sp     <- 0
  for(i in 1:g){
    indeces  <- which(clusters==labels[i])
    ni       <- length(indeces)         # every iteration ni is updated
    mcol[i,] <- colMeans(X[indeces,])   # mean of the i-th cluster
    B        <- B  + ni * cbind(mcol[i,] - m) %*% rbind(mcol[i,] - m)
    Sp       <- Sp + (ni-1) * cov(X[indeces,])
  }
  B  <-  B/(nTot-g)
  Sp <- Sp/(nTot-g)
  
  # Matrix Sp^(-1/2)
  val.Sp <- eigen(Sp)$val
  vec.Sp <- eigen(Sp)$vec
  invSp=0
  for(i in 1:nFishComp){ invSp = invSp + 1/sqrt(val.Sp[i])*vec.Sp[,i]%*%t(vec.Sp[,i])}
  spec.dec <- eigen(invSp %*% B %*% invSp)
  
  # computation of the Fisher components
  a <- array(0, c(p,nFishComp))
  for(i in 1:nFishComp){a[,i] <- invSp %*% spec.dec$vec[,i]}
  
  # discrimination
  cc<- mcol %*% a
  return(list(fComp=a, center=cc))
}

# function to use the results from fda to perform predictions
# IMPORTANT REMARK: if the data given to fda were standardized you should standardize predData !!!
fdaPred <- function(a, cc, predData){
  
  # -- INPUT: 
  #    - a             : component of Fischer analysis
  #    - cc            : centres of the Fisher analysis
  #    - predData      : obs to discriminate [check if you have to standardize]
  # -- OUTPUT:
  #    - cluster : cluster for the observation to discriminate
  
  g <- dim(cc)[1]                  # number of groups(cluters)
  ccPred=as.numeric(predData)%*%a  # rotate the data to predict
  dist<-array(1,g)
  for(i in 1:g){dist[i]=sqrt(sum((ccPred-cc[i,])^2))}
  cluster=which.min(dist)          # assign to the nearest center
  return(cluster)
}
