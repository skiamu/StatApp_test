# function to perform Fisher Discriminant Analysis (FDA)

# fda is a function that perform the Fisher linear discriminant analysis:
# we assume: 1) same cost of misclassifying, 
#            2) same covariance matrix, 
#            3) prior prob proportional to the number of elements

fda <- function(X, clusters, labelsCl, nFishComp){
  
  # -- INPUT: 
  #    - X             : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given (result from kmeans)
  #    - nFishComp     : number of fisher components
  #    - labelsCl      : name of the category
  # -- OUTPUT:
  #    - fComp: Matrix that has the Fisher Components as columns  
  #    - center: Coordinates of the centroides of the clusters
  

  p      <- length(colnames(X))   # dimension of the features vector
  labels <- labelsCl              # labels of the clusters
  g      <- length(labels)        # number of groups (cluster)
  nTot   <- length(clusters)      # total number of observations
  s      <- min(g-1,p)            # num max of compo

  # checking if the num of compo is acceptable
  if(nFishComp>s){ 
    print('Num of components too large: the num of compo has to be less than')
    print(s)
    stop("Error: Num of components too large")
  }
  
  m      <- as.numeric(colMeans(X))          # Mean of all the dataframe
  mcol   <- array(0, c(g, p))     # Declaration and initialization of the mean of the clusters
  B      <- 0                     # Declaration and initialization of the Between Variance
  Sp     <- 0                     # Declaration and initialization of the Within Variance
  for(i in 1:g){
    indeces  <- which(clusters==labels[i]) # indexes of the cluster i elements
    ni       <- length(indeces)            # every iteration ni is updated
    mcol[i,] <- as.numeric(colMeans(X[indeces,]))      # mean of the i-th cluster
    B        <- B  + ni * cbind(mcol[i,] - m) %*% rbind(mcol[i,] - m)
    if(ni==1)
      {Sp       <- Sp}
    else
      {Sp <- Sp + (ni-1) * cov(X[indeces,])}
  }

  B  <-  B/nTot
  Sp <- Sp/(nTot-g)
  
  # Matrix Sp^(-1/2)
  val.Sp <- eigen(Sp)$val
  vec.Sp <- eigen(Sp)$vec
  invSp=0
  for(i in 1:nFishComp){ invSp = invSp + 1/sqrt(val.Sp[i])*vec.Sp[,i]%*%t(vec.Sp[,i])}
  spec.dec <- eigen(invSp %*% B %*% invSp)
  
  # Computation of the Fisher components
  a <- array(0, c(p,nFishComp))
  for(i in 1:nFishComp){a[,i] <- invSp %*% spec.dec$vec[,i]}
  
  # Computation of the coordinates of the centroides of the clusters
  cc<- mcol %*% a
  
  return(list(fComp=a, center=cc))
}

# function to use the results from fda to perform predictions
# IMPORTANT REMARK: if the data given to fda were standardized you should standardize predData !!!
fdaPred <- function(a, cc, predData){
  
  # -- INPUT: 
  #    - a             : component of Fischer analysis
  #    - cc            : centres of the Fischer analysis
  #    - predData      : obs to discriminate [check if you have to standardize]
  # -- OUTPUT:
  #    - cluster : cluster for the observation to discriminate
  
  g <- dim(cc)[1]                  # number of groups(cluters)
  ccPred=as.numeric(predData)%*%a   # rotate the data to predict
  dist<-array(NaN,g)
  for(i in 1:g){dist[i]=sqrt(sum((ccPred-cc[i,])^2))}
  cluster=which.min(dist)          # assign to the nearest center
  return(cluster)
}

# Compute the estimate of the AER by cross-validation one-by-one
crossValFDA <- function(X, cluster, labelsCl, nFishComp){
  # -- INPUT: 
  #    - X             : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given (result from kmeans)
  #    - nFishComp     : number of fisher components
  #    - labelsCl      : name of the category
  # -- OUTPUT:
  #    - AERCV: Cross-validtion AER
  loocv <- 0
  num_obs <- dim(X)[1]
  for (k in 1:num_obs) {
    X_temp   <- X[-k, ]
    trainSet <- cluster[-k]
    testObs  <- X[k, ]
    fdc      <- fda(X_temp,trainSet,labelsCl,nFishComp)
    cv       <- fdaPred(fdc$fComp, fdc$center, testObs)
    loocv <- loocv + as.numeric(cv != cluster[k])
  }
  AERCV   <- loocv/num_obs
  return(AERCV)
}
crossValKNN <- function(X, cluster, labelsCl){
  # -- INPUT: 
  #    - X             : dataframe on which perform the discrimination
  #    - clusters      : column with the groups already given (result from kmeans)
  #    - nFishComp     : number of fisher components
  # -- OUTPUT:
  #    - AERCV: Cross-validtion AER
  library('class')
  loocv <- 0
  num_obs <- dim(X)[1]
  for (k in 1:num_obs) {
    X_temp   <- X[-k, ]
    trainSet <- cluster[-k]
    testObs  <- X[k, ]
    cv      <- knn(train = X_temp, test = testObs, cl = trainSet, k = length(labelsCl), prob=F)
    loocv <- loocv + as.numeric(cv != cluster[k])
  }
  AERCV   <- loocv/num_obs
  return(AERCV)
}
