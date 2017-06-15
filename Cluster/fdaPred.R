# fdaPred is a function that perform predict a cluster by Fisher linear discriminant analysis:
# we assume: 1) same cost of misclassifying, 
#            2) same covariance matrix, 
#            3) prior prob proportional to the number of elements
# Warning: pass the data standardized

fdaPred <- function(a, cc, predDataStd){
  
  # -- INPUT: 
  #    - a             : component of Fischer analysis
  #    - cc            : centres of the Fisher analysis
  #    - predDataStd   : obs to discriminate [standardized]
  # -- OUTPUT:
  #    - cluster : cluster for the observation to discriminate
  
  
  ccPred=predDataStd%*%a
  dist<-array(1,g)
  for(i in 1:g){dist[i]=sqrt(sum((ccPred-cc[i,])^2))}
  # assign to the nearest mean
  cluster=which.min(dist)
  return(cluster)
}