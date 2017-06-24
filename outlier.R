# this function tries to detect outlier from a mulativariate dataframe
# 
# ALGORITMO:
#           1) make a scatter plot of each variable
#           2) make a general scatter plot
#           3) compute and analyze standardized values
#           4) compute and analyse Mahalanobis distance
#            

find_outlier <- function(X, remove = FALSE,plot = F){
   #
   # INPUT:
   #       X = dataframe
   #       remove = TRUE if you want to remove observation that are considered
   #                as outlier
   # 
   # OUTPUT:
   #       filtered = filtered dataframe (without outlier)
   library(dplyr)
   obs.name <- rownames(X)
   ## 1) single dot plot
   # sample cardinality         # sample dimensionality
   n <- dim(X)[1];              p <- dim(X)[2]
   if(plot){
      # open a big plot device
      x11(width=15) 
      # deviide it in a 2xp matrix
      par(mfrow = c(2,ceiling(p/2))) 
      # extract columns name
      names <- colnames(X) 
      for(i in 1:p)  plot(X[,i],main = names[i])
   }
   ## 2) multiple dot plot
   if(plot){
      x11()
      plot(X)
   }
   ## 3) standardized values
   
   # center and normalize the data
   if(plot){
      Z <- scale(X)
      x11()
      par(mfrow = c(2,ceiling(p/2)))
      # plot standardized data, values outside the 4 or 3.5 band can be considered 
      # outliers
      for(i in 1:p){
         plot(Z[,i],main = names[i],ylim = c(-4,4))
         abline(h = -3:3)
      }  
   }
   ## 4) mahalanobis distance
   d <- mahalanobis(as.matrix(scale(X)),colMeans(scale(X)),cov(scale(X)))
   cfr.chi_squared <- qchisq(0.95,p)
   if(plot){
      x11()
      # histogram of d
      hist(d, prob = T, main="Mahalanobis distance hist",
           xlab="d^2")
      # add the point to the histogram
      points(d,rep(0,length(d)), pch=19)
      #  add chi-sq density with p degree of freedom
      lines(seq(min(d),max(d),length.out = 200),
            dchisq(seq(min(d),max(d),length.out = 200),p),
            col='blue', lty=2)
      # draw the chi-squared quantile @ level 99%: values grater than
      # quantile may be considered as outlier
      abline(v = cfr.chi_squared,col="orange")
   }
   ############# remove outlier ###################
   
   if(remove){
      # add mahalanobis distance in the dataframe
      X <- X %>% mutate(d = d,n.row = rownames(X))
      
      # filter out observation whose distance is grater than the chi-squared 
      # quantile @ level 99%
      filtered <- X %>% 
         filter(d <= cfr.chi_squared)
      
      cat("I've filtered out",setdiff(obs.name,filtered$n.row),"\n")
      return(select(filtered,-d))
   }
   
}# end "find_outlier"