# function for T2 Hotelling test of the mean vector.
# Both exact case and asyntotic one.
# plotting rejection region

T2.test <- function( X = NULL,
                     mu0,
                     large_n = F,
                     S = NULL,
                     X_bar = NULL,
                     alpha = 0.05
                     ){
   # INPUT:
   #      X = dataframe 
   #      large_n = if TRUE asymptotic analysis, otherwihe gaussian hp
   #      mu0 = null hp
   #      S = sample covariance matrix
   #      X_bar = sample mean
   #      alpha = first type error
   # OUTPUT:
   #      p.value = p-value of the T2 test or the asyntotic test (if n large)
   
   # load function for checking gaussianity
   load("mcshapiro.test.RData")
   # if not given as input, compute sample mean and sample covariance
   if(is.null(X_bar)) X_bar <- colMeans(X)
   if(is.null(S)) S <- cov(X)
   # sample cardinality    # population dimension
   n <- dim(X)[1];         p <- dim(X)[2]
   # max and min values within the dataframe, it's a global variable
   Range <<- range(X)
   # if small n we relay on gaussian hp, we need to check it
   if(!large_n) check_gaussianity(X,alpha)
   # computed statistic
   x.T2 <- n * (X_bar - mu0)%*%solve(S)%*%(X_bar - mu0)
   #compute the p_value
   p.value <- ifelse(large_n,1-pchisq(x.T2, p),1 - pf((n - p) / (p * (n - 1)) * x.T2, p, n-p))
   # print the result
   print("--------------------")
   cat("@ conf level alpha = ",alpha,ifelse(p.value<alpha,"we reject H0","we cannot reject H0"),"\n")
   cat("T2 test p-value = ", p.value,"\n")
   # plot the rejection region if X is bidimensional
   if(p==2){
      # load file with function "plot_ellipse"
      source("plot_ellipse.R")
      # plot rejection region and give information about the ellipse
      plot_ellipse(mu0, S, alpha = alpha, sample = T, n = n,
                   large_n = ifelse(large_n,T,F))
      title(paste("Rejection region @ level ", alpha, sep = ""))
      # add the null hp, center of the ellipse
      points(mu0[1], mu0[2], pch = 19, cex = 1.5, lwd = 2, col ='blue')
      # add the sample mean, if this point is from too far from mu0 we
      # reject the null hp. "far" in the sense of mahalanobis distance
      points(X_bar[1], X_bar[2], pch = 4, cex = 1.5, lwd = 2, col ='red')
      legend("topright",legend = c("null hp","sample mean"),col = c("blue","red"),
             pch = c(19,4))
      # add observation
      points(X[,1],X[,2])
   }
   # compute simultaneous confidence intervals for the components
   # of the mean vector mu, if we've rejected H0 we want to see if
   # the sample mean of some components don't belong to its confidence 
   # interval
   source("Inference_functions.R")
   IC <- ConfidenceRegion(X,large_n = F,alpha = alpha,to.do = "sim_CI",plot = T)
   points(1:p,mu0,pch=16, col = "orange")
   
   return(p.value)
   
} # end t2.test function

# definition of the function for checking gaussianity
check_gaussianity <- function(X, alpha){
   
   load("mcshapiro.test.RData")
   # check for gaussianity
   mctest <- mcshapiro.test(X)
   if(mctest$pvalue < alpha)
      warning("WARNING: gaussian hp not supported by data, shapiro p-value = ",mctest$pvalue,"\n")
   else
      cat("You are assuming gaussianity, there's no evidence to say the contrary, shapiro p-value = ",mctest$pvalue,"\n")
   
} # end function check_gaussianity

