# function "ConfidenceRegion" does the following things:
# 1) computes confidence regions for the true mean of a normal population
# 2) computes confidence regions for the true mean of any population (n large)
# 3) simultaneous confidence intervals (F)
# 4) Bonferroni simultaneous confidence intervals (t)
#
#  REMARK: 1)when the gaussian hp is used it is always check using mcshapiro test, the corresponding
#            p-value is printed.
#          2) call the function with the following different values of argument
#             to.do:
#             "CR" = confidence region
#             "sim_CI" = F simulataneous conf intervals
#             "sim_Bonf" = Bonferroni simultaneous conf intervals
#  

ConfidenceRegion <- function(X = NULL,
                             large_n = F,
                             S = NULL,
                             X_bar = NULL,
                             alpha = 0.05,
                             k = NULL,
                             to.do = NULL,
                             A = NULL
){
   # INPUT:
   #      X = dataframe 
   #      large_n = if TRUE asymptotic analysis, otherwihe gaussian hp
   # OUTPUT:
   # 
   
   load("mcshapiro.test.RData")
   # if not given as input, compute sample mean and sample covariance
   if(is.null(X_bar))
      X_bar <- colMeans(X)
   if(is.null(S))
      S <- cov(X)
   # sample cardinality
   n <- dim(X)[1]  
   # population dimension
   p <- dim(X)[2]
   
   if(!large_n){# n is small, relay on Gaussian hp
      # check for gaussianity
      mctest <- mcshapiro.test(X)
      if(mctest$pvalue < alpha)
         warning("WARNING: gaussian hp not supported by data, shapiro p-value = ",mctest$pvalue,"\n")
      else
         cat("You are assuming gaussianity, there's no evidence to say the contrary, shapiro p-value = ",mctest$pvalue,"\n")
      
      # if A, the matrix of the directions, is not specified; by default let's
      # compute the interval for the mean components
      if(is.null(A)){
         A <- diag(p)
      }
      # let's start inference analysis
      switch(to.do,
             CR = {# case  "confidence region for the mean"
                
                source("plot_ellipse.R")
                # plot the confidence region @ level 1-alpha for the mean
                plot_ellipse(X_bar, S, alpha = alpha, sample = T, n = n)
                
             },
             sim_CI = {# case "simulatneous confidence interval"
                
                # IC is a matrix dim(A)[1]x2, each row is a CI
                IC <- simult_CI(X_bar, S, alpha, A, n)
                # print the results
                cat("Simultaneus confidence interval @ level alpha = ",alpha,"\n")
                print(IC)
             },
             Bonf_CI = {# case "Bonferroni confidence interval"
                
                # IC is a matrix dim(A)[1]x2, each row is a CI
                IC <- simult_CI(X_bar, S, alpha, A, n, Bonf = T)
                # print the results
                cat("Bonferroni confidence interval @ level alpha = ",alpha,"\n")
                print(IC)
                
             },
             {
                print("default on switch \n")
             }
      ) # end switch
   }
   else{# n is large, build an asyntotic confidence region for the mean
      source("plot_ellipse.R")
      # plot the confidence region @ level 1-alpha for the mean
      plot_ellipse(X_bar, S, alpha = alpha, sample = T, n = n,large_n = T)
      
   }
   
   
   
} # end function mean_inference

# compute simultaneous confidence interval for linear combnations of mu
simult_CI <- function(X_bar, S, alpha = 0.05, A, n, Bonf = FALSE){
   # INPUT:
   #       X_bar = sample mean
   #       S = sample covariance
   #       1-alpha = confidence level
   #       A = matrix whose COLUMNS ARE the direction for the linear comb
   #       n = sample cardinality
   #       Bonf = if TRUE Bonferroni interval otherwise F intervals
   # OUTPUT:
   #       IC = matrix whose row are confidence intervals
   #  
   
   if(!(length(X_bar) == dim(A)[1]))
      stop("dimension of matrix A disagrees")
   # population dimension 
   p <- dim(S)[1]  
   # number of simultaneous interval
   k <- dim(A)[2]
   # initialization: IC will contain the simulataneous intervals
   IC <- matrix(nrow = k, ncol = 2)
   # coefficient that appears in the IC formula, it depends on Bonf or F
   csi <- ifelse(Bonf,qt(1 - alpha/(2*k),n-1), sqrt(p*(n-1)/(n-p) * qf(1-alpha, p, n-p)))
   for(i in 1:k){
      # select the direction for the linear combination
      a <- A[,i]
      IC[i,1] <- t(a)%*% X_bar - sqrt(t(a)%*%S%*%a / n) * csi
      IC[i,2] <- t(a)%*% X_bar + sqrt(t(a)%*%S%*%a / n) * csi
      
   }
   return(IC)
}# end simult_CI





