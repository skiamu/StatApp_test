# function for checking gaussianity

is.gauss <- function(X){
   # INPUT:
   #       X = dataframe nxp
   # 
   # OUTPUT:
   
   
   n <- dim(X)[1] # number of observations
   p <- dim(X)[2] # number of variables
   
   ####### APPROACH 1 #########
   # check some linear combinations:
   # 
   # 1.1) variables' direction
   x11(width=13) # open a large device
   par(mfrow = c(2,p)) # divide the device in a 2xp way
   # plot the histograms and overlapping normal density with sample parameters
   # plot the qqplot
   shap.var.direction <- NULL # initialization
   for(i in 1:p){
      # histogram
      hist(X[,i], prob=T, main=paste('Histogram of X', i, sep=''),
           xlab=paste('X', i, sep=''))
      # add the point to the histogram
      points(X[,i],rep(0,length(X[,i])), pch=19)
      # normal density
      lines(seq(min(X[,i]),max(X[,i]),length.out = 2000),
            dnorm(seq(min(X[,i]),max(X[,i]),length.out = 2000),mean(X[,i]),sd(X[,i])),
            col='blue', lty=2)
      # qqplot
      qqnorm(X[,i],main = paste("QQplot of X",i,sep = ""))
      qqline(X[,i])
      # shapiro test variables' direction
      shap.var.direction[i] <- shapiro.test(X[,i])$p
      print(paste("p-value Shapiro test variable X",i,sep = ""))
      print(shap.var.direction[i])
      print("-----------")
   }
   
   # 1.2) principal component direction
   
   # compute the scores (data rototranslated in the PC reference system)
   PC <- data.frame(princomp(X)$scores)
   x11(width=13) # open a large device
   par(mfrow = c(2,p)) # divide the device in a 2xp way
   # plot the histograms and overlapping normal density with sample parameters
   # plot the qqplot
   shap.PC.direction <- NULL # initialization
   for(i in 1:p){
      # histogram
      hist(PC[,i], prob=T, main=paste('Histogram of PC', i, sep=''),
           xlab=paste('X', i, sep=''))
      points(PC[,i],rep(0,length(PC[,i])), pch=19)
      # normal density
      lines(seq(min(PC[,i]),max(PC[,i]),length.out = 2000),
            dnorm(seq(min(PC[,i]),max(PC[,i]),length.out = 2000),mean(PC[,i]),sd(PC[,i])),
            col='red', lty=2)
      # qqplot
      qqnorm(PC[,i],main = paste("QQplot of PC",i,sep = ""))
      qqline(PC[,i])
      # shapiro test variables' direction
      shap.PC.direction[i] <- shapiro.test(PC[,i])$p
      print(paste("p-value Shapiro test PC",i,sep = ""))
      print(shap.PC.direction[i])
      print("-----------")
   }
   
   ###### APPROACH 2 ##########
   # Check mahalanobis distance of observations from baricenter
   
   M <- colMeans(X) # sample mean
   S <- cov(X) # sample covariance matrix
   
   #  d2 it's a vector whose components are the mahalanobis distance of each
   # p-dimensional observation from the baricenter (sample mean)
   d2 <- matrix(mahalanobis(X, M, S))
   
   x11(width=13) # open large device
   par(mfrow=c(1,2)) # divide it as a 1x2 matrix
   hist(d2, prob=T, main = "Mahalanobis distance histogram")
   # add the observation point to the hist
   points(d2,rep(0,length(d2), pch=19))
   # according to theory, if the p-dimensional random vector X is normal
   # then d^2 ~ chi-square(p)
   lines(0:2000/100, dchisq(0:2000/100,p), col='blue', lty=2)
   qqplot(qchisq(seq(0.5/30, 1 - 0.5/30 ,by = 1/30), df = p), d2,  main='QQplot di d2',
          xlab='Theoretical Quantiles',ylab='Sample Quantiles')
   abline(0, 1)
   # divide the data in 6 intervals according to the quantile of a chi-sq
   d2.class <- cut(d2, qchisq((0:6)/6, df = p))
   d2.freq  <- table(d2.class)
   
   chi.squared.test <- chisq.test(x = d2.freq, p = rep(1/6, 6), simulate.p.value = T)
   print("chi-squared test on d^2")
   chi.squared.test
   print("------------")
   
   
   ###### APPROACH 3 ##########À
   # test all directions simulaneosuly
   
   mcshapiro <- mcshapiro.test(X)
   print("p-value mcshapiro test")
   mcshapiro$pvalue
   
   return(list(shap.var.direction = shap.var.direction,
               shap.PC.direction = shap.PC.direction,
               chi.squared.test = chi.squared.test,
               mcshapiro = mcshapiro))
   
   
}# end function
