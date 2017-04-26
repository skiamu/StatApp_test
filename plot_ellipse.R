# this function draws an ellipse:
#1) sample = TRUE --> it means that matrix Sigma is the sample variance. In this case
#                     the elliptic region is a confidence region for the mean vector
#2) sample = FALSE --> it means that matrix Sigma is the variance matrix of
#                      a normal bivariate population. In this case the elliptic
#                      region is a probability reagion @ level 1-alpha 
plot_ellipse <- function (mu, Sigma, alpha = 0.05, sample = T,n = NULL){
   # INPUT:
   #      mu = mean vector (2-D)
   #      Sigma = 2x2 positive def matrix
   #      alpha = 
   #      sample = T if Sigma is the sample varianze  F if it's the population's
   #               variance
   #      n = sample cardinality (numerosità campionaria)
   # OUTPUT: none
   
   library(car) 
   # Sigma must be 2x2
   if(all(dim(Sigma) != c(2,2))){
      stop("dimensioni matrice errate per plot 2-D")
   }
   # applico teorema spettarle
   w <- eigen(Sigma) 
   # estraggo autovalori
   lambda1 <- w$values[1]
   lambda2 <- w$values[2]
   # estraggo autovettori
   e1 <- w$vectors[,1]
   e2 <- w$vectors[,2]
   # dimensione popolazione
   p <- 2 
   # quantile distribuzione
   qnt <- myifelse(sample, qf(1-alpha,p,n - p), qchisq(1-alpha,2))
   # raggio ellisse
   r <- myifelse(sample,sqrt((p * (n - 1)) / (n * (n - p))) * 
                    sqrt(qnt),sqrt(qnt))
   S <- myifelse(sample,Sigma/n,Sigma)
   
   # stampa caratteristiche ellisse
   max.semiax <- sqrt(lambda1) * r
   min.semiax <- sqrt(lambda2) * r 
   cat("(lambda1, lambda2) = ",c(lambda1,lambda2),"\n",sep = "\t")
   print("autovettori = ")
   print(w$vectors)
   cat("semiasse maggiore = ", max.semiax,"\n")
   cat("semiasse minore = ",min.semiax,"\n")
   
   
   # plotting
   x11()
   plot(mu[1],mu[2],
        ylim=c(mu[2] - max.semiax,mu[2] + max.semiax),
        xlim = c(mu[1] - max.semiax, mu[1] + max.semiax),
        col='blue',pch=19,xlab='X.1',ylab='X.2',asp=1)
   ellipse(center=mu, shape= S, radius=r, col = 'blue')
   m = e1[2]/e1[1]
   abline(a=mu[2] - m * mu[1], b = m, lty=2, col='grey')
   abline(a=mu[2] + mu[1]/m,b=-1/m,col='grey',lty=2)
   
}# end function

# implementazione funzione myifelse
myifelse <- function(condition,x,y){
   if(condition)
      return(x)
   else
      return(y)
}


