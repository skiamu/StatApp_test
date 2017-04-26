
library(mvtnorm)
library(car)

source("Inference_functions.R")

mu <- c(1,0)
sig <- matrix(c(1,1,1,2),nrow=2)
invsig <- solve(sig)

set.seed(28071987)
x <- rmvnorm(n=30, mean=mu, sigma=sig)
x <- data.frame(X.1=x[,1],X.2=x[,2])


#1) conf reagion
ConfidenceRegion(x,large_n = F,alpha = 0.05,to.do = "CR")


#2) simult int
ConfidenceRegion(x,large_n = F,alpha = 0.05,to.do = "sim_CI")


# 3) bonf
ConfidenceRegion(x,large_n = F,alpha = 0.05,to.do = "Bonf_CI")
