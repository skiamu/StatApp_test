X <- data.frame(X1 = speed1,X2 = speed2)
Y <- distance
attach(X)
fit <- lm(Y ~ X1 + X2)
list.A <- list(rbind(c(0,1,0), c(0,0,1)),rbind(c(1,0,1), c(0,1,0)),
               rbind(c(0,1,0),c(0,0,1)))
list.b <- list(c(0,0),c(0,0),c(0,0))
X0.new <- data.frame(X1 = 20, X2 = 369)
w <- lin_reg(Y,
             X,
             list.A = list.A,
             list.b = list.b,
             print.plot.DIAGN = F,
             print.result.PCA = F,
             X0.new = X0.new,
             interval = "confidence",
             sim.band = T,
             pointwise = T)

