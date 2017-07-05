make.yy <- function(Ind,Country,flag.my.agg = F,myYear = NULL){
   dq <- getIndicators(myInd = Ind,myCnt = Country,agg = flag.my.agg,myYear)
   dq1 <- unifCnt(dq,showCnt = T, showInd = F)
   # years where data is available
   y <- unique(dq1$Year)
   X <- get3D(dq1,y)
   y.name <- Ind
   # number of observation in each year
   nn <- dim(X[[1]])[1]
   
   # dataframe with the following structure:
   #    rows = nations
   #    col = years
   #    
   #    in position yy[i,j] we have the growth for country i over the time interval
   #    [t_j,t_(j-10)]
   yy <- data.frame(matrix(nrow = nn,ncol = length(y)-10 ))
   for(i in 1:(length(y)-10)){# ciclo sugli intervalli decennali
      for(j in 1:nn){# ciclo sulle nazioni dentro ogni decennio
         yy[j,i] <- (X[[i+10]][j,1] - X[[i]][j,1]) / X[[i]][j,1]
      }
   }  
   colnames(yy) <- seq(y[1]+10,y[length(y)],by = 1)
   rownames(yy) <- rownames(X[[1]]) 
   return(list(yy = yy, Ind.like = dq1))
}



make.ggplot <- function(yy){
   rownames(yy) <- gsub("\\(.*$", "", rownames(yy))
   n <- dim(yy)[1]
   p <- dim(yy)[2]
   z <- data.frame(matrix(nrow = n * p,ncol = 3))
   k = 1
   for(i in 1:n){
      for(j in 1:p){
         # Value
         z[k,1] <- yy[i,j]
         # Country
         z[k,2] <- rownames(yy)[i]
         # Year
         z[k,3] <- colnames(yy)[j]
         k = k + 1
      }
   }
   colnames(z) <- c("Value","Country","Year")
   return(z)
}

