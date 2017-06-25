# This function extract the dataframe of observation in the specified year
# in order to to prediction. If requested, it returns also the vector of 
# true growth rates in order to evaluate the predictor
# INPUT: 
#      myYear = year you are standing in
#      myInd = all the regressors (countinus + discrete)
# OUTPUT:
#      XD = design matrix
#      Y = vector of response
Design_Matrix_pred <- function(myYear,
                               myInd,
                               response.vector = T,
                               myCountry = NULL,
                               GDP = "GDP per capita (constant 2005 US$)"){
   
   
   if(response.vector & length(myYear) == 1){
      stop("dare in input anke l'anno per la risposta Y")
   }
      
   df1 <- getIndicators(myYear = myYear,
                        myInd = myInd,
                        agg = F,
                        ind = Indicators,
                        myCnt = myCountry)

   ###### 1) regressori discreti negli anni
   df1 <- unifCnt(df1,showCnt = T, showInd = F)
   X.discrete <- get3D(df1,myYear)
   # nazioni per cui verrà fatta l'analisi
   nazioni <- rownames(X.discrete[[1]])
   n <- length(nazioni)
   
   ###################### CREAZIONE DELLE DUMMY REGION ###########
   nazione.income <- nazione.regione <- vector("character",n)
   Code <- Indicators %>% filter(CountryName %in% nazioni) %>% select(CountryCode) %>%
      unique() 
   region.income <- data.frame()
   for(i in 1:n){
      t <- Country %>% filter(CountryCode == Code$CountryCode[i]) %>%
         select(Region,IncomeGroup)
      region.income <- rbind(region.income,t)
   }
   for(i in 1:n){
      if(region.income[i,1] == "South Asia"){
         region.income[i,1] = "East Asia & Pacific"
      }
   }
   # nessuna distinzione tra OECD e non OECD
   region.income[,2] <- gsub("\\:.*$", "", region.income$IncomeGroup)
   # specificare le dummy che si vuole mettere
   dummy.region <- c("East Asia & Pacific","Sub-Saharan Africa","Europe & Central Asia")
   dummy.income <- c("High income","Low income")
   R <- data.frame(matrix(0,nrow = n,ncol = length(dummy.region)))
   I <- data.frame(matrix(0,nrow = n,ncol = length(dummy.income)))
   for(i in 1:length(dummy.region)){
      for(j in 1:n){
         if(region.income[j,1] == dummy.region[i]){
            R[j,i] <- 1
         }
      }
      colnames(R)[i] <- paste("R",i,sep = "")
   }
   for(i in 1:length(dummy.income)){
      for(j in 1:n){
         if(region.income[j,2] == dummy.income[i]){
            I[j,i] <- 1
         }
      }
      colnames(I)[i] <- paste("I",i,sep = "")
   }
   rownames(R) <- rownames(I) <- nazioni
   
   # assemblo le dummy con ogni dataframe in X.continum
   for(i in 1:length(X.discrete)){
      X.discrete[[i]] <- cbind(X.discrete[[i]],R,I)
   }
   
   ############### COSTRUZIONE VETTORE RISPOSTE
   # if required in input, we compute the vector Y to check the quality of the 
   # predictor. For exaple, if i'm in 2000 (hence myYear = c(2000,2010)) we are
   # forecasting the growth over the time interval [2000,2010] with data until
   # 2000. The correct answer will be availabe in 2010, so the true growth is
   # the one over the interval [2000,2010] taht we are about to compute
   if(response.vector){
      # calcolo i 3 vettori separatamente e poi li impilo
      Y.true <- vector("double")
      y.name <- GDP
      for(j in 1:n){# ciclo sulle nazioni dentro ogni decennio
         Y.true[j] <- (X.discrete[[2]][j,y.name] - X.discrete[[1]][j,y.name]) /
            X.discrete[[1]][j,y.name]
      }
   }
   
   return(list(Y = myifelse(response.vector,Y.true,"no response vector"),# response vector
               XD = X.discrete[[1]],# design matrix
               n = n,
               nations = nazioni))# number of countries in the analysis
   
}# end functon

myifelse <- function(condition,x,y){
   if(condition)
      return(x)
   else
      return(y)
   
}# end function




