

manual_stuff <- function(X){
   colnames(X) <- gsub("\\(.*$", "", colnames(X))
   colnames(X) <- gsub("\\,.*$", "", colnames(X))
   name <- colnames(X)
   # prendo il log(GDP)
   X[,name[3]] <- log(X[,name[3]])
   # prendo 1/life_expectancy = mortality rate
   X[,name[8]] <- 1/X[,name[8]]
   colnames(X)[1:9] <- c("fertility","FDI","GDP","investment","education","consumi",
                              "inflation","health","openess")
   
   return(X)
}