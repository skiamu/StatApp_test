# this function receive the contry and plot the 10y growth plot with predictions


# to run this function you need to have loaded:
# 1. prediction.RData (year.pred, X.pred)

plot10yPred <- function(cnt,flagPred=T,GDP="GDP per capita (constant 2005 US$)"){
  nY <- length(year.pred) # num of years for which you have predictions
  yNA <- NULL
  predDf <- NULL
  for (i in 1:nY){
    if (cnt %in% row.names(X.pred[[i]])) { # if there are predicions for that year
      predDf <- rbind( predDf,
                       c(Year = year.pred[i], X.pred[[i]][cnt,]) )
    } else {
      yNA <- c(yNA, i)
      #predDf <- rbind( predDf,
      #                 c(Year = year.pred[i], rep(NA,3)) )
    }
  }
  for (i in yNA) {
    predDf <- rbind( predDf,
                     c(Year = year.pred[i], rep(NA,3)) )
  }
  if (all(colSums(is.na(predDf[,-1]))==nY)){ warning('There are no prediction for this country') } 
  predDf <- as.data.frame(predDf)
  predDf$CountryName <- rep(cnt,nY)
  
  temp <- getIndicators(myInd = GDP,myCnt = cnt) %>%
    select(CountryName,Year,Value)                   
  y  <- sort(unique(temp$Year))   # years
  yp <- y[-((length(y)-10+1):(length(y)))]    # years in which the 10y growth can be computed
  nyp <- length(yp)               
  dataDf <- data.frame(Year = yp,
                       CountryName = rep(cnt,nyp),
                       Growth10y = rep(NA,nyp))
  for(i in yp){ # cycle on 10y intervals
    if(all(c(i,i+1) %in% temp$Year)) {
      dataDf[which(dataDf$Year == i), 'Growth10y'] <-
        temp[which(temp$Year == i+10), 'Value']/temp[which(temp$Year == i), 'Value'] - 1
    }
  } 
  # plot
  plotCnt <- ggplot() + 
    geom_line(    data=dataDf, aes(x = Year, y = Growth10y, colour = CountryName)) + 
    geom_point(   data=dataDf, aes(x = Year, y = Growth10y, colour = CountryName)) 
  if (flagPred){
    plotCnt <- plotCnt +
      geom_line(    data=predDf, aes(x = Year, y = fit,       colour = CountryName)) + 
      geom_point(   data=predDf, aes(x = Year, y = fit,       colour = CountryName)) +
      geom_errorbar(data=predDf, mapping=aes(x = Year, ymin=lwr, ymax=upr), width=.1)
  }
  
  return(plotCnt)
}
  
checkPred <- function(cnt){
  cond = F
  for (i in 1:length(year.pred)){
    cond <- cond | (cnt %in% row.names(X.pred[[i]]))
  }
  return(cond)
}
