# this function receive the contry and plot the 10y growth plot with predictions


# to run these function you need to have loaded:
# 1. prediction.RData (year.pred, X.pred)

# to get the right notation
# setting year.pred[i] = tau
# the corresponding predictions in X.pred  are prediction for the quantity Y~(tau) [the ~ should be seen as upon the Y]
# where Y~(tau) = ( GDP(tau+10) -GDP(tau) ) / GDP(tau)
# and it is obtained with data in tau, namley Y~(tau) = beta * Z(tau)

# however in the plot we want to have Y(t) vs t with
# Y(t) = ( GDP(t) - GDP(t-10) ) / GDP(t-10) 
# and this is obtained by the previous with t = tau + 10

# I use the real Y until 2013
# in 2014, 2015 I would Y for some countries but to make things uniform I  will have:
# exacta Y until 2013, predicted Y from 2014

getPred <- function(cnt,y=year.pred,X=X.pred){
  # this function extract the dataset with the predictions for cnt from year.pred and X.pred
  nY <- length(year.pred) # num of years for which there are predictions
  yNA <- NULL             # set of years with no prediction
  predDf <- NULL          # dataframe of the predictions for cnt 
  for (i in 1:nY){                         # for each year for which there are predictions (for at least one country)
    if (cnt %in% row.names(X.pred[[i]])) { # if there are predicions for that year
      predDf <- rbind( predDf,             # add the prediction to predDf
                       c(Year = year.pred[i]+10, X.pred[[i]][cnt,]) )
    } else { yNA <- c(yNA, i) }            # else store the year with no pred
  }
  for (i in yNA) {                         # for each year with no pred
    predDf <- rbind( predDf,               # fill the dataset with NA
                     c(Year = year.pred[i]+10, rep(NA,3)) )
  }
  if (all(colSums(is.na(predDf[,-1]))==nY)){ warning('There are no prediction for this country') } 
  predDf <- as.data.frame(predDf)
  predDf$CountryName <- rep(cnt,nY)
  return(predDf)
}

getGrow <- function(cnt,y=year.pred,X=X.pred,GDP="GDP per capita (constant 2005 US$)"){
  # this function extract from Indicators the 10y growth
  temp <- getIndicators(myInd = GDP,myCnt = cnt,myYear = 1960:2013) %>% 
    select(CountryName,Year,Value)            # extract the annual growth       
  y  <- sort(unique(temp$Year))               # years for which there is annual growth
  #yp <- y[-((length(y)-10+1):(length(y)))]    # years in which the 10y growth can be computed
  yp <- y[-(1:10)]                            # years in which the 10y growth can be computed
  nyp <- length(yp)                           # number of years for which the 10y growth can be computed
  dataDf <- data.frame(Year = yp,             # initialize the output
                       CountryName = rep(cnt,nyp),
                       Growth10y = rep(NA,nyp))
  for(i in yp){                               # cycle on 10y intervals
    if(all(c(i-10,i) %in% temp$Year)) {       # if t-10 and t in the collected data
      dataDf[which(dataDf$Year == i), 'Growth10y'] <-
        temp[which(temp$Year == i), 'Value']/temp[which(temp$Year == i-10), 'Value'] - 1
    }
  }
  return(dataDf)
}

plot10yPred <- function(cnt,cnt2=NULL,flagPred=T,GDP="GDP per capita (constant 2005 US$)"){
  predDf <- getPred(cnt,y=year.pred,X=X.pred)
  dataDf <- getGrow(cnt,y=year.pred,X=X.pred,GDP="GDP per capita (constant 2005 US$)")
  
  # if a 2' cnt is selected, its data are attached [NO predictions for the second, otherwise plot too messy]
  if(!is.null(cnt2)){ 
    dataDf2 <- getGrow(cnt2,y=year.pred,X=X.pred,GDP="GDP per capita (constant 2005 US$)")
    dataDf <- rbind(dataDf,dataDf2)
  }
  
  # plot
  plotCnt <- ggplot() + 
    theme_economist() + 
    geom_hline(yintercept = 0, colour = 'black') + 
    geom_line(    data=dataDf, aes(x = Year, y = Growth10y, colour = CountryName), size = 2) + 
    geom_point(   data=dataDf, aes(x = Year, y = Growth10y, colour = CountryName), size = 3) +
    labs(color = "Country")
  if (flagPred){
    plotCnt <- plotCnt +
      #geom_line(    data=predDf, aes(x = Year, y = fit, colour = CountryName), size = 2) +
      geom_errorbar(data=predDf, mapping=aes(x = Year, ymin=lwr, ymax=upr), 
                    width=1, size=1, color="grey30") + 
      geom_point(   data=predDf, aes(x = Year, y = fit),                       size = 3, shape=21, fill="white")
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
