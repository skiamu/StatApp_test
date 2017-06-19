# function to pass from std to unstd data

# find the years in which a certain state has all the features
findYears <- function(ind,cnt){
  temp  <- getIndicators(myInd = ind, myCnt = cnt)
  dc   <- getYearInd(temp,cnt,showY=F)
  return(list(years=row.names(dc),dc=dc))
}

#test <- findYears(myIndAgr,'Italy')

findValues <- function(dc,year){
  return(dc[as.character(year),])
}

#val <- findValues(test$dc,2000)

std <- function(val,mm,vv){
  return((val-mm)/vv)
}

#valS <- std(val,meanAgr,varAgr)
