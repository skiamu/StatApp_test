# Functions for the extraction of the refined datasets

library(reshape2) # dcast
library(dplyr)    # %>%

# source('Filters/functionsFullMatrix.R') # fullness
source(paste(path,"Filters/functionsFullMatrix.R",sep = "/"))

# 01 getIndicators
# getIndicators filter from 'Indicators' the observations which have:
# 1) years      in myYear
# 2) countries  in myCnt       [CountryName]
# 3) indicators in myInd       [IndicatorName]
# 4) topics     in myTopic
# 5) regions    in myRegion    (REM: aggregate countries have region = '')
# giving as an output a data.frame with 4 columns: cnt, ind, year, val 
# (same structure of Indicators, without the Code columns for cnt and ind)

getIndicators <- function(myYear = NULL, myCnt = NULL, myInd = NULL, myTopic = NULL, 
                          myRegion = NULL,myAggregate = NULL,
                          ind = Indicators, ser = Series, count = Country){
   # -- INPUT: 
   #    - myYear     : years                        [vector of int]
   #    - myCnt      : country names                [vector of strings]
   #    - myInd      : indicators                   [vector of strings]
   #    - myTopic    : topics                       [vector of strings]
   #    - myRegion   : country geographical regions [vector of strings]
   #    - ind        : "Indicator" dataframe
   #    - count      : "Country" dataframe    
   #    - ser        : "Series" dataframe 
   # -- OUTPUT:
   #    - Indicators : "Indicator" dataframe filtered
   # -- USES:
   #    - %>%
   #    - dcast
   
   
   if(!is.null(myAggregate)){# i'm looking for aggregate countries
      myAggregate %in% Indicators$CountryName
      # check for mispelling
      if(!all(myAggregate %in% Indicators$CountryName)){
         w <- myAggregate %in% Indicators$CountryName
         print(w)
         cat("nomi errati: ",myAggregate[w],"\n")
         stop("ERROR: aggregate country name not valid")
         
      }   
      Indicators <- Indicators %>%
         filter(CountryName %in% myAggregate)
      
   }
   else{# remove aggregated Country from Indicators
      agg_code <- Country %>%
         filter(Region == "") %>% # agg countries have "Region" blank
         select(CountryCode)
      Indicators <- Indicators %>%
         filter(!(CountryCode %in% agg_code$CountryCode))
      warning("I've eliminated aggregated countries from the dataframe")
   }
   # Years
   if(!is.null(myYear)){ # Year is an activated criteria
      if(!all(myYear %in% Indicators$Year))
         stop("ERROR: at least one year is not present in the dataframe")
      Indicators <- Indicators %>% filter(Year %in% myYear)
   }
   
   # Countries
   if(!is.null(myCnt)){ # Countryname is an activated criteria
      if(!all(myCnt %in% Country$CountryName)){ # CountryName is added in read_data.R
         print('Countries in myCnt not in Country$CountryName :')
         print(setdiff(myCnt, Country$CountryName))
         stop("Error: at least one country is not present in the dataframe Countries")
      }
      my.country.name <- Country %>% filter(CountryName %in% myCnt)
      Indicators <- Indicators %>% filter(CountryCode %in% my.country.name$CountryCode)
   }
   
   # Indicators
   if(!is.null(myInd)){# IndicatorName is an activated criteria
      if(!all(myInd %in% Indicators$IndicatorName)){
         print('Indicators in myInd not in Indicators$IndicatorName :')
         print(setdiff(myInd, Indicators$IndicatorName))
         stop("ERROR: at least one indicator in input is not present in the dataframe")
      }
      Indicators <- Indicators %>% filter(IndicatorName %in% myInd)
   }
   
   # Topics
   if(!is.null(myTopic)){ # Topic is an activated criteria
      my.idx.code <-Series %>% filter(Topic %in% myTopic) 
      Indicators <- Indicators %>% filter(IndicatorCode %in% my.idx.code$SeriesCode)
   }
   else{
      # if there's no Topic in input, warn the user that Indicators
      # are selected from all the Topics
      warning("WARNING:no Topic has been specified, I selected them all")
   }
   
   # Region 
   if(!is.null(myRegion)){ # Region is an activated criteria
      my.country.code <- Country %>% filter(Region %in% myRegion)
      Indicators <- Indicators %>% filter(CountryCode %in% my.country.code$CountryCode)
   }
   
   
   
   return(Indicators)
}

# 02.1 getCntInd
# getCntInd extract for a given year ('year') from a dataframe like Indicators a dataframe with
# rows : countries
# cols : indicators
# if dropNA =T drop the countries with at least one NA
# if showCnt=T print the countries filtered out
getCntInd <- function(df, year, dropNA = T, showCnt = T){
   # -- INPUT: 
   #    - df      : dataframe Indicators-like
   #    - year    : fixed year
   #    - dropNA  : TRUE for dropping the countries with a NA value
   #    - showCnt : TRUE for showing the countries filtered out
   # -- OUTPUT:
   #    - Indicators : "Indicators" dataframe filtered
   # -- USES:
   #    - %>%
   #    - dcast
   df <- filter(df, Year==year)                                      # fix the year
   dc <- dcast(df, CountryName ~ IndicatorName, value.var = "Value") # reshape
   row.names(dc) <- dc$CountryName                                   # set cnt as row names
   dc <- select(dc,-CountryName)
   dcAll <- dc
   if(dropNA){                                                       # drop the NA
      dc <- na.omit(dcAll)
      if(showCnt){                                                    # show the filtered out
         cntIn  <- dc    %>% row.names()
         cntOut <- dcAll %>% row.names() %>% setdiff(cntIn)
         if(length(cntOut)!=0){print(paste(length(cntOut),'Countries out:')); print(cntOut)}
         else{print('No countries has been filtered out')}
      }
   }
   return(dc)
}

# 02.2 getIndYear
# getIndYear extract for a given country ('cnt') from a dataframe like Indicators a dataframe with
# rows : indicators
# cols : years
# if dropNA =T drop the indicators with at least one NA
# if showCnt=T print the indicators filtered out
getIndYear <- function(df, cnt, dropNA = T, showInd = T){
   # -- INPUT: 
   #    - df      : dataframe Indicators-like
   #    - cnt     : fixed country                                     [CountryName]
   #    - dropNA  : TRUE for dropping the countries with a NA value
   #    - showCnt : TRUE for showing the countries filtered out
   # -- OUTPUT:
   #    - Indicators : "Indicators" dataframe filtered
   # -- USES:
   #    - %>%
   #    - dcast
   df <- filter(df, CountryName==cnt)                                # fix the country
   dc <- dcast(df, IndicatorName ~ Year, value.var = "Value")        # reshape
   row.names(dc) <- dc$IndicatorName                                 # set ind as row names
   dc <- select(dc,-IndicatorName)
   dcAll <- dc
   if(dropNA){                                                       # drop the NA
      dcFil <- na.omit(dc)
      if(showInd){                                                    # show the filtered out
         indIn  <- dc    %>% row.names()
         indOut <- dcAll %>% row.names() %>% setdiff(indIn)
         if(length(indOut)!=0){print(paste(length(indOut),'Indicators out:')); print(indOut)}
         else{print('No indicators has been filtered out')}
      }
   }
   return(dc)
}

# 02.3 getCntYear
# getCntYear extract for a given indicator ('ind') from a dataframe like Indicators a dataframe with
# rows : countries
# cols : years
# if dropNA =T drop the countries with at least one NA
# if showCnt=T print the countries filtered out
getCntYear <- function(df, ind, dropNA = T, showCnt = T){
   # -- INPUT: 
   #    - df      : dataframe Indicators-like
   #    - ind     : fixed indicator                                 [CountryName]
   #    - dropNA  : TRUE for dropping the countries with a NA value
   #    - showCnt : TRUE for showing the countries filtered out
   # -- OUTPUT:
   #    - dcFil   : "Indicators" dataframe filtered
   # -- USES:
   #    - %>%
   #    - dcast
   df <- filter(df, IndicatorName==ind)                              # fix the indicator
   dc <- dcast(df, CountryName ~ Year, value.var = "Value")          # reshape
   row.names(dc) <- dc$CountryName                                   # set cnt as row names
   dc <- select(dc,-CountryName)
   dcAll <- dc
   if(dropNA){                                                       # drop the NA
      dc <- na.omit(dc)
      if(showCnt){                                                    # show the filtered out
         cntIn  <- dc    %>% row.names()
         cntOut <- dcAll %>% row.names() %>% setdiff(cntIn)
         if(length(cntOut)!=0){print(paste(length(cntOut),'Countries out:')); print(cntOut)}
         else{print('No countries has been filtered out')}
      }
   }
   return(dc)
}

# 03 unifCnt 
# unifCnt for a dataframe Indicators-like performs an intersection of the countries over years
# so that for each year you will have the same Indicators and the smae countries 
# (the 3D matrix is full)
# if showCnt=T print the countries filtered out
# if showInd=T print the number of missing values for each indicators 
#              [use it to figure out if there are some problematic (over time) indicators]
unifCnt <- function(df, showCnt=T, showInd=T){
   # -- INPUT: 
   #    - df      : dataframe Indicators-like
   #    - showCnt : TRUE for showing the countries filtered out
   #    - showInd : TRUE for showing the number of missing values for each indicators (View)
   # -- OUTPUT:
   #    -         : df without countries outside the intersection
   # -- USES:
   #    - %>%
   #    - dcast
   multDc <- dcast(df, CountryName + Year ~ IndicatorName, value.var = "Value") # reshape df
   if(showInd){ # show the number of missing values for each indicators 
      numNa    <- sapply(multDc[,-c(1,2)], function(x) sum(is.na(x))) # compute the number of NA values
      df_numNa <- data.frame(Value=numNa, row.names=names(numNa))      # dataframe for the View
      colnames(df_numNa) <- 'Number of missing values'                # reset the col name
      View(df_numNa)
   }
   years  <- df$Year        %>% unique() 
   cntAll <- df$CountryName %>% unique() # all the cnt
   cnt    <- cntAll
   for(y in years){
      dc  <- filter(multDc, Year==y)       # filter only the observations in year y
      dc  <- na.omit(dc)                   # drop the cnt with a missing value
      cnt <- intersect(cnt,dc$CountryName) # perform the intersection
   }
   if(showCnt){ # show the filtered out
      cntOut <-  setdiff(cntAll,cnt)
      if(length(cntOut)!=0){print(paste(length(cntOut),'Countries out:')); print(cntOut)}
      else{print('No countries has been filtered out')}
   }
   return(filter(df,CountryName %in% cnt))
}

# 04 get3D
# get3D
# get3D from a dataframe Inidcators-like creates a list of 2D dataframe 'cnt vs ind'
# the i-th element of the list correspond to the i-th year in 'years'
# REM: I need to have 'years' as parameter so I can get the correspondence with the positions 
#      in the list
get3D <- function(df,years){
   # -- INPUT: 
   #    - df    : dataframe Indicators-like
   #    - years : years to pun in the list            [vector of int]
   # -- OUTPUT:
   #    - ll    : list of 2D dataframe 'cnt vs ind' representing the 3D-matrix 
   # -- USES:
   #    - fullness
   #    - getCntInd
   if(!all(years %in% df$Year))
      stop("ERROR: at least one year is not present in the dataframe")
   if(fullness(df)!=1) 
      warning("WARNING: the 3D matrix is not full")
   i <- 1
   ll <- list()
   for(y in years){
      ll[[i]] <- getCntInd(df, y, dropNA = F, showCnt = F)
      i = i+1
   }
   return(ll)
}
