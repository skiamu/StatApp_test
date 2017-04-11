######### get functions #############
# here we implement functions for geting things from the dataset

get_Indicators <- function(myTopic=NULL, myYear=NULL,myRegion=NULL,
                           myCountries=NULL,Indicators. = Indicators,
                           Series. = Series,Country. = Country){
  # get_Indicators is a function for extracting the desired indicators
  # from dataframe "Indicators"
  #
  #INPUT:
  #     myTopic = indicators topic [vector of strings] 
  #     myYear = indicators year [vector of int]
  #     myRegion = country geographical region [vector of strings]
  #     myCounties = country names [vector of strings]
  #     
  #
  #OUTPUT:
  #     Indicators = filtered and casted dataframe
  #
  #N.B. 1) before calling the function make sure the dataframes "Indicators",
  #     "Series" and "Country" have been imported. Make also sure the names match.
  #     However, if you don't want to use the dafault name for your dataframe,
  #     pass them as argument to the function
  #     2) we can't have argument x = x, hence the dot(.) 
  #        http://stackoverflow.com/questions/4357101/promise-already-under-evaluation-recursive-default-argument-reference-or-earlie
  #     3) you can avoid the order of the input arguments if you specify the label
  #        for example call the function in this way:
  #        q <- get_Indicators(myYear = myYear,myTopic = myTopic)
  
  library(reshape2)
  library(dplyr)
          
  ####### extract the indicators ###########
  
  if(!is.null(myTopic)){ # Topic is an activated criteria
    my.idx.code <-Series. %>% filter(Topic %in% myTopic) 
    Indicators. <- Indicators. %>% 
      filter(IndicatorCode %in% my.idx.code$SeriesCode)
    
  }
  
  if(!is.null(myYear)){ # Year is an activated criteria
    Indicators. <- Indicators. %>%
      filter(Year %in% myYear)
  }
  
  if(!is.null(myRegion)){ # Region is an activated criteria
    my.country.code <- Country. %>%
      filter(Region %in% myRegion)
      
    Indicators. <- Indicators. %>% 
      filter(CountryCode %in% my.country.code$CountryCode)
  }
  

  
  if(!is.null(myCountries)){ # Countryname is an activated criteria
    # check that all the county names in input are ok
    stopifnot(myCountries %in% TableName)
    
    my.country.name <- Country. %>%
      filter(TableName %in% myCountries)
    Indicators. <- Indicators. %>% 
      filter(CountryCode %in% my.country.name$CountryCode)
    
  }
  
  ######## indicators from observation to variables  ############
  
  # there might be problem with the casting, still thinking about it
  Indicators. <- dcast(Indicators.,formula = CountryCode + Year ~ IndicatorName,
              value.var = "Value")
  
  # rename each row with its country name
  row.names(Indicators.) <- Indicators.$CountryCode
  
  
  return(Indicators.)
  
}


