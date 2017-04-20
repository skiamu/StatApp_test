######### get functions #############
# here we implement functions for getting things from the dataset

# REMARK: here are some "Countryname" you can use in the argument 
# "myCountries" if you want to do analysis at an aggregateed level
# 
# 
# [1] "Arab World"                                    
# [2] "Caribbean small states"                        
# [3] "Central Europe and the Baltics"                
# [4] "East Asia & Pacific (all income levels)"       
# [5] "East Asia & Pacific (developing only)"         
# [6] "Euro area"                                     
# [7] "Europe & Central Asia (all income levels)"     
# [8] "Europe & Central Asia (developing only)"       
# [9] "European Union"                                
# [10] "Fragile and conflict affected situations"      
# [11] "Heavily indebted poor countries (HIPC)"        
# [12] "High income"                                   
# [13] "High income: nonOECD"                          
# [14] "High income: OECD"                             
# [15] "Latin America & Caribbean (all income levels)" 
# [16] "Latin America & Caribbean (developing only)"   
# [17] "Least developed countries: UN classification"  
# [18] "Low & middle income"                           
# [19] "Low income"                                    
# [20] "Lower middle income"                           
# [21] "Middle East & North Africa (all income levels)"
# [22] "Middle East & North Africa (developing only)"  
# [23] "Middle income"                                 
# [24] "North America"                                 
# [25] "OECD members"                                  
# [26] "Other small states"                            
# [27] "Pacific island small states"                   
# [28] "Small states"                                  
# [29] "South Asia"                                    
# [30] "Sub-Saharan Africa (all income levels)"        
# [31] "Sub-Saharan Africa (developing only)"          
# [32] "Upper middle income"                           
# [33] "World"              

get_Indicators <- function(myTopic=NULL, 
                           myYear=NULL,
                           myRegion=NULL,
                           myCountries=NULL,
                           myAggregate = NULL,
                           ind = Indicators,
                           ser = Series,
                           count = Country){
   # get_Indicators is a function for extracting the desired indicators
   # from dataframe "Indicators"
   #
   #
   #INPUT:
   #     myTopic = indicators topic [vector of strings] 
   #     myYear = indicators year [vector of int]
   #     myRegion = country geographical region [vector of strings]
   #     myCounties = country names, it could be also a string
   #                  from the list above [vector of strings]
   #     
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
      my.idx.code <-Series %>% filter(Topic %in% myTopic) 
      Indicators <- Indicators %>% 
         filter(IndicatorCode %in% my.idx.code$SeriesCode)
   }
   else{
      stop("ERROR: you must select a topic")
      
   }
   
   if(!is.null(myYear)){ # Year is an activated criteria
      Indicators <- Indicators %>%
         filter(Year %in% myYear)
   }
   
   
   if(!is.null(myRegion)){ # Region is an activated criteria
      my.country.code <- Country %>%
         filter(Region %in% myRegion)
      
      Indicators <- Indicators %>% 
         filter(CountryCode %in% my.country.code$CountryCode)
   }
   
   if(!is.null(myAggregate)){# i'm looking for aggregate countries
      # check for mispelling
      if(!all(myAggregate %in% Indicators$CountryName))
         stop("ERROR: aggregate country name not valid")
      Indicators <- Indicators %>%
         filter(CountryName %in% myAggregate)
      
   }
   
   if(!is.null(myCountries)){ # Countryname is an activated criteria
      # check that all the county names in input are ok
      if(!all(myCountries %in% Country$TableName))
         stop("Error: some Countries in input are not in TableName")
      my.country.name <- Country %>%
         filter(TableName %in% myCountries)
      Indicators <- Indicators %>% 
         filter(CountryCode %in% my.country.name$CountryCode)
      
   }
   
   ######## indicators from observation to variables  ############
   
   # there might be problem with the casting, still thinking about it
   Indicators <- dcast(Indicators,formula = CountryCode + Year ~ IndicatorName,
                        value.var = "Value")
   
   # rename each row with its country name
   #row.names(Indicators.) <- Indicators.$CountryCode
   # this line works just when myYear is a scalar.
   # not a big problem, leave it like that for know
   
   return(Indicators)
   
}


