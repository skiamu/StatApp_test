######### get functions #############
# here we implement functions for getting things from the dataset

# REMARK: here are some "Countryname" you can use in the argument 
# "myAgg" if you want to do analysis at an aggregateed level
# 
# 
# [1] "Arab World"                                    
# [2] "Caribbean small states"                        
# [3] "Central Europe and the Baltics"                
# [4] "East Asia & Pacific (all income levels)"       
# [5] "East Asia & Pacific (developing only)"         
# [6] "Euro area"                                     
# [7] "Europe & Central Asia (all income levels)"     
# [8] "Europe & Central Asia (developing only)"  GDP per capita (current US$)     
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
                           myInd.Name = NULL,
                           ind = Indicators,
                           ser = Series,
                           count = Country,
                           clear_name = F){
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
   #     myAggregate = aggregated states from the list above [vector of string]
   #                   if this argument is NULL the aggregated countries will be
   #                   eliminated             
   #     clear_name = TRUE if you don't want the unit of measure in the 
   #                  indicator names, FALSE otherwise (FALSE dafault)
   #     ind = "Indicator" dataframe
   #     count = "Country" dataframe    
   #     ser = "Series" dataframe         
   #
   #
   #OUTPUT:
   #     Indicators = final dataframe ready for the analysis.
   #                  row : Country name
   #                  colums : indicator names
   #                  example:
   #                           GDP   population    growth
   #                  italy    18    75485         7
   #                  france   17    12545         6
   #                  germany  35    652148        8
   #
   
   library(reshape2)
   library(dplyr)
   
   ####### extract the indicators ###########
   if(!is.null(myInd.Name)){# IndicatorName is an activated criteria
      if(!all(myInd.Name %in% Indicators$IndicatorName))
         stop("ERROR: at least one Indicator name in input is not present in the dataframe")
      Indicators <- Indicators %>%
         filter(IndicatorName %in% myInd.Name)
   }
   
   if(!is.null(myTopic)){ # Topic is an activated criteria
      my.idx.code <-Series %>% filter(Topic %in% myTopic) 
      Indicators <- Indicators %>% 
         filter(IndicatorCode %in% my.idx.code$SeriesCode)
   }
   else{
      # if there's no Topic in input, warn the user that Indicators
      # are selected from all the Topics
      warning("WARNING:no Topic has been specified, I selected them all")
      
   }
   
   
   if(!is.null(myYear)){ # Year is an activated criteria
      if(!all(myYear %in% Indicators$Year))
         stop("ERROR: at least one year is not present in the dataframe")
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
   else{# remove aggregated Country from Indicators
      agg_code <- Country %>%
         filter(Region == "") %>% # agg countries have "Region" blank
         select(CountryCode)
      Indicators <- Indicators %>%
         filter(!(CountryCode %in% agg_code$CountryCode))
      warning("I've eliminated aggregated countries from the dataframe")
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
   
   # for each (CountryCode,Year) let IndicatorName be a variable (column)
   Indicators <- dcast(Indicators,formula = CountryCode + Year ~ IndicatorName,
                        value.var = "Value")
   
   # clear unit of measure from indicator name
   if(clear_name){
      names(Indicators) <- gsub("\\(.*$", "", names(Indicators))
      
   }

   return(Indicators)
   
}

















