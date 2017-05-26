# clear_na deletes a row from X if there's at least one NA in it.
# It shows which row has been eliminated
#
# INPUT:
#      X = dataframe we want to clear
#      Country = dataframe with country information
#      print = T if you want to know which countries have been eliminated
# OUTPUT:
#      X_clear = dataframe X without na

clear_na <- function(X, Country, print = T){
   
   library(dplyr) # %>%
   # remove row with na
   X_clear <- na.omit(X)
   # extract the CountryCode of the eliminated countries
   code <- X %>%
      select(CountryCode) %>%
      filter(!(X$CountryCode %in% X_clear$CountryCode)) 
   # extract the full name of the eliminated countries
   full_name <- filter(Country,CountryCode %in% code$CountryCode) %>%
      select(ShortName)
   # convert factor to char
   full_name <- as.character(full_name[,1])
   # print the eliminated countries
   cat("I've eliminated the following countries: ","\n")
   for(i in 1:length(full_name)){
      cat(paste(i, full_name[i],sep = " "),"\n")
   }
   # return the dataframe without na
   return(X_clear)
}