
read_data <- function(path){
  # function for reading the dataframes
  #
  # INPUT: 
  #     path = directory path (where the .csv are)
  
  # rm(list = ls())
  
  Country <- read.csv(paste(path,"Country.csv",sep = "/"), 
                      header= T)
  
  
  CountryNotes <- read.csv(paste(path,"CountryNotes.csv",
                                 sep="/"), header= T)
  
  
  Indicators <- read.csv(paste(path,"Indicators.csv",sep="/"),
                         header = T)
  
  Series <- read.csv(paste(path,"Series.csv",sep = "/"), 
                     header = T)
  
  # in R we can return just a single object,
  # i return a list of dataframes
  mylist <- list(County = Country,
                 CountryNotes = CountryNotes,
                 Indicators = Indicators,
                 Series = Series)
  
  return(mylist)
}











