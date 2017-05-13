# HOW TO USE THIS FUNCTION:
# this function has to be run just once.
# 1) import the function with --> source("read_data.R")
# 2) call the function --> read_data("your_folder_path")
# 3) load the data --> load("data.RData")
# from now on just load the data as in step 3).
# Remark: if you are in a subfolder and the file "data.RData" is upstream
# you need to specify the full path (somehow)


read_data <- function(path){
  # function for reading the dataframes. it converts long and ugly names with
  # better and shorter ones, cuts the unit of measure within "()" from
  # IndicatorName, saves a file .RData ready to be loaded for data analysis
  #
  # INPUT: 
  #     path = directory path (where the .csv are)
  #
  # OUTPUT: none
  
  library(plyr) # function revalue
  library(dplyr) # %>%
   
  Country <- read.csv(paste(path,"Country.csv",sep = "/"), 
                      header= T)
  
  
  CountryNotes <- read.csv(paste(path,"CountryNotes.csv",
                                 sep="/"), header= T)
  
  
  Indicators <- read.csv(paste(path,"Indicators.csv",sep="/"),
                         header = T)
  
  # let's change some awful Country names in Indicators
  correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")
  Indicators <- Indicators %>%
     transform(CountryName=revalue(CountryName,correction))
  
  # let's change some awful Country names in Country
  Country <- Country %>%
     transform(TableName=revalue(TableName,correction),
               ShortName=revalue(ShortName,correction))
  
  # # let's cut the unit of measure in "indicatorName" 
  # Indicators <- Indicators %>%
  #    transform(IndicatorName = gsub("\\(.*$", "", IndicatorName))
  
  Series <- read.csv(paste(path,"Series.csv",sep = "/"), 
                     header = T)
  
  # save the dataframes in a file for convenience
  save(Indicators,Country,CountryNotes,Series,file = "data.RData")
  
 
}











