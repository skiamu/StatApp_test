# functions for managing the names of the variables

# read_data.R saves two dataframe with the correspondence of codes and names for cnt and ind:
# cntNameCode,indNameCode

library(dplyr)

# the following functions receive in input a vector of names (codes) and give as an output
# the vector of correspondent codes (names)
# 01.1 name2codeCnt
name2codeCnt <- function(names, cntNameCode){
  codes <- cntNameCode %>% filter(CountryName %in% names)
  return(codes$CountryCode)
}

# 01.2 code2nameCnt
code2nameCnt <- function(codes, cntNameCode){
  names <- cntNameCode %>% filter(CountryCode %in% codes)
  return(names$CountryName)
}

# 02.1 name2codeInd
name2codeInd <- function(names, indNameCode){
  codes <- indNameCode %>% filter(IndicatorName %in% names)
  return(codes$IndicatorCode)
}

# 02.2 code2nameInd
code2nameInd <- function(codes, indNameCode){
  names <- indNameCode %>% filter(IndicatorCode %in% codes)
  return(names$IndicatorName)
}

# 03   indShortName [empty]
