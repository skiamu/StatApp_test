# useful function for extracting a full Matrix

library(dplyr)                  # %>%

# 01 fIlterCountriesRegion ####
# this function filters OUT of the dataframe 'ind' the countries 
# that belong to the region 'reg'
# REM: the aggregate countries have region = ''
# -- INPUT:
#    - ind : data.frame of the indicators
#    - cnt : data.frame of the countries
#    - reg : region to filter out
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
fIlterCountriesRegion <- function(ind,cnt,reg){
  fil_cnt <- cnt %>% 
    filter(Region != reg) %>% 
    select(c(CountryCode))
  fil_ind <- ind %>% 
    filter(ind$CountryCode %in% fil_cnt$CountryCode)
}

# 02 dimInd ####
# this function compute the dimension of the 3d matrix containing ind
# -- INPUT:
#    - ind       : data.frame of the indicators
# -- OUTPUT:
#    - dim3d : vector with no. of ind, no. of cnt, no. of year (in this order)
# -- USES:
dimInd <- function(ind){
  dim3d <- c(n_distinct(ind$IndicatorCode),
             n_distinct(ind$CountryCode),
             n_distinct(ind$Year))
}

# 03 fullness ####
# this function compute the fullness of the 3d matrix containing ind
# -- INPUT:
#    - ind       : data.frame of the indicators
# -- OUTPUT:
#    - fullness  : percentage of non NA values in the 3d matrix
# -- USES:
#    - dimInd
fullness <- function(ind){
  dim3d <- dimInd(ind)
  fullness <- (dim(ind)[1])/(dim3d[1]*dim3d[2]*dim3d[3])
}


# 02 



# 04 extract2DmatrixWithFullestIndicators ####
# this function extract from th matrix Indicators:
#  A- a selection of non super small states
#  B- a selection of the M fullest indicators
#  C- from a only year 'fixYear'
# REM : it is very delicate use it extremely carefully!
# -- INPUT:
#    - Indicators  : data.frame of the indicators
#    - M           : show the best M indicators
#    - fixYear     : fix this year,                   default 2010
#    - Tind        : fix the threshold,               default 400
#    - viewFlag    : TRUE to see the best indicators, default TRUE
# -- OUTPUT:
#    - temp4       : Indicators filtered with the countries and the indicators found
# -- USES:
#    - fIlterCountriesRegion
extract2DmatrixWithFullestIndicators <- function(Indicators,M,fixYear=2010,Tind=400,viewFlag=TRUE){
  temp1 <- Indicators %>%                  # fix 2010
    filter(Year == fixYear)           
  temp2 <- temp1 %>%                       # out the aggregate conutries
    fIlterCountriesRegion(Country,'')    
  cnt2 <- temp2 %>%                        # countries with too few indicators (under Tind)
    group_by(CountryName) %>% 
    summarise(numInd = n_distinct(IndicatorCode)) %>% 
    filter(numInd>Tind) 
  temp3 <- temp2 %>%                       # out the countries with too few indicators
    filter(temp2$CountryName %in% cnt2$CountryName)
  ind3 <- temp3 %>% 
    group_by(IndicatorName) %>% 
    summarise(numCnt = n_distinct(CountryCode)) %>%
    arrange(desc(numCnt))
  ind4 <- ind3[1:M,]
  if(viewFlag) {View(ind4)}
  temp4 <- temp3 %>%                       # in the first M indicators
    filter(temp3$IndicatorName %in% ind4$IndicatorName)
  return(list(x = ind4, y = temp4))
}

