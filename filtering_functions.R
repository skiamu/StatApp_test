# Filtering functions
# Last update: 11-04
# Leo

# 01 FIlter_Countries_Region ####
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
FIlter_Countries_Region <- function(ind,cnt,reg){
  fil_cnt <- cnt %>% 
    filter(Region != reg) %>% 
    select(c(CountryCode))
  fil_ind <- ind %>% 
    filter(ind$CountryCode %in% fil_cnt$CountryCode)
}

# 02 Filter_Years_With_Too_Low_Mean ####
# this function filters out of the dataframe 'ind' the years 
# with mean number of indicators among countries below T1
# -- INPUT:
#    - ind : data.frame of the indicators
#    - T1  : threshold for the yearly mean among indicators
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
Filter_Years_With_Too_Low_Mean <- function(ind,T1){
  year_ok <- ind %>% 
    group_by(Year,CountryCode) %>% 
    summarise(NumIND = length(IndicatorCode)) %>% 
    group_by(Year) %>%
    summarise(Mean = mean(NumIND)) %>%
    filter(Mean > T1) %>%
    select(Year)
  fil_ind <- ind %>% 
    filter(ind$Year %in% year_ok$Year)
}

# 03 Filter_Countries_With_Too_Few_Ind_WRT_Mean ####
# this function filters out of the dataframe 'ind' the countries 
# with too few indicators with respect to the mean, in details:
# if for at least one year the countries has a number of indicators
# smaller than T2 then the country is filtered out
# -- INPUT:
#    - ind : data.frame of the indicators
#    - T2  : threshold for the minimum fraction of the mean accepted
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
Filter_Countries_With_Too_Few_Ind_WRT_Mean <- function(ind,T2){
  year_ind <- ind %>%
    group_by(Year,CountryCode) %>% 
    summarise(NumIND = length(IndicatorCode)) 
  year_mean <- year_ind %>%
    group_by(Year) %>%
    summarise(Mean = mean(NumIND))
  cnt_2 <- year_ind %>%
    merge(year_mean, by='Year') %>%
    group_by(CountryCode) %>%
    summarise(Cond = all(NumIND > T2 * Mean)) %>%
    filter(Cond) %>%
    select(CountryCode)
  fil_ind <- ind %>%
    filter(ind$CountryCode %in% cnt_2$CountryCode)
}

# 04 Filter_Countries_With_Too_Low_Pop ####
# this function filters out of the dataframe 'ind' the countries 
# with mean population among years below T3
# -- INPUT:
#    - ind : data.frame of the indicators
#    - T3  : threshold for the minimum population accepted
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
Filter_Countries_With_Too_Low_Pop <- function(ind,T3){
  # Compute mean population among years and select only the countries with mean > T3
  # REM: if a value is missing it is NOT counted as 0!
  temp <- ind %>%
    filter(IndicatorCode == 'SP.POP.TOTL') %>%
    group_by(CountryCode) %>% 
    summarise(Mean = mean(Value)) %>%
    filter(Mean > T3) %>%
    select(CountryCode)
  fil_ind <- ind %>% 
    filter(ind$CountryCode %in% temp$CountryCode)
}

