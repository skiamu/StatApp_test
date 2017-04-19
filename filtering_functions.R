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
# 05 Filter_Indicators_Too_Recent ####
# this function filters out of the dataframe 'ind' the indicators 
# that start after TY
# -- INPUT:
#    - ind : data.frame of the indicators
#    - TY  : threshold for the maximum starting year accepted
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
Filter_Indicators_Too_Recent <- function(ind,TY){
  df1 <- ind %>%
    group_by(IndicatorCode) %>%
    summarise(min_year = min(Year)) %>%
    filter(min_year <= TY)
  fil_ind <- ind %>%
    filter(ind$IndicatorCode %in% df1$IndicatorCode)
}
# 06 Filter_Indicators_That_Ended ####
# this function filters out of the dataframe 'ind' the indicators 
# that ended before TY
# -- INPUT:
#    - ind : data.frame of the indicators
#    - TY  : threshold for the minimum ending year accepted
# -- OUTPUT:
#    - fil_ind : ind filtred
# -- USES:
#    - %>%
Filter_Indicators_That_Ended <- function(ind,TY){
  df1 <- ind %>%
    group_by(IndicatorCode) %>%
    summarise(max_year = max(Year)) %>%
    filter(max_year >= TY)
  fil_ind <- ind %>%
    filter(ind$IndicatorCode %in% df1$IndicatorCode)
}

# 07 Filter_Countries_With_Too_Many_NA ####
#  !!! there is a problem with 'St. Martin (French part)'
#  !!! and maybe others 
# this function filters out of the dataframe countries with a mean (over years)
# percentage of NA values under T_NA
# -- INPUT:
#    - ind       : data.frame of the indicators
#    - T_NA      : threshold for the minimum NA percentage accepted
#    - Flag_Plot : TRUE if you need the plot
# -- OUTPUT:
#    - df3       : ind filtred
# -- USES:
#    - %>%
#    - HowManyNA_cnt
Filter_Countries_With_Too_Many_NA <- function(ind,T_NA,Flag_Plot = TRUE){
  # suboptimal...
  # countries
  cnt <- ind %>% select(CountryName) %>% unique()
  # initialize the vector that will contain the NA_perc
  mean_NA_perc <- vector(mode="numeric", length=dim(cnt)[1])
  for(i in 1:length(mean_NA_perc)){
    df1 <- HowManyNA_cnt(ind,cnt[i,'CountryName'],Flag_Plot = FALSE)
    mean_NA_perc[i] <- mean(df1$NA_perc)
  }
  df2 <- data.frame(CountryName = cnt,
                    MeanNAPerc  = mean_NA_perc)
  df3 <- df2 %>% filter(MeanNAPerc < T_NA)
  # Plot
  if(Flag_Plot){
    p <- ggplot(df3, aes(x = reorder(CountryName, +MeanNAPerc), y = MeanNAPerc, label=CountryName)) +
      geom_point() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle('Mean percentage of NA value for each state') +
      geom_hline(yintercept = T_NA) + 
      labs(x = '', y = 'Mean percentage of NA values')
    x11(); print(p)
  }
  df3
}
