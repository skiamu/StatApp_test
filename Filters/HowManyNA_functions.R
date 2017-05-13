# HowManyNA functions
# Last update: 17-04
# Leo

# use these functions to understand if:
#       - the dataset you are considering is full
#       - the filter you applied improved the 'fullness' of your dataset
#       - if a particular country or index is problematic

# REM consider to slightly modify them for a specific use
# A LOT OF DOUBTS... Seems fixed


# 00 DimInd ####
# this function compute the dimension of the 3d matrix containing ind
# -- INPUT:
#    - ind       : data.frame of the indicators
# -- OUTPUT:
#    - dim3d : vector with no. of ind, no. of cnt, no. of year (in this order)
# -- USES:
DimInd <- function(ind){
  dim3d <- c(n_distinct(ind$IndicatorCode),
             n_distinct(ind$CountryCode),
             n_distinct(ind$Year))
}
# 01 HowManyNA ####
# this function compute the total percentage of NA value in the 3d matrix
# it will also plot a graph showing the percentage of NA values for each year 
# -- INPUT:
#    - ind       : data.frame of the indicators
#    - Flag_Plot : TRUE if you need the plot
# -- OUTPUT:
#    - totNAperc : percentage of NA value in the 3d matrix induced by ind
# -- USES:
#    - %>%
HowManyNA <- function(ind,Flag_Plot=TRUE){
  # theoretical dimension of the 3d matrix
  teo_dim <- n_distinct(ind$CountryCode)*n_distinct(ind$IndicatorCode)*n_distinct(ind$Year)
  # total percentage of NA values
  totNAperc <- 100*(1-dim(ind)[1]/teo_dim)
  # NA percentage per each year
  df1 <- data.frame(table(ind$Year)); colnames(df1) <- c('Year','Freq')
  df1['NA_perc'] <- 100*(1-df1$Freq/(n_distinct(ind$CountryCode)*n_distinct(ind$IndicatorCode)))
  # plot with some information in the subtitle
  if(Flag_Plot){
    p <- ggplot(df1, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle('Percentage of NA value for each year',subtitle = paste('Total percentage of NA values =',totNAperc,'%'))
    x11(); print(p)
  }
  totNAperc
}
# 02 HowManyNA_cnt ####
# this function compute the percentage of NA values for each year for the country cnt
# it will also plot a graph showing the dataframe in output
# -- INPUT:
#    - ind       : data.frame of the indicators
#    - cnt       : country selected (CountryName)
#    - Flag_Plot : TRUE if you need the plot
# -- OUTPUT:
#    - df1 : data.frame with the percentage of NA values for each year
# -- USES:
#    - %>%
HowManyNA_cnt <- function(ind,cnt,Flag_Plot=TRUE){
  # number of distinct indicators for the country 'cnt'
  num_ind_cnt <- ind %>% filter(CountryName == cnt) %>% select(IndicatorCode) %>% n_distinct()
  # compute the NA percentage for each year
  df1 <- ind %>% filter(CountryName == cnt) %>% group_by(Year) %>% 
    summarise(NA_perc = 100*(1-n_distinct(IndicatorCode)/num_ind_cnt))
  # Plot
  if(Flag_Plot){
    p <- ggplot(df1, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle(paste('Percentage of NA value for each year for',cnt))
    x11(); print(p)
  }
  df1
}
# 03 HowManyNA_indicator ####
# this function compute the percentage of NA values for each year for the indicator 'indicator'
# it will also plot a graph showing the dataframe in output with in addition the first year 
# in which the indicator appears (in the subtitle)
# -- INPUT:
#    - ind       : data.frame of the indicators
#    - indicator : indicator selected (IndicatorName)
#    - Flag_Plot : TRUE if you need the plot
# -- OUTPUT:
#    - df1 : data.frame with the percentage of NA values for each year
# -- USES:
#    - %>%
HowManyNA_indicator <- function(ind,indicator,Flag_Plot=TRUE){
  # number of distinct indicators for 'indicator'
  num_cnt_ind <- ind %>% filter(IndicatorName == indicator) %>% select(CountryCode) %>% n_distinct()
  # compute the NA percentage for each year
  df1 <- ind %>% filter(IndicatorName == indicator) %>% group_by(Year) %>% 
    summarise(NA_perc = 100*(1-n_distinct(CountryCode)/num_cnt_ind))
  # Plot
  if(Flag_Plot){
    p <- ggplot(df1, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle(paste('Percentage of NA value for each year for',indicator))
    x11(); print(p)
  }
  df1
}
# 01 HowManyNA_old ####
HowManyNA_old<- function(ind,Flag_Plot=TRUE){
  # theoretical dimension of the 3d matrix
  teo_dim <- n_distinct(ind$CountryCode)*n_distinct(ind$IndicatorCode)*n_distinct(ind$Year)
  print(paste('cnt:',n_distinct(ind$CountryCode),'  ind',n_distinct(ind$IndicatorCode),'  year',n_distinct(ind$Year)))
  # percentage of NA values
  totNAperc <- 100*(1-dim(ind)[1]/teo_dim)
  # Freq = number of observation per year
  df1 <- data.frame(table(ind$Year)); colnames(df1) <- c('Year','Freq')
  # theoretical dimension of the 2d matrix (for each year)
  df2 <- fil_ind_04 %>% group_by(Year) %>% 
    summarize(n_ind = n_distinct(IndicatorCode), 
              n_cnt = n_distinct(CountryCode), 
              Full  = n_distinct(IndicatorCode)*n_distinct(CountryCode))
  # merge the two tables
  df3 <- df1 %>% merge(df2,by='Year')
  # compute the NA percentage for each year
  df3['NA_perc'] <- 100*(1-df3$Freq/df3$Full)
  # select only the useful columns (unuseful)
  df4 <- df3 %>% select(Year,NA_perc)
  # plot with some information in the subtitle
  if(Flag_Plot){
    p <- ggplot(df4, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle('Percentage of NA value for each year',subtitle = paste('Total percentage of NA values =',totNAperc,'%'))
    x11(); print(p)
  }
  totNAperc
}
# 02 HowManyNA_cnt_old ####
HowManyNA_cnt_old <- function(ind,cnt,Flag_Plot=TRUE){
  # number of distinct indicator for each year
  df0 <- ind %>% group_by(Year) %>% summarise(n_ind_tot = n_distinct(IndicatorCode))
  # number of distinct indicator for each year for cnt, then merged
  df1 <- ind %>% filter(CountryName == cnt) %>% group_by(Year) %>% 
    summarise(n_ind_cnt = n_distinct(IndicatorCode)) %>% merge(df0,by='Year')
  # compute the percentage
  df1['NA_perc']=100*(1-df1$n_ind_cnt/df1$n_ind_tot)
  # Plot
  if(Flag_Plot){
    p <- ggplot(df1, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle(paste('Percentage of NA value for each year for',cnt))
    x11(); print(p)
  }
  df1
}
# 03 HowManyNA_indicator_old ####
HowManyNA_indicator_old <- function(ind,indicator,Flag_Plot=TRUE){
  # number of distinct indicator for each year
  df0 <- ind %>% group_by(Year) %>% summarise(n_cnt_tot = n_distinct(CountryCode))
  # number of distinct indicator for each year for cnt, then merged
  df1 <- ind %>% filter(IndicatorName == indicator) %>% group_by(Year) %>% 
    summarise(n_cnt_ind = n_distinct(CountryCode)) %>% merge(df0,by='Year')
  # compute the percentage
  df1['NA_perc']=100*(1-df1$n_cnt_ind/df1$n_cnt_tot)
  # Plot
  if(Flag_Plot){
    p <- ggplot(df1, aes(x=Year,y=NA_perc)) + 
      geom_point() + 
      ggtitle(paste('Percentage of NA value for each year for',indicator),
              subtitle = paste('First year for the indicator:',min(df1$Year)))
    x11(); print(p)
  }
  df1
}