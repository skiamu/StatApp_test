# run extract a 'full' matrix

setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")

library(reshape2)
source('functionsFullMatrix.R')
source("read_data.R")
load('data.RData')

# df similar to indicators
# dc after dcast

M <- 30
df <- extract2DmatrixWithFullestIndicators(Indicators,M,viewFlag=TRUE) # another name for the function?
print(dimInd(df))
print(fullness(df))

# manual selection [a very rapid choice, this is the delicate point]
bestInd <- c('Access to electricity (% of population)',
             'Population, total',
             'Birth rate, crude (per 1,000 people)',
             'CO2 emissions (kt)',
             'International migrant stock, total')

df1 <- df %>% filter(df$IndicatorName %in% bestInd) # could be done with a function
print(dimInd(df1))
print(fullness(df1))

# have a look to the NA values
dc2 <- df1 %>%
  select(CountryName,IndicatorName,Value) %>%
  dcast(CountryName ~ IndicatorName, value.var='Value')
View(dc2)
# problematic countries
probCountries <- c('St. Kitts and Nevis',
                   'Kosovo')
# drop them
df3 <- df1 %>% filter(!df1$CountryName %in% probCountries) # could be done with a function
print(dimInd(df3))
print(fullness(df3)) # 100%

dc3 <- dc2 %>% filter(!dc2$CountryName %in% probCountries)
View(dc3)

# On dc3 we can perform PCA
source('PCA/PCA_function.R')
PC(dc3[,-1])
