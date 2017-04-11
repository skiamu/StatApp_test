# Filter the dataset
# 11-04-17
# Leo


# 01 ------- Set working directory (set yours) and import libraries (check you installed them) #####
#setwd('/Users/mowythebest/Desktop/Ingegneria matematica/5.2 Applied Statistics/world-development-indicators')
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/Playground')
# Diana
# Andrea      Paste yours
# Victor
library(dplyr)      # %>%
library(reshape2)   # dcast
library(ggplot2)    # ggplot
library(data.table) # setnames

source('filtering_functions.R')

# 02 ------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')

# 03 ------- Filter out the aggregate countries ####
# The aggregate countries have the variable 'Region' blank

fil_ind_03 <- FIlter_Countries_Region(ind,country,'')

# 04 ------- Filter the years with too few indicators ####

T1 <- 200
fil_ind_04 <- Filter_Years_With_Too_Low_Mean(fil_ind_03,T1)

# 05 ------- Filter the countries with too few indicators with respect to the mean ####

T2 <- 0.12
fil_ind_05 <- Filter_Countries_With_Too_Few_Ind_WRT_Mean(fil_ind_04,T2)

# 06 ------- Filter the countries with too few population ####

T3 <- 100
fil_ind_06 <- Filter_Countries_With_Too_Low_Pop(fil_ind_05,T3)

# 06.01 ---- Some graphs to understand how to set the threshold T3 [To be deleted?]
pop <- ind %>% 
  filter(IndicatorCode == 'SP.POP.TOTL') %>%
  select(CountryCode,Year,Value)

x11()
plot_pop <- ggplot() +
  geom_line(data = pop, aes(x = Year, y = Value, colour = CountryCode), show.legend = FALSE)
# It doesn't work.... I try again tomorrow 


prova <- ind %>%
  filter(IndicatorCode=='SP.POP.TOTL') %>%
  group_by(CountryCode) %>%
  summarise(Mean = mean(Value), Min = min(Value))

dim(prova)
head(prova)
prova

