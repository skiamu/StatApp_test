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
T2 <- 0.30
fil_ind_05 <- Filter_Countries_With_Too_Few_Ind_WRT_Mean(fil_ind_04,T2)
# 06 ------- Filter the countries with too few population ####

T3 <- 1e06
fil_ind_06 <- Filter_Countries_With_Too_Low_Pop(fil_ind_05,T3)

# 06.01 ---- Some graphs to understand how to set the threshold T3 [To be deleted?] ####
# REM : this code is just useful to decide the threshold for the population
pop <- fil_ind_04 %>%
  filter(IndicatorCode == 'SP.POP.TOTL') %>%
  filter(Value < 1e06) %>%
  group_by(CountryCode) %>%
  summarise(Mean = mean(Value)) 

x11()
ggplot(pop, aes(x = reorder(CountryCode, +Mean), y = Mean, label=CountryCode)) +
  geom_point() + 
  geom_text(hjust = 0, nudge_x = 0.05,check_overlap = TRUE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ggtitle('Total population (mean over years)')


TP <- 1e06 #threshold below which countries are filtred out
cnt_out <- pop %>%
  filter(Mean < TP) %>%
  select(CountryCode)

cnt_name_out <- country %>%
  filter(country$CountryCode %in% cnt_out$CountryCode) %>%
  select(ShortName)

cnt_name_out


# What happens if we invert the filters in 05 and 06
# My idea is that if we use the pop filter then the filter with the
# fraction of the mean can be avoided

fil_ind_56.1 <- fil_ind_04 %>%
  Filter_Countries_With_Too_Few_Ind_WRT_Mean(T2)

fil_ind_56 <- fil_ind_56.1 %>%
  Filter_Countries_With_Too_Low_Pop(T3)

fil_ind_65.1 <- fil_ind_04 %>%
  Filter_Countries_With_Too_Low_Pop(T3)

fil_ind_65 <- fil_ind_65.1 %>%
  Filter_Countries_With_Too_Few_Ind_WRT_Mean(T2)

dim(fil_ind_56.1)
dim(fil_ind_56)
dim(fil_ind_65.1)
dim(fil_ind_65)


