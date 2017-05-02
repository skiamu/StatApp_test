# Filter_Dataset
# a try to filter the dataset with some tests 
# 12-04
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
# user-defined funtions
source('filtering_functions.R')
source('HowManyNA_functions.R')
# 02 ------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')
# 03 ------- Filter aggregate countries###### 
# The aggregate countries have the variable 'Region' blank
fil_ind_03 <- FIlter_Countries_Region(ind,country,'')
# 04 ------- Filter the years with too few indicators ####
# How do I set the threshold T1? see the 'set_T1.R' script [to be done, from the previous works]
T1 <- 200
fil_ind_04 <- Filter_Years_With_Too_Low_Mean(fil_ind_03,T1)
# 05 ------- Filter the indicators that are too recent ####
TY_start <- 1970
fil_ind_05 <- Filter_Indicators_Too_Recent(fil_ind_04,TY_start)
# 06 ------- Filter the indicators that ended before the last year ####
TY_end   <- 2014 # maybe better 2013?
fil_ind_06 <- Filter_Indicators_That_Ended(fil_ind_05,TY_end)


# TEST ####

# How the dataset impoved over the different steps?
v1 <- c(HowManyNA(ind,Flag_Plot = FALSE),
        HowManyNA(fil_ind_03,Flag_Plot = FALSE),
        HowManyNA(fil_ind_04,Flag_Plot = FALSE),
        HowManyNA(fil_ind_05,Flag_Plot = FALSE),
        HowManyNA(fil_ind_06,Flag_Plot = FALSE))
v2.1 <- c(DimInd(ind)[1], DimInd(fil_ind_03)[1], DimInd(fil_ind_04)[1], DimInd(fil_ind_05)[1], DimInd(fil_ind_06)[1])
v2.2 <- c(DimInd(ind)[2], DimInd(fil_ind_03)[2], DimInd(fil_ind_04)[2], DimInd(fil_ind_05)[2], DimInd(fil_ind_06)[2])
v2.3 <- c(DimInd(ind)[3], DimInd(fil_ind_03)[3], DimInd(fil_ind_04)[3], DimInd(fil_ind_05)[3], DimInd(fil_ind_06)[3])
v3 <- v2.1*v2.2*v2.3
df <- data.frame(NA_perc  = v1,
                 Volume3D = v3,
                 NumIND   = v2.1,
                 NumCNT   = v2.2,
                 NumYear  = v2.3)
df
# which countries are left?
vv <- fil_ind_06 %>% select(CountryName) %>% unique()

# a problematic country
HowManyNA_cnt(fil_ind_06,'Turkmenistan')
# a ?? country 
HowManyNA_cnt(fil_ind_06,'Poland')
# an ok country
HowManyNA_cnt(fil_ind_06,'Germany')

mean_NA_perc <- vector(mode="numeric", length=dim(vv)[1])
fil_ind_temp <- fil_ind_06 %>% filter(Year > 1990 & Year < 2014)
for(i in 1:length(mean_NA_perc)){
  df1 <- HowManyNA_cnt(fil_ind_temp,vv[i,'CountryName'],Flag_Plot = FALSE)
  mean_NA_perc[i] <- mean(df1$NA_perc)
}
df2 <- data.frame(CountryName = vv,
                  MeanNAPerc  = mean_NA_perc)
df2

View(fil_ind_temp)
# Plot trying to show the worse countries
df3 <- df2 %>% filter(MeanNAPerc > 40) 
x11()
ggplot(df3, aes(x = reorder(CountryName, +MeanNAPerc), y = MeanNAPerc, label=CountryName)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Mean percentage of NA value for each state') +
  geom_hline(yintercept = 70) + 
  labs(x = '', y = 'Mean percentage of NA values')

# this function could be done much better
T_NA <- 50
fil_ind_07 <- Filter_Countries_With_Too_Many_NA(fil_ind_06,T_NA)

Y <- 2013
n_ind <- fil_ind_06 %>%
  filter(Year == Y) %>% 
  select(IndicatorCode) %>%
  n_distinct()
temp <- fil_ind_06 %>%
  filter(Year == Y) %>%
  group_by(CountryName) %>%
  summarise(NA_perc = 100*(1-n_distinct(IndicatorCode)/n_ind))
temp_plot <- temp %>% filter(NA_perc > 60)
x11(); ggplot(temp_plot, aes(x = reorder(CountryName, +NA_perc), y = NA_perc, label=CountryName)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle(paste('Mean percentage of NA value for each state in',Y)) +
  labs(x = '', y = 'Mean percentage of NA values')
# Canada would be excluded


# select the indicators that are present for each countries
# number of distinct countries in the dataset
n_totCNT <- fil_ind_06 %>%
  select(CountryName) %>%
  n_distinct()

temp <- fil_ind_06 %>%
  group_by(IndicatorCode) %>%
  summarise(CNT_perc = n_distinct(CountryName)/n_totCNT) %>%
  filter(CNT_perc >= .95) %>%
  select(IndicatorCode)
dim(temp)
temp
