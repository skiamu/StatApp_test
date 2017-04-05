# Explore the dataset
# 05-04-17
# Leo

# Adapt the code to your pc (look at the first two blocks)
# I am not very experienced with Git, yet

# It takes a little

#---------- Set working directory (set yours) #####
setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/Playground')

#---------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')

#---------- Look for indicators which are present in every state ###### 
# fix a year (fy), cut the country name and indicator name
ind_fy <- ind[ind$Year==2000,c('CountryCode','IndicatorCode','Value')] # change the year 'manually'
head(ind_fy)
dim(ind_fy)

library(reshape2)

# put the indicators as columns (ic)
ind_fy_ic <- dcast(ind_fy, CountryCode ~ IndicatorCode)
#head(ind_fy_ic) # too many rows
dim(ind_fy_ic)

# drop the indicators (columns) with a NA value
#http://stackoverflow.com/questions/12454487/remove-columns-from-dataframe-where-some-of-values-are-na
ind_fy_ic_noNAc <- ind_fy_ic[,colSums(is.na(ind_fy_ic))==0]
#head(ind_fy_ic_noNAc) # too many rows
dim(ind_fy_ic_noNAc)

# drop the countries (rows) with a NA value
ind_fy_ic_noNAr <- ind_fy_ic[rowSums(is.na(ind_fy_ic))==0,]
#head(ind_fy_ic_noNAr) # too many rows
dim(ind_fy_ic_noNAr)

# Partial results: 
#   1960 ---> no indicators common to every state
#             no state with all the indicators
#   2000 ---> only: SP.POP.GROW, SP.POP.TOTL
#             no state with all the indicators

#---------- Look for states with very few indicators #####
# I want to draw 3 graphs
# 1) total number of indicators (counted mult. times if present for multiple states) for each year [1 line]
# 2) number of indicators for a country (set manually) for each year [1 line]
# 3) number of indicators for each country for each year [247 lines]

library(dplyr)

# 1)
tot_ind <- ind %>%
  group_by(Year) %>% 
  summarise(NumIND = length(IndicatorCode))
tot_ind
# Plot
x11()
plot(tot_ind$Year,tot_ind$NumIND,'l',
     main='1) Tot ind num (counted mult. times if present in mult states)',
     xlab="Year",ylab="Total number of indicators")

# 2)
nat_ind <- ind %>%
  filter(CountryCode=='ITA') %>%  
  group_by(Year) %>% 
  summarise(NumIND = length(IndicatorCode))
nat_ind
# Plot
x11()
plot(tot_ind$Year,tot_ind$NumIND,'l',
     main='2) Ind num for ITA',
     xlab="Year",ylab="Number of indicators")

# REM: summarise tranform tha dataframe in a tibble.... take note 
#      filter dataframe --> dataframe

all_nat_ind <- ind %>%
  group_by(Year,CountryCode) %>% 
  summarise(NumIND = length(IndicatorCode))
head(all_nat_ind)
all_nat_ind
# Plot
#http://stackoverflow.com/questions/7912180/plotting-multiple-lines-from-a-data-frame-in-r
library(ggplot2)
x11()
ggplot(all_nat_ind, aes(x = Year, y = NumIND, colour = CountryCode),
       main='3) Ind num for nations',xlab="Year",ylab="Number of indicators") + geom_line()
# It takes some seconds, expand the screen
# Something better could be done

# try to find some numerical threshold to individuate countries with very few indicators 
# (target: San Marino, Andorra, ecc)
# eliminate them with a criterion

# extend analoggally the analysis to year

# the goal should be to refine the dataset (drop some years and some countries)
# using some numerical criteria


# Moreover:
# find the aggregate states
# find the states like URSS, JUGOSLAVIA that used to exist for a only period
# decide how to treat them

# One possible other way to get the number of indicators
indITA2000 <- ind[ind$Year==2000 & ind$CountryCode=='ITA',]
head(indITA2000)
dim(indITA2000) #first dimension
# Let's check
filter(all_nat_ind, Year==2000 & CountryCode=='ITA') # ITA has 693 indicators

#---------- Try to individuate states with very few indicators ####
# Have a look at the regions
country[,c(1,2,8)]
#
regions <- unique(country[,'Region'])
regions # REM: this field is blank if the Counrty is aggrgate or 'strange'

# Merge the indicators with the country region
ind_reg <- merge(ind[,c('CountryCode','CountryName','IndicatorCode','Year','Value')],
                 country[,c('CountryCode','Region')]) # Take some seconds
head(ind_reg)
dim(ind_reg)
dim(ind)   # ok it includes the one without a region
# You may like not to drop CountryName

# Now for each region I want to do the graph 3)
for (reg in regions){
  print(reg)
  reg_nat_ind <- ind_reg %>%
    filter(Region == reg) %>%
    group_by(Year,CountryName) %>% 
    summarise(NumIND = length(IndicatorCode))
  head(reg_nat_ind)
  #reg_nat_ind
  # Plot
  x11()
  print(ggplot(reg_nat_ind, aes(x = Year, y = NumIND, colour = CountryName),
               main=paste('Area:',reg),xlab="Year",ylab="Number of indicators") 
        + geom_line()
        + ggtitle(paste('Area:',reg)))
}

# I can do several things manually from here. 
# However I'd rather trying to find some numerical threshold in order to drop some states
# The main reason is that if we need to change those threshold then repeating the analysis is immediate

# Through the region we can find the 'aggregate' countries

#---------- First proposal for a threshold ####
# For each state I want to find the maximum number of indicators over years
#graphics.off()
# CountryName o CountryCode
nat_max_num_ind <- ind %>%
  group_by(Year,CountryName) %>% 
  summarise(NumIND = length(IndicatorCode)) %>%
  group_by(CountryName) %>%
  summarise(MaxNumIND = max(NumIND)) %>%
  arrange(MaxNumIND)
head(nat_max_num_ind)

# Plot the maximum number of indicators for each state
x11()
ggplot(nat_max_num_ind, aes(x = reorder(CountryName, +MaxNumIND), y = MaxNumIND, label=CountryName)) +
  geom_point() + geom_text(hjust = 0, nudge_x = 0.05,check_overlap = TRUE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  ggtitle('Maximum number of indicators over years')

# Threshold = 400 ? think about it...


