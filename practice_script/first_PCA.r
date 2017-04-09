# In this script we extract all the indexes within a specific
# topic for countries in a specific region.
#  We select all the indicators within the topic "Public Sector: Government 
# finance: Expense". This topic has been looked up in the dataframe
# "Series" --> "Topic"

# The aim of this "example" script is to estiblish some actions on
# the dataframe in a particular case that can be easily extended
# to other dataframe's parts and analysis

# needed library, first download them
library(reshape2)
library(dplyr)
library(ggplot2)
source("read_data.r")


attach(Series)
attach(Indicators)

# read the dataframe from the .csv files


########### dataframe manipulation ##############

# select an index topic of interest
specified_topic <- "Public Sector: Government finance: Expense"
# select the region of interest
specified_region <- "Europe & Central Asia"
# select the "IndicatorsCodes" that corresponds to the Topic specified
# from the dataframe "Series"
index.code <-
  Series %>% 
  filter(Topic == specified_topic) %>%
  select(SeriesCode) 

# get "CountryCode" for countries in the region of interest
europe.county.code <- Country %>%
  filter(Region == specified_region) %>%
  select(CountryCode)

# get the indicator name to better understand what they are about.
# it might be useful throughout the analysis
index.name <-
  Series %>%
  filter(SeriesCode %in% index.code$SeriesCode) %>%
  select(IndicatorName,SeriesCode) 

# convert the names from factor to string(per sport) 
index.name[,"IndicatorName"] <- sapply(index.name[, "IndicatorName"], as.character)

# select rows from the main dataframe "Indicators" whose indexes are in the
# specified topics:
# each "IndicatorCode" in dataframe "Indicators" has unique correspondent in
# "SeriesCode" in dataframe "Series". This is the link between the two.
temp <- filter(Indicators,IndicatorCode %in% index.code$SeriesCode,
               CountryCode %in% europe.county.code$CountryCode)

# summarize the shrinked datafarme with number of indexes and number
# of countries
summary <- summarize(temp, n_index = n_distinct(IndicatorCode),n_country = 
            n_distinct(CountryCode))
summary

# we select the year 2010 for our first analysis
temp <- temp %>% filter(Year == 2010)

# CountryCodes of (almost) all the aggregate country
aggregate.country <- c("ARB","CSS","CAF","CEB","EAS","EAP","ECS","ECA",
                       "EUU","FCS","HPC","HIC","NOC","OEC","LCN","LAC",
                       "LDC","LMY","LIC","LMC","MEA","MNA","MIC","NAC",
                       "MNP","OED","OSS","PSS","SSF","SSA","TCA","ARE",
                       "UMC","WLD","EMU","SAS","SST")

# remove the aggregate countries
temp <- temp %>% filter(!CountryCode %in% aggregate.country)

# summarize again the temporary dataframe
summary <- summarize(temp, n_index = n_distinct(IndicatorCode),n_country = 
                       n_distinct(CountryCode))
summary


# how many countries does each indicator have? (within the indicator 
# specidied)
how.many.index <- temp %>% 
  group_by(IndicatorCode) %>%
  summarise(n())


# how many indicators does each country have?
# from this table we could decide to drop countries that don't have
# enough indexes
how.many.index <- temp %>% 
  group_by(CountryCode) %>% 
  summarise(n())


# final dataframe: we transform observations in variables.
# formula = ID ~ item
# the left-hand side it's gonna be the unique ID,
# the right-hand side is the variable whose values will become
# variables.
d <- dcast(temp,formula = CountryCode ~ IndicatorCode,value.var = "Value")

# rename each row with its country name
row.names(d) <- d$CountryCode

# delete the column "CountryCode"
d <- select(d,-(CountryCode))


########## DATA ANALYSIS  ##########

# we have now the dataset in a suitable form for data analysis.

# I decide to analyse just indexes that are % of public expense
expense.index <- c("GC.XPN.COMP.ZS","GC.XPN.GSRV.ZS","GC.XPN.INTP.ZS",
                   "GC.XPN.OTHR.ZS","GC.XPN.TRFT.ZS")

# tutti i dati sono in percentuale
d <- d %>% select(one_of(expense.index))

# check if all the rows sum up to 100
check <- apply(d,1,sum)

# just some basic plot
x11()
plot(d)
x11()
boxplot(d) # many outliers

# PCA analysis (row data, not standardized)
pc.d <- princomp(na.omit(d),scores = T)
pc.d
summary(pc.d)

# extract the loading 
load.d <- pc.d$loadings





