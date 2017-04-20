# Refine the dataset: on Years, Nations, Indicators;
#                     Nations hist per Topics, Supertopics;
#                     Indicators hist per Topics, Supertopics;
# 10-04-17
# Mowy

# 01 ------- Set working directory (set yours) and import libraries (check you installed them) #####
setwd('/Users/mowythebest/Desktop/Ingegneria matematica/5.2 Applied Statistics/world-development-indicators')
library(dplyr)    # %>%
library(reshape2) # dcast
library(ggplot2)  # ggplot
library(data.table) # setnames

# 02 ------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')

# 03 ------- Filter out the aggregate countries ####
# The aggregate countries have the variable 'Region' blank
# SC = Single Countries
SC <- country %>%
  filter(Region != '') %>%
  select(c(CountryCode))
# AG = Aggregate Countries
AC <- country %>%
  filter(Region == '') %>%
  select(c(CountryCode))

# RD = Refined Dataset
RD <- ind %>%
  filter(ind$CountryCode %in% SC$CountryCode)

# 04 ------- Filter the years with too few indicators ####
# YI = Yearly Indicators
YI <- RD %>%
  group_by(Year,CountryCode) %>% 
  summarise(NumIND = length(IndicatorCode))
# Compute the mean of num of ind among countries for all years
# MI = Mean Indicators
MI <- YI %>%
  group_by(Year) %>%
  summarise(Mean = mean(NumIND))

# Fix a threshold for the minimum mean number of indicators that we accept every year
T1 <- 200

# Plot                                         ***FIND NICER COLOURS***
#  scale_colour_manual(values = rep(brewer.pal(5,"Set1")), times=1) +
Plot_Indicators_w_Mean <- ggplot() +
  geom_line(data = YI, aes(x = Year, y = NumIND, colour = CountryCode), show.legend = FALSE) +
  geom_line(data = MI,    aes(x = Year, y = Mean), 
            colour = '#FF9999', size = 3, linejoin = "round", lineend = "round", show.legend = FALSE) +
  geom_line(data = MI,    aes(x = Year, y = T1), 
            colour = 'grey20', size = 1, linejoin = "round", lineend = "round", show.legend = FALSE) + 
  ggtitle('Number of indicators for each state over the years',
          sub  = 'Mean among countries (orange) - Threshold T1 (Dark Grey)')

# YO = Year Ok
YO <- MI %>%
  filter(Mean > T1) %>%
  select(Year)
# YN = Year NOT Ok
YN <- MI %>%
  filter(Mean <= T1) %>%
  select(Year)

#Refined Dataset
RD <- RD %>%
  filter(RD$Year %in% YO$Year)

# 05 ------- Filter the countries with too few indicators with respect to the mean ####
# the countries accepted will be only the ones that for every year have a number of indicators 
# that is more than 10% of the mean [may change this threshold]
T2 <- 0.12

YI1 <- RD %>%
  group_by(Year,CountryCode) %>% 
  summarise(NumIND = length(IndicatorCode))

MI1 <- YI1 %>%
  group_by(Year) %>%
  summarise(Mean = mean(NumIND))

YMI <- YI1 %>%
  merge(MI1, by='Year')

# Plot
Plot_Indicators_w_Mean_w_T2 <- ggplot() +
  geom_line(data = YI1, aes(x = Year, y = NumIND, colour = CountryCode), show.legend = FALSE) + 
  geom_line(data = MI1,    aes(x = Year, y = Mean), 
            colour = '#FF9999', size = 3, linejoin = "round", lineend = "round", show.legend = FALSE) +
  geom_line(data = MI1,    aes(x = Year, y = T2*Mean), 
            colour = 'grey20', size = 1, linejoin = "round", lineend = "round", show.legend = FALSE) + 
  ggtitle('Number of indicators for each state over the years',
          sub  = 'Mean among countries (orange) - Threshold T2 (Dark Grey)')

#Country that pass the T2 threshold
C2  <- YMI %>%
  group_by(CountryCode) %>%
  summarise(Cond = all(NumIND > T2 * Mean)) %>%
  filter(Cond) %>%
  select(CountryCode)
#Country that DON'T pass the T2 threshold
CN2  <- YMI %>%
  group_by(CountryCode) %>%
  summarise(Cond = any(NumIND <= T2 * Mean)) %>%
  filter(Cond) %>%
  select(CountryCode)
# I want to see which countries are filtered out, so that I can fix T2
cn2 <- country %>%
  filter(country$CountryCode %in% CN2$CountryCode) %>%
  select(ShortName)
cn2

#Refined Dataset
RD <- RD %>%
  filter(RD$CountryCode %in% C2$CountryCode)

# 06 ------- Plot and results ####
x11()
Plot_Indicators_w_Mean
x11() 
Plot_Indicators_w_Mean_w_T2

# In 03                  the countries filtered out are 33 (see AC)
# In 04 if T1 = 200 then the years     filtered out are 11 (see YN)  [out: 1960 to 1969 and 2015] 
# In 05 if T2 = 15% then the countries filtered out are 40 (see CN2) [out: Uzbekistan, Turkmenistan, Slovenia, Lithuania, Latvia, Kyrgyz Republic, Kazakhstan, Eritrea, Croatia, Azerbaijan, Armenia]
#       if T2 = 10% then the countries filtered out are 18 (see CN2) [in : Andorra, San Marino, ecc]
# T2, for now, is problematic

# 07 ------- How many indicators are common between all the selected countries? ####
RD2010 <- RD %>% filter(Year == 2010) %>% select(-Year) %>% dcast(CountryCode ~ IndicatorCode); RD2010 <- RD2010[,colSums(is.na(RD2010))==0]
RD2000 <- RD %>% filter(Year == 2000) %>% select(-Year) %>% dcast(CountryCode ~ IndicatorCode); RD2000 <- RD2000[,colSums(is.na(RD2000))==0]
RD1990 <- RD %>% filter(Year == 1990) %>% select(-Year) %>% dcast(CountryCode ~ IndicatorCode); RD1990 <- RD1990[,colSums(is.na(RD1990))==0]
RD1980 <- RD %>% filter(Year == 1980) %>% select(-Year) %>% dcast(CountryCode ~ IndicatorCode); RD1980 <- RD1980[,colSums(is.na(RD1980))==0]
RD1970 <- RD %>% filter(Year == 1970) %>% select(-Year) %>% dcast(CountryCode ~ IndicatorCode); RD1970 <- RD1970[,colSums(is.na(RD1970))==0]

yy <- c(2010,2000,1990,1980,1970)
vv <- c(dim(RD2010)[2], dim(RD2000)[2], dim(RD1990)[2], dim(RD1980)[2], dim(RD1970)[2]) - 1
dd <- data.frame(Year=yy,NumCommIND=vv)
dd

# Which are them?
colnames(RD1970)

# Which indicators do we prefer?
# 08 ------- How many nations per every indicator ####

# Change the name to obtain the merge
setnames(ser, "SeriesCode", "IndicatorCode")
# Merge the indicators with the topic
# IT = Indicator Top 
IT <- merge(RD[,c('CountryCode','CountryName','IndicatorCode','Year','Value')],
            ser[,c('IndicatorCode','Topic')],by='IndicatorCode') # Take some seconds
IT$SuperTopics<- c(sapply(IT[,6], substring, 1, 8))
# NI = Nation Indicators
NI <- IT %>%
  group_by(Year,IndicatorCode) %>% 
  summarise(NumNAT = length(CountryCode))

# Compute the mean of num of nat among ind for all years
# MN = Mean Nations
MN <- NI %>%
  group_by(Year) %>%
  summarise(Mean = mean(NumNAT))

# Plot                                         ***FIND NICER COLOURS***
Plot_Nations_w_Mean <- ggplot() +
  geom_line(data = NI, aes(x = Year, y = NumNAT, colour = IndicatorCode), show.legend = FALSE) +
  geom_line(data = MN,    aes(x = Year, y = Mean), 
            colour = '#FF9999', size = 3, linejoin = "round", lineend = "round", show.legend = FALSE) +
  ggtitle('Number of nations for each indicator over the years',
          sub  = 'Mean among indicators (orange)')

x11()
Plot_Nations_w_Mean
# 09 ------- How many nations per topic along all the years and counting the repetitions? ####
topics <- unique(IT[,'Topic'])
# NT = Nations per Topic
NT <- IT %>%
  group_by(Topic) %>% 
  summarise(numNat = length(CountryCode))
# Computation of the mean of numNat
meanNat = colMeans(NT[,2])

# Plot the histogram
x11()
ggplot(NT, aes(x = Topic, y = numNat)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=meanNat, na.rm = FALSE, show.legend = NA)

# 10 ------- Filter the topics with too few nations with respec to the mean ####
# TMN = Treshold meanNat, to filter the topics
TMN = .2
tMeanNat = meanNat * TMN

# Topics that pass the TMN threshold
NT2  <- NT %>%
  group_by(Topic) %>%
  summarise(Cond = (numNat > tMeanNat)) %>%
  filter(Cond) %>%
  select(Topic)
# Topics that DON'T pass the T2 threshold
NTN2  <- NT %>%
  group_by(Topic) %>%
  summarise(Cond = (numNat <= tMeanNat)) %>%
  filter(Cond) %>%
  select(Topic)
NTN2

# Plot the histogram
x11()
ggplot(NT, aes(x = Topic, y = numNat)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=meanNat, na.rm = FALSE, show.legend = NA) +
  geom_hline(yintercept=tMeanNat, na.rm = FALSE, show.legend = NA)
# Results:
# with treshold 0.3:    19 topics are excluded
# with treshold 0.3:    11 topics are excluded
# with treshold 0.1:    5 topics are excluded
# 11 ------- How many nations per supertopic along all the years and counting the repetitions? ####
# Definition of the supetopics
supertopics <- unique(sapply(NT[1], substring, 1, 8))

# NST = Nation per SuperTopic
NST <- IT %>%
  group_by(SuperTopics) %>% 
  summarise(numNatST = length(CountryCode))
# Computation of the mean of numNat
meanNatST = colMeans(NST[,2])

# Plot the histogram
x11()
ggplot(NST, aes(x = SuperTopics, y = numNatST)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept=meanNatST, na.rm = FALSE, show.legend = NA)

# 12 ------- How many indicators per topic or supertopic? ####
# IPT = Indicators per Topic
IPT <- IT %>%
  group_by(Topic) %>% 
  summarise(numInd = length(unique(IndicatorCode)))

# Plot the histogram
x11()
ggplot(IPT, aes(x = Topic, y = numInd)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# IPT = Indicators per Super Topic
IPST <- IT %>%
  group_by(SuperTopics) %>% 
  summarise(numInd = length(unique(IndicatorCode)))

# Plot the histogram
x11()
ggplot(IPST, aes(x = SuperTopics, y = numInd)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
