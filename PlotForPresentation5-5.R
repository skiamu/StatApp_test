# Some plots for the 5 may presentation
# last update: 26-04
# Leo

# Looking at 'Indicators' as a 3d matrix lead to some problems:
#   1) YEARS      : not homogeneous                                 [plot0001.png]
#   2) COUNTRIES  : small countries                                 [plot0002.png]
#   3) INDICATORS : too many, hard to choose the interesting ones   [histogram mowy?]

# Our first approach:
#   1) fix an year (the 'best' one, 2010)
#   2) shrink the countries with too many missing values [the plot where we cut after lybia...]
#   3) order the indicators from the ones with th fewest missing countries to the others. select the first N [already done...]
#   4) select the most significative manually
#   5) evaluate the fullness of the shrinked matrix 
#   6) consider to shrink problematic indicators and countries still in the matrix
#   7) fill the (few) missing data using interpolation, value at the previous year or picking the value from its source
#   8) [optional] expand over the years

# 00 - preliminars ----
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")
#setwd("/Users/mowythebest/Desktop/Ingegneria_matematica/5.2_Applied_Statistics/StatApp_test")
library(dplyr)      # %>%
library(reshape2)   # dcast
library(ggplot2)    # ggplot
library(data.table) # setnames
library("ggthemes") # theme_economist() https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html
library(extrafont)  # for Officiana font
library(ggrepel)    # geom_text_repel
library(stringr)    # str_trim
library(scales)     # rescale_none
# user-defined funtions
source('filtering_functions.R')
source('HowManyNA_functions.R')
source("read_data.R") # some problems with dplyr and plyr
#read_data('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc/Playground')
#read_data('/Users/mowythebest/Desktop/Ingegneria_matematica/5.2_Applied_Statistics/StatApp_test')
load('data.RData')

# 01 - Number of indicators per Year ----
detach(package:plyr)    # otherwise the group_by gives problems
library(dplyr)
# number of indicators per year
temp <- Indicators %>% group_by(Year) %>% summarise(numIND = length(IndicatorCode))

# I want to change the font from the std one to 'Verdana'
# why 'Verdana'? because is similar to 'ITCOfficinaSans LT Book' that is the one of the economist
# https://www.rdocumentation.org/packages/ggthemes/versions/3.4.0/topics/theme_economist
# however I tried several things to make it work and now (since I successufully imported) 
# it's not really easy to find the noly necessary steps to do in order to make it work
# so I will put some instructions but I am not sure it will work. But I should be able to help you,
# and in the end it's only for the plot so it is not a huge problem...

#install.packages("extrafont")
#library(extrafont)
#font_import('Verdana')                                                             # doesn't  work
#font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = 'Verdana.ttf') # doesn't  work
#font_import()                                                                      # import ALL fonts, takes a LOT
windowsFonts(Verdana=windowsFont("Verdana"))                                        # don't know why it works but it does...

IndPerYear <- ggplot(temp, aes(x=Year, y=numIND)) +
  theme_economist_white(base_size = 11, base_family = "Verdana",
                        gray_bg = TRUE, horizontal = TRUE) +
  geom_line(color='steelblue', size=2) +
  scale_x_continuous("", breaks = c(1960, seq(1960, 2015, 10), 2015)) +
  geom_vline(xintercept = 2010,linetype = 2) +
  labs(x = 'year', y = 'number of observations',
       title = 'Number of observations for each year',
       subtitle = 'Increasing in the years, dropping in the very last due to the difficulty to find recent data',
       caption = 'For each year, an indicator is counted multiple times if present for multiple countries')
# plot
x11(); IndPerYear # The dimensions should be nice in the image saved, not in this plot
# save the image
ggsave("plot0001.png",IndPerYear, width=10, height=5)

# Want to save in .pdf?
# ggsave("plot0001.pdf",IndPerYear, width=10, height=5, device=cairo_pdf)
# why cairo_pdf? http://stackoverflow.com/questions/27542302/ggplot-embedded-fonts-in-pdf

# Add a curved arrow?
# +   geom_curve(aes(x=1985, xend=2009, y=175000, yend=187000), arrow=arrow(length = unit(0.5, "cm")), 
#                size=1.5, color='Red', curvature=-0.1)
# use geom_segment for a straight arrow

# Add a text in the plot?
# +   annotate("text", x=1981, y=174500, label="2010", size=7)

# 02 - Number of indicators for some countries over years ----
# Normal Country      : USA
# Small Country       : San Marino
# Problematic Country : Afghanistan

temp <- Indicators %>% 
  filter(CountryName %in% c('Afghanistan','San Marino','USA')) %>% 
  group_by(CountryName,Year) %>% summarise(numIND = length(IndicatorCode))
#View(temp)
numDistInd <- Indicators %>% select(IndicatorCode) %>% n_distinct() #unique() %>% length() #dim() %>% [1]

IndCNT <- ggplot(temp, aes(x=Year, y=numIND, colour=CountryName, group=CountryName)) +
  theme_economist_white(base_size = 11, base_family = "Verdana",
                        gray_bg = TRUE, horizontal = TRUE) + 
  geom_line(size=1) +
  scale_x_continuous("", breaks = c(1960, seq(1960, 2015, 10), 2015)) +
  geom_segment(aes(x=1960, xend=2015, y=numDistInd, yend=numDistInd), size=1, linetype=2, colour='Black') +
  annotate("text", x=1973, y=numDistInd-50, label="number of distinct indicators (1344)", size=4) +
  labs(x = 'year', y = 'number of indicators', colour = 'Country',
       title = 'Number of indicators for some significative countries',
       subtitle = 'USA are normal, San Marino is small, Afghanistan is problematic')
# plot
x11(); IndCNT
# save the image
ggsave("plot0002.png",IndCNT, width=10, height=5)


# 03 - Number of indicators per each supertopic ----
temp <- Series %>% select(Topic,IndicatorName) 
View(temp)
temp$SuperTopic <- gsub("\\:.*$", "",temp$Topic)
temp <- temp %>% 
  group_by(SuperTopic) %>%
  summarise(numInd = length(unique(IndicatorName)))
head(temp)

histSuper <- ggplot(temp, aes(x = SuperTopic, y = numInd)) +
  theme_economist(base_size = 11, base_family = "Verdana",
                  horizontal = TRUE) +
  geom_bar(stat = "identity", fill='steelblue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = '', y = 'number of indicators',
       title = 'Number of indicators for each supertopic',
       subtitle = 'Prevalence of economic indicators: our field of analysis',
       caption = 'The supertopic is not present in the raw dataset but comes naturally from the topic. \n (e.g. topic = Infrastructure: Transportation, supertopic = Infrastructure)')
x11(); histSuper # The dimensions should be nice in the image saved, not in this plot
# save the image
ggsave("plot0003.png",histSuper, width=10, height=5)

# same image, add two arrows
histSuperArrow <- histSuper + 
  annotate("text", x=2.4, y=455, label="GDP", size=7) +
  geom_curve(aes(x=2, xend=1, y=450, yend=400), arrow=arrow(length = unit(0.3, "cm")), 
             size=1, color='Black', curvature=0.1) +
  annotate("text", x=5.65, y=255, label='threatened bird species', size=7) +
  geom_curve(aes(x=4, xend=3, y=250, yend=100), arrow=arrow(length = unit(0.3, "cm")), 
             size=1, color='Black', curvature=0.1)
x11(); histSuperArrow # The dimensions should be nice in the image saved, not in this plot
# save the image
ggsave("plot0004.png",histSuperArrow, width=10, height=5)

# 04 - Fix the best year: 2010 ----
FixedYear <- 2010 
df04 <- Indicators %>% filter(Year == FixedYear)        # only the obs of FixedYear

# 05 - Number of missing indicators per countries ----
df05 <- df04 %>% FIlter_Countries_Region(Country,'')    # out the aggregate conutries

cnt05 <- df05 %>% 
  group_by(CountryName) %>% 
  summarise(numInd = n_distinct(IndicatorCode))
dim(cnt05)



cntInPlot <- c('Puerto Rico','North Korea','San Marino',
               'Bermuda','Libya','Saudi Arabia','USA','Afghanistan','Somalia','St. Kitts and Nevis')
Tind <- 400 # threshold for indicators

p_cnt05 <- ggplot(cnt05, aes(x = reorder(CountryName, +numInd), y = numInd)) + 
  geom_point(colour='steelblue') + 
  theme_economist_white(base_size = 11, base_family = "Verdana",
                        gray_bg = TRUE, horizontal = TRUE) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_hline(yintercept=Tind,colour='Grey80',linetype=2,size=1) +
  geom_vline(xintercept=27.5,colour='Grey20',linetype=2,size=1) +
  annotate("text", x=200, y=370, label="400", size=4) + 
  annotate("text", x=37, y=910, label="IN",  size=9, colour='Dark Green') + 
  annotate("text", x=12, y=910, label="OUT", size=9, colour='Red') + 
  geom_text_repel(aes(label=ifelse(CountryName %in% cntInPlot,as.character(CountryName),''),
                      colour=factor(numInd>Tind))) + 
  scale_colour_manual(values=c("Red", "Dark Green")) +
  labs(x = 'countries', y = 'number of indicators',
       title = 'Which countries have too few indicators?',
       subtitle = 'Threshold set at 400 indicators, in red the countries out, in green the countries in',
       caption = 'The threshold was set in order not to exclude the first significative country: Libya') + 
  guides(colour=FALSE)

# countries in
cntIn05 <- cnt05 %>% filter(numInd>Tind)

# plot
x11(); p_cnt05
# save the image
ggsave("plot0005.png",p_cnt05, width=10, height=5)

# 06 - Fullest indicators ----
# shrink the countries belowthe threshold
df06 <- df05 %>% filter(df05$CountryName %in% cntIn05$CountryName)
numDistCnt06 <- dim(cntIn05)[1]
# how many indicators I want to show
M <- 30
# compute the unber of countries per indiactor
ind06 <- df06 %>% group_by(IndicatorName) %>% summarise(numCnt = n_distinct(CountryCode))
# drop the unit of measure ( after the '(' ) and, if present the last whitespace
ind06$IndicatorName <- gsub("\\(.*$", "",ind06$IndicatorName) %>% str_trim("right") 
# REM some indicatros differs only for the part in the parentesis, so now they are equal
# to avoid confusion I will drop every indicator which have a duplicate
# sounds brutal but it is only for the plot!
ind06 <- ind06[!duplicated(ind06[,'IndicatorName']),]
# select the first M
ind06 <- arrange(ind06,desc(numCnt))[1:M,] 

pos <- integer(M); pos[c(3,6)] <- c(1,1)
p_ind06 <- ggplot(ind06, aes(x = reorder(IndicatorName, +numCnt), y=numCnt))+ 
  geom_hline(yintercept=numDistCnt06) + 
  geom_bar(position="dodge", stat='identity', fill='steelblue') + 
  theme_economist(base_size = 11, base_family = "Verdana",horizontal = FALSE) +
  scale_y_continuous(limits=c(183,187),oob=rescale_none) +
  labs(x = 'indicators', y = 'number of countries',
       title = 'The fullest indicators',
       subtitle = 'Look at the indicators manually and select the most significative',
       caption = 'At this point the countries remaining are 187') + 
  coord_flip() 
# tried to put it on the right, but I didn't manage
# plot
x11(); p_ind06
# save the image
ggsave("plot0006.png",p_ind06, width=10, height=5)

# 07 - Visualize manual selection ----
# manual selection
pos <- integer(M); pos[c(4,6,20,25,29)] <- c(1,1,1,1,1)

p_ind06WithSelInd <- p_ind06 +
  geom_bar(position="dodge", stat='identity', fill=ifelse(pos,'Yellow','steelblue'))
# plot
x11(); p_ind06WithSelInd
# save the image
ggsave("plot0007.png",p_ind06WithSelInd, width=10, height=5)
