# A try to perform some filtering to PCA
# last update: 20-04
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
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# 
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# 02 ------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')
# TEST 0 ####
# 00. ----
# I fix a year (to make things simple for the beginning)
FixedYear <- 2013                                           # it seemed the fullest
df0_0 <- FIlter_Countries_Region(ind,country,'')            # out the aggregate conutries
df0   <- df0_0 %>% filter(Year == FixedYear)                # only the obs of FIxedYear
n_cnt0 <- df0 %>% select(CountryCode) %>% n_distinct()      # number of countries
n_ind0 <- df0 %>% select(IndicatorCode)   %>% n_distinct()  # number of indicators
tot_na_perc0 <- 100*(1-length(df0$Value)/(n_cnt0*n_ind0))   # total percentage of NA values

# 01. ----
# which are the 'the most incomplete' indicators and countries?
# how much are those incomplete?
# compute the percentage of NA for each indicator and each country
ind1 <- df0 %>% group_by(IndicatorName) %>% summarise(na_perc = 100*(1-n_distinct(CountryCode)  /n_cnt0))
cnt1 <- df0 %>% group_by(CountryName)   %>% summarise(na_perc = 100*(1-n_distinct(IndicatorCode)/n_ind0))
# plot in increasing order to see if there are interesting behaviour
p_ind1 <- ggplot(ind1, aes(x = reorder(IndicatorName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x =element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = 'Indicators', y = 'Percentage of NA values')
p_cnt1 <- ggplot(cnt1, aes(x = reorder(CountryName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Countries', y = 'Percentage of NA values')
x11(); p_ind1
x11(); p_cnt1

# 02. ----
# have a closer look to the worst countries
cnt2 <- cnt1 %>% filter(na_perc>50)
p_cnt2 <- ggplot(cnt2, aes(x = reorder(CountryName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Countries', y = 'Percentage of NA values') +
  geom_hline(yintercept = 70)
x11(); p_cnt2

# 03. ----
# DECISION: filter all the countries after Lybia (na_perc <~70)
cnt3 <- cnt1 %>% filter(na_perc<70)
df3 <- df0 %>% filter(df1$CountryName %in% cnt3$CountryName)
n_cnt3 <- df3 %>% select(CountryCode)     %>% n_distinct()  # number of countries
n_ind3 <- df3 %>% select(IndicatorCode)   %>% n_distinct()  # number of indicators
tot_na_perc3 <- 100*(1-length(df1$Value)/(n_cnt3*n_ind3))   # total percentage of NA values

# 04. ----
# does the na_perc for indicators changed?
ind4 <- df3 %>% group_by(IndicatorName) %>% summarise(na_perc = 100*(1-n_distinct(CountryCode)  /n_cnt3))
cnt4 <- df3 %>% group_by(CountryName)   %>% summarise(na_perc = 100*(1-n_distinct(IndicatorCode)/n_ind3))
p_ind4 <- ggplot(ind4, aes(x = reorder(IndicatorName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = 'Indicators', y = 'Percentage of NA values')
p_cnt4 <- ggplot(cnt4, aes(x = reorder(CountryName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Countries', y = 'Percentage of NA values') 

x11(); multiplot(p_ind1 + ggtitle('OLD') + geom_hline(yintercept = 50),
                 p_ind4 + ggtitle('NEW') + geom_hline(yintercept = 50),cols=2)
x11(); multiplot(p_cnt1 + ggtitle('OLD') + coord_cartesian(ylim = c(0, 100)),
                 p_cnt4 + ggtitle('NEW') + coord_cartesian(ylim = c(0, 100)),cols=2)

# 05. ----
# filter out the indicators with more than 50 % of na_perc
ind5 <- ind4 %>% filter(na_perc < 50)
p_ind5 <- ggplot(ind5, aes(x = reorder(IndicatorName, +na_perc), y = na_perc*n_cnt3/100)) + 
  geom_point() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = 'Indicators', y = 'Number of missing countries')
x11(); p_ind5

# 06. ----
# which indicators are present for at least N countries?
# how many are they?
# REM: may not be the same countries...

N <- 180 # N???[0,n_cnt]
ind6 <- ind5 %>% filter(N < (1-na_perc/100)*n_cnt2)
p_ind6 <- ggplot(ind6, aes(x = reorder(IndicatorName, +na_perc), y = na_perc*n_cnt3/100)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Indicators', y = 'Number of missing countries')
x11(); p_ind6
dim(ind6)

# 07. ----
# find the M 'fullest' indicators
M <- 50
ind7 <- arrange(ind4,desc(-na_perc))[1:M,]
#View(ind7) # you may like to change from na_perc to number of countries but it is immediate
# filter the df using only this selected indicators
df7 <- df3 %>% filter(df3$IndicatorName %in% ind7$IndicatorName)
n_cnt7 <- df7 %>% select(CountryCode)   %>% n_distinct()     # number of countries
n_ind7 <- df7 %>% select(IndicatorCode) %>% n_distinct()     # number of indicators
tot_na_perc7 <- 100*(1-length(df7$Value)/(n_cnt7*n_ind7))    # total percentage of NA values

# 08. ----
# did it improve the situation?
v_cnt <- c(n_cnt0,n_cnt3,n_cnt7)
v_ind <- c(n_ind0,n_ind3,n_ind7)
v_nap <- c(tot_na_perc0,tot_na_perc3,tot_na_perc7)
cbind(v_cnt,v_ind,v_nap)
