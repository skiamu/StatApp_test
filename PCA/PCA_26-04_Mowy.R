# A try to perform PCA: add merge and filtering on it
# last update: 26-04
# Mowy

# INPUTS 
# 00 ------- Set working directory (set yours) and import libraries (check you installed them) #####
setwd('/Users/mowythebest/Desktop/Ingegneria matematica/5.2 Applied Statistics/world-development-indicators')
#setwd('C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/Playground')
# Diana
# Andrea      Paste yours
# Victor
library(dplyr)      # %>%
library(reshape2)   # dcast
library(ggplot2)    # ggplot
library(data.table) # setnames
library(reshape)   # melt and cast functions
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
# 01 ------- Import Data###### 
ind <- read.csv('Indicators.csv')
country <- read.csv('Country.csv')
country_notes <- read.csv('CountryNotes.csv')
foot_notes <- read.csv('CountryNotes.csv')
ser <- read.csv('Series.csv')
ser_notes <- read.csv('SeriesNotes.csv')


# FILTERING

# 00.  Fixing a year ----
FixedYear <- 2013                                           # it seemed the fullest
df0_0 <- FIlter_Countries_Region(ind,country,'')            # out the aggregate conutries
df0   <- df0_0 %>% filter(Year == FixedYear)                # only the obs of FIxedYear
n_cnt0 <- df0 %>% select(CountryCode) %>% n_distinct()      # number of countries
n_ind0 <- df0 %>% select(IndicatorCode)   %>% n_distinct()  # number of indicators
tot_na_perc0 <- 100*(1-length(df0$Value)/(n_cnt0*n_ind0))   # total percentage of NA values

# 01. 'The most incomplete' indicators and countries ----
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

# 02. The worst countries ----
cnt2 <- cnt1 %>% filter(na_perc>50)
p_cnt2 <- ggplot(cnt2, aes(x = reorder(CountryName, +na_perc), y = na_perc)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Countries', y = 'Percentage of NA values') +
  geom_hline(yintercept = 70)
x11(); p_cnt2

# 03. Filter all the countries after Lybia (na_perc <~ 70) ----
cnt3 <- cnt1 %>% filter(na_perc<70)
df3 <- df0 %>% filter(df0$CountryName %in% cnt3$CountryName)
n_cnt3 <- df3 %>% select(CountryCode)     %>% n_distinct()  # number of countries
n_ind3 <- df3 %>% select(IndicatorCode)   %>% n_distinct()  # number of indicators
tot_na_perc3 <- 100*(1-length(df3$Value)/(n_cnt3*n_ind3))   # total percentage of NA values

# 04. Na_perc for indicators changes ----
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

# 05. Filter out the indicators with more than 25 % of na_perc ----
ind5 <- ind4 %>% filter(na_perc <= 0)
p_ind5 <- ggplot(ind5, aes(x = reorder(IndicatorName, +na_perc), y = na_perc*n_cnt3/100)) + 
  geom_point() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x = 'Indicators', y = 'Number of missing countries')
x11(); p_ind5
graphics.off()
# 00. Number of indicator survived for every supertopic ----
# The merge is really fast after the filtering (merge needed to have topic and supertopic in the table frame)
serind0 <- merge(ser, ind5, by='IndicatorName')
serind0$SuperTopics <- c(sapply(serind0[,3], substring, 1, 8))
dfind0 <- merge(df3, ind5, by='IndicatorName')
dfind0 <- merge(dfind0, ser[,c('Topic','IndicatorName')])

IPT <- serind0 %>%
  group_by(Topic) %>% 
  summarise(numInd = length(unique(IndicatorName)))

IPT_filtered <- IPT %>%
  filter(numInd >1)
# Plot the histogram
x11()
ggplot(IPT, aes(x = Topic, y = numInd)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

x11()
ggplot(IPT_filtered, aes(x = Topic, y = numInd)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# IPT = Indicators per Super Topic
IPST <- serind0 %>%
  group_by(SuperTopics) %>% 
  summarise(numInd = length(unique(IndicatorName)))

# Plot the histogram
x11()
ggplot(IPST, aes(x = SuperTopics, y = numInd)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# the evident result is that doing a PCA on the topics with more than one indicator would be a great idea!!

# 01. PCA on each topics ----

######################################### I HAVE TO FIND A WAY TO COVER THE NAN to do PCA 
# I have still to check if the units of measure are coherent 
# (for the first topic the second column is in percentage)
dfind1 <- dfind0 %>% filter(Topic == 'Economic Policy & Debt: Balance of payments: Capital & financial account')
ctrind <- cast(dfind1, CountryName + CountryCode ~ IndicatorName, value = "Value")
# Boxplot
x11()
boxplot(ctrind[,3:10])
# We observe a lot of outliers

# 02. PCA on indicator without NAN ####
ctrindPCA <- cast(dfind0, CountryName + CountryCode ~ IndicatorName, value = "Value")
# Boxplot
x11()
boxplot(ctrindPCA[,3:ncol(ctrindPCA)])
x11(width=10, height=7)
par(mfrow=c(2,3))
boxplot(ctrindPCA[,3])
boxplot(ctrindPCA[,4])
boxplot(ctrindPCA[,5])
boxplot(ctrindPCA[,6])
boxplot(ctrindPCA[,7])
boxplot(ctrindPCA[,8])
# We observe a lot of outliers and to much incoherent units of measure

########################################## PCA on original data 
pc.ctrindPCA <- princomp(ctrindPCA[,3:ncol(ctrindPCA)], scores=T)
pc.ctrindPCA

summary(pc.ctrindPCA)
# To obtain the rows of the summary:
# standard deviation of the components
#pc.ctrindPCA$sd
# proportion of variance explained by each PC
#pc.ctrindPCA^2/sum(pc.ctrindPCA$sd^2)
# cumulative proportion of explained variance
#cumsum(pc.ctrindPCA$sd^2)/sum(pc.ctrindPCA$sd^2)

######################################### Loadings 
#(recall: coefficients of the linear combination of the original variables that defines each principal component)

load.ctrindPCA <- pc.ctrindPCA$loadings
load.ctrindPCA

# graphical representation of the loadings of the first six principal components
x11()
par(mar = c(1,4,0,2), mfrow = c(6,1))
for(i in 1:6) barplot(load.ctrindPCA[,i], ylim = c(-1, 1))

# Interpretation of the loadings:
# First PCs: adjusted savings
# Second PCs: Total population (neg)
# Second PCs: Land area and Surface area effect
# Second PCs: Land area vs Surface area
# Second PCs: Population density 
# Last PCs: Population growth
# The loadings reflect the previous observation: the first PC is represented by the adjusted savings, 
# the second by the Total population, etc. (units of meausure effect)

#################################### Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.ctrindPCA, las=2, main='Principal components')
barplot(sapply(ctrindPCA[,3:ncol(ctrindPCA)],sd)^2, las=2, main='Original Variables', ylab='Variances')
plot(cumsum(pc.ctrindPCA$sd^2)/sum(pc.ctrindPCA$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ctrindPCA),labels=1:ncol(ctrindPCA),las=2)

# The first PC (var. adjusted savings) explains more than 99.99% of the total variability. 
# This is due to the masking effect of that variable over the others

############################## Scores: repr of the original data in the space PC1 PC2
scores.ctrindPCA <- pc.ctrindPCA$scores  

layout(matrix(c(1,2),2))
boxplot(ctrindPCA[, 3:ncol(ctrindPCA)], las=2, col='red', main='Original variables')
scores.ctrindPCA <- data.frame(scores.ctrindPCA)
boxplot(scores.ctrindPCA, las=2, col='red', main='Principal components')

# biplot
x11()
biplot(pc.ctrindPCA, scale=0, cex=.7)

graphics.off()

# 03. Principal component analysis but on the standardized variables ----

# We compute the standardized variables
ctrindPCA.sd <- scale(ctrindPCA[,3:ncol(ctrindPCA)])
ctrindPCA.sd <- data.frame(ctrindPCA.sd)

pc.ctrindPCA <- princomp(ctrindPCA.sd, scores=T)
pc.ctrindPCA
summary(pc.ctrindPCA)

# Explained variance
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.ctrindPCA, las=2, main='Componenti principali')
abline(h=1, col='blue')
barplot(sapply(ctrindPCA.sd,sd)^2,las=2, main='Variabili originarie', ylim=c(0,6), ylab='Variances') #?????
plot(cumsum(pc.ctrindPCA$sde^2)/sum(pc.ctrindPCA$sde^2), type='b', axes=F, xlab='numero di componenti', ylab='contributo alla varianza totale', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ctrindPCA.sd),labels=1:ncol(ctrindPCA.sd),las=2)

# If we wanted to perform dimensionality reduction, we could keep
# 1 or 2 PCs

# loadings
load.ctrindPCA <- pc.ctrindPCA$loadings
load.ctrindPCA
# se aiuta l'interpretazione posso cambiare segno
x11()
par(mar = c(2,2,2,1), mfrow=c(3,1))
for(i in 1:3)barplot(load.ctrindPCA[,i], ylim = c(-1, 1), main=paste('Loadings PC ',i,sep=''))

# Interpretation of the loadings:
# The first PC represents an average of the first two and last two variables, taken with very similar (negative) weights. 
# The sencond PC contrasts the population density and the population growth.

# High PC1 (in val assoluto): the value of the first and last two variables is high
# Low PC1: the value of the first and last two variables is low
# High PC2: large density less growth
# Low PC2: high density and high growth

# scores: repr of the original data in the space PC1 PC2
scores.ctrindPCA <- pc.ctrindPCA$scores
scores.ctrindPCA

x11()
plot(scores.ctrindPCA[,1:2])

x11()
layout(matrix(c(1,2),2))
boxplot(ctrindPCA.sd, las=2, col='red', main='Original variables')
scores.ctrindPCA <- data.frame(scores.ctrindPCA)
boxplot(scores.ctrindPCA, las=2, col='red', main='Principal components')

x11()
layout(matrix(c(1,2),1))
plot(ctrindPCA.sd[,'m100'],ctrindPCA.sd[,'Marathon'],type="n",xlab="m100",ylab="Marathon", asp=1)
text(ctrindPCA.sd[,'m100'],ctrindPCA.sd[,'Marathon'],dimnames(runrec)[[1]],cex=.7)
plot(-scores.ctrindPCA[,1],-scores.ctrindPCA[,2],type="n",xlab="-pc1",ylab="-pc2", asp=1)
text(-scores.ctrindPCA[,1],-scores.ctrindPCA[,2],dimnames(ctrindPCA)[[1]],cex=.7)

x11()
biplot(pc.ctrindPCA)
