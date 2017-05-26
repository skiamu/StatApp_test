########## serial PCA  ##########
# this script computes PCA for each Topic.
# Topic j is represented by the first principal component of PCA j

path <- "/home/andrea/StatApp/StatApp_test"
source(paste(path,"Filters/functionsFullMatrix.R",sep = "/"))
source(paste(path,"get_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"PCA/PCA_function.R",sep = "/"))
source(paste(path,"clear_na.R",sep = "/"))
load(paste(path,"ReadData/data.RData",sep = "/"))

# show the best M Indicators
M <- 300
# countries with Tind indicators or more in a specified year, the other
# are discarded. Data frame in Indicators-like format
mylist <- extract2DmatrixWithFullestIndicators(Indicators,
                                           M,
                                           viewFlag=TRUE,
                                           Tind = 400) # another name for the function?
# df is the filtered dataframe Indicators
df <- mylist$y
# dataframe with IndicatorNames and number of Countries
Indicator.good <-mylist$x

# extract all the indicators name from Indicators
myInd <- as.character(Indicator.good$IndicatorName)
# fix the year
myYear <- c(2010)

q <- get_Indicators(myYear = myYear,
                    myInd.Name = myInd,
                    #myTopic = myTopic,
                    ind = df,
                    ser = Series, 
                    #myRegion = myRegion,
                    count = Country,clear_name = F) 
# the country in w are the ones with values for all the Indicator.good
w <- clear_na(q, Country)

myCountries <- as.character(w$CountryCode) 

###### bulid a list Topic-Indicators_dataframe #######
# select the Topic from Series
Topics <- as.character(unique(Series$Topic))
Topics_X <- NULL
for(i in 1:length(Topics)){
   Topics_X[i] <- paste("X",i,sep="")
}
# fill up the blank spaces in the variables names
names(w) <- make.names(colnames(w))
# fill up the blank spaces in the variables names
Indicator.good <- transform(Indicator.good, IndicatorName = make.names(Indicator.good$IndicatorName))
# initialize an empty dataframe
d <- data.frame(row.names = myCountries)
for(i in 1:length(Topics)){
   #  Indicators in the i Topic
   IND <- Series %>% filter(Topic == Topics[i]) 
   # Indicators.good of Topic i
   qq <- filter(Indicator.good,IndicatorName %in% make.names(IND$IndicatorName))
   # do PCA only if we have 4 or more Indicators in a Topic
   
   if(sum(qq$IndicatorName %in% colnames(w)) >= 4){
      ind <- w %>% select_(qq$IndicatorName)
      pc_data1<-PC(ind,method="eigen",scaled=T,graph=F,rm.na=T,print.results=F)
      a <- Topics_X[i]
      print(a)
      d[Topics_X[i]] <- pc_data1$scores[,1]
   }
}

rownames(d) <- 1:87
d <- find_outlier(d, remove = T)

pc_data1<-PC(d,method="eigen",scaled=T,graph=T,rm.na=T,print.results=T)


