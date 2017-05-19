########### PCA script #############
setwd('/Users/mowythebest/Desktop/StatApp_test')
source('Filters/functionsFullMatrix.R')
source("get_functions.R")
source("outlier.R")
source("PCA/PCA_function.R")
load("ReadData/data.RData")

# I want to keep the fullest M indicators
M <- 400
# countries with Tind indicators or more in a specified year
df <- extract2DmatrixWithFullestIndicators(Indicators,
                                           M,
                                           viewFlag=TRUE,
                                           Tind = 400) # another name for the function?

# selection Indicators i'm interested in
# myInd <- c("Population, total",
#            "CO2 emissions (kt)",
#            "GDP per capita (constant LCU)",
#            "Time required to start a business (days)",
#            "Unemployment, total (% of total labor force)")
myYear <- c(2010)
myTopic <- c("Environment: Density & urbanization")
# myRegion <- c("Europe & Central Asia")
q <- get_Indicators(myYear = myYear,
                    #myInd.Name = myInd,
                    myTopic = myTopic,
                    ind = df,
                    ser = Series, 
                    #myRegion = myRegion,
                    count = Country,clear_name = T) 

# remove first 2 columns   
q <- q[,3:dim(q)[2]]
# remove na
w <- na.omit(q)
# remove outlier
w <- find_outlier(w, remove = T)
# PCA
pc_data1<-PC(w,method="eigen",scaled=T,graph=T,rm.na=T,print.results=T)












