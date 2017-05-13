########### PCA script #############


# show the best M Indicators

setwd('/Users/mowythebest/Desktop/StatApp_test')
source('Filters/functionsFullMatrix.R')
source("get_functions.R")
source("outlier.R")
source("PCA/PCA_function.R")
load("ReadData/data.RData")

# I want to keep the fullest M indicators
M <- 600
# countries with Tind indicators or more in a specified year, the other
# are discarded. Data frame in Indicators-like format
df <- extract2DmatrixWithFullestIndicators(Indicators,
                                           M,
                                           viewFlag=TRUE,
                                           Tind = 400) # another name for the function?
# selection Indicators i'm interested in
myInd <- c("Export value index (2000 = 100)",
           #"GDP at market prices (current US$)",
           "Import value index (2000 = 100)",
           "GDP growth (annual %)",
           "Merchandise trade (% of GDP)"
           )
# myInd <- c("Energy use (kg of oil equivalent per capita)",
#            "Fossil fuel energy consumption (% of total)",
#            "Electricity production from oil sources (% of total)",
#            "Energy imports, net (% of energy use)"
#            )
myYear <- c(2010)
# myTopic <- c("Environment: Natural resources contribution to GDP")
# myRegion <- c("Europe & Central Asia")
q <- get_Indicators(myYear = myYear,
                    myInd.Name = myInd,
                    #myTopic = myTopic,
                    ind = df,
                    ser = Series, 
                    #myRegion = myRegion,
                    count = Country,clear_name = T) 

# remove na
w <- clear_na(q, Country)
# remove first 2 columns   S
w1 <- w[,3:dim(w)[2]]
# remove outlier
w2 <- find_outlier(w1, remove = T)
graphics.off()
# PCA
pc_data1<-PC(w1,method="eigen",scaled=T,graph=F,rm.na=T,print.results=F)

w1 <- mutate(w1, test = X1 - X2)










