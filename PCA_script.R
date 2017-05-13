########### PCA script #############
path <- "/home/andrea/StatApp/StatApp_test"
source(paste(path,"functionsFullMatrix.R",sep = "/"))
source(paste(path,"get_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"PCA/PCA_function.R",sep = "/"))
source(paste(path,"clear_na.R",sep = "/"))
load(paste(path,"data.RData",sep = "/"))

# show the best M Indicators
M <- 600
# countries with Tind indicators or more in a specified year, the other
# are discarded. Data frame in Indicators-like format
df <- extract2DmatrixWithFullestIndicators(Indicators,
                                           M,
                                           viewFlag=TRUE,
                                           Tind = 400) # another name for the function?

# selection Indicators i'm interested in
myInd <- c("Methane emissions (kt of CO2 equivalent)",
           "PM2.5 air pollution, mean annual exposure (micrograms per cubic meter)",
           "CO2 emissions (kt)",
           "Inflation, consumer prices (annual %)",
           "GDP per capita (constant LCU)",
           "Unemployment, total (% of total labor force)")
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
# remove first 2 columns   
w <- w[,3:dim(w)[2]]
# remove outlier
w <- find_outlier(w, remove = T)
# PCA
pc_data1<-PC(w,method="eigen",scaled=T,graph=T,rm.na=T,print.results=T)












