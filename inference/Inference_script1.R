######## script for inference ##############
path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
library(directlabels)

# select the ingredients
myYear <- 1970:2013
# myInd <- "Central government debt, total (% of GDP)"
myInd <- "Inflation, consumer prices (annual %)"
myInd <- "Life expectancy at birth, total (years)"
myInd <- "GDP per capita growth (annual %)"
myInd <- "GDP per capita (constant 2005 US$)"
myRegion <- "Europe & Central Asia"
myAgg <- c("East Asia & Pacific (all income levels)",
           "Europe & Central Asia (all income levels)",
           "Latin America & Caribbean (all income levels)",
           "Middle East & North Africa (all income levels)",
           "Sub-Saharan Africa (all income levels)",
           "North America" 
           )
myCountry <- c("Italy","France","Germany","UK","Spain","Greece","Austria",
               "Portugal")
df1 <- getIndicators(myYear = myYear,
                    myInd = myInd,
                    #myRegion = myRegion
                    #myCnt = myCountry
                    myAggregate = myAgg
                    )
df <- getCntYear(df1,
                 myInd,
                 dropNA = T,
                 showCnt = T)
# plot for judtifying the analysis, what do u expect....
s <- df1 %>% group_by(Year) %>% summarise(z = mean(Value))
pp <- ggplot(df1,aes(x = Year,y = Value, colour = CountryName, 
                    linetype = CountryName)) +
   geom_line()  +
   geom_point()  + 
   geom_dl(aes(label = CountryCode),
           method = list(dl.combine("first.points", "last.points"), cex = 0.8)) 

# p <- geom_point(data = s, aes(x = Year,y = z))
# p <- ggplot(s,aes(x = Year, y = x)) +  geom_smooth()   
# p <- p + geom_hline(yintercept = 2, linetype="solid", 
#                     color = "blue", size=1.0) 

pp
#### inference analysis######

# traspose the dataframe
df <- as.data.frame(t(df))
n <- dim(df)[1]
p <- dim(df)[2]
# initialize a dataframe with a row less
diff <- df[1:(n-1),]
# consider the increments year to year
for(i in 1:(n-1)){
   diff[i,] <- df[i+1,] - df[i,]
}

# accetto H0, non ho evidenza per dire che non è stata costante
delta0 <- rep(0,p)
T2.test(X = diff, delta0)

# rifiuto globalmente accetto marginali
df.t <- as.data.frame(t(df))
delta0 <- rep(0,p)
T2.test(X = df.t, delta0)

# COMMENTI:
# GDP lo  prendo nell'intervallo 1970:2010, veglio testare se rimane costante
# quindi studio gli incrementi e vedo se sono nulli
det(cor(df.t))
mcshapiro.test(df.t)
