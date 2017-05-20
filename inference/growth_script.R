######## GROWTH GDP rate  ################
#
# this script performs Inference analysis on the GDP per capita growth rate.
# This Incicators gives a measure of the health of the nation's economy, normalized
# by the population
# 
path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"gaussianity/is_gaussian.R",sep = "/"))

# fix the time window
myYear <- 1970:2013
# select the desired indicators
myInd <- "GDP per capita growth (annual %)"
# select the aggregate countries used for the first part of the analysis
myAgg <- c("East Asia & Pacific (all income levels)",
           "Europe & Central Asia (all income levels)",
           "Latin America & Caribbean (all income levels)",
           "Middle East & North Africa (all income levels)",
           "Sub-Saharan Africa (all income levels)",
           "North America" 
)
# get the specified Indicators
df1 <- getIndicators(myYear = myYear,
                     myInd = myInd,
                     myAggregate = myAgg
)
# get the desired dataframe ready to be analysed, drop the observation
# if there's at least one NA, show who it is
df <- getCntYear(df1,
                 myInd,
                 dropNA = T,
                 showCnt = T)

# plot lines:
pp1 <- ggplot(df1,aes(x = Year,y = Value, colour = CountryName, 
                     linetype = CountryName)) +
   geom_line()  +
   geom_point()  + 
   geom_dl(aes(label = CountryCode),
           method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
   ggtitle("GDP per capita growth (annual %)") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))

pp1

# question: has the Growth been constant (without trends) over the last 4 
# decades? dubbi, discutere 
# 
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
df <- as.data.frame(t(df))
# accetto H0, non ho evidenza per dire che non è stata costante
delta0 <- rep(0,p)
T2.test(X = diff, delta0)

# question: has the Growth been zero, ON AVERAGE, over the last four decades?
# 
# method applied: T2 test for the mean vector + Bonferroni for marginal
df.t <- as.data.frame(t(df))
delta0 <- rep(0,p)
T2.test(X = df.t, delta0)

########### impact of the crisis ###########
# inspired by plot 1 let's analyse what happend in 2008 /2009.
# It must bu something really impactful since data at continent level
# experienced a huge negative bump. We analyze growth before, during and
# after the crisis to see if there's statistical evidence that the crisis
# event affected the economic growth
#
# applied technique : inference with repeated measure
# repeated measure: before, during and after the crisis
myYear <- c(2007,2009,2014)
# myYear <- c(2000,2003,2006)

df1 <- getIndicators(myYear = myYear,
                     myInd = myInd
                     #myAggregate = myAgg
)
df <- getCntYear(df1,
                 myInd,
                 dropNA = T,
                 showCnt = T)
# calcolo distanza malanobis
a <- mahalanobis(df,colMeans(df),cov(df))
# vwdo quale stato sta sopra 12 (già pessima)
a[a>12]
# rimuovo per il plot, altrimenti non si vede il pattern
df1 <- df1 %>% 
   filter(!(CountryName %in% c("Afghanistan","Azerbaijan",
                               "Libya","West Bank and Gaza","South Sudan","United Arab Emirates")))
pp2 <- ggplot(df1,aes(x = Year,y = Value, colour = CountryName)) +
   geom_line()  +
   geom_point()  + guides(colour=FALSE) +
   geom_dl(aes(label = CountryCode),
           method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
   ggtitle("GDP per capita growth (annual %)") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))

pp2

# define contrast matrix: we want to study mu1-mu2,mu1-mu3
C <- rbind(c(1,-1,0),c(1,0,-1))
# remove outlier, fuck them
w <- find_outlier(X = df,remove = T,plot = T)
# set the null hp
delta0 <- rep(0,dim(C)[1])
# performe the analysis
rep_measure(X = w,C,delta0 = delta0)
# check gaussianity in details
is.gauss(w)
# miracolosamente gaussiani


######## study the region and year factor in growth #########
#
# per fare ANOVA devo dare in input vettori di uguale lunghezza.
# uno deve essere quello delle osservazioni X_i_j gli altri sono 
# vettori di fattori e corrispondono al primo e secondo livello di ogni
# ossrvazione

# create vector level1 = Year
Year <- as.factor(df1$Year)

# create vector level2 = Region
Region <- Country %>% select(CountryName, Region)
fRegion <- factor(levels = unique(Region$Region))
for(i in 1:dim(df1)[1]){
   q <- Region %>% filter(CountryName == df1$CountryName[i])
   fRegion[i] <- as.factor(q$Region)
}

# create vector level3 = IncomeGroup
Income <- Country %>% select(CountryName,IncomeGroup)
fIncome <- factor(levels = unique(Income$IncomeGroup))
for(i in 1:dim(df1)[1]){
   q <- Income %>% filter(CountryName == df1$CountryName[i])
   fIncome[i] <- as.factor(q$IncomeGroup)
}

# create the vector of observations
Growth <- df1$Value

# we now have the three ingredients of ANOVA two way.

# no interaction
fit.anova1 <- aov(Growth ~ fRegion + Year)
summary.aov(fit.anova1)
# strong evidence that factor 1 and factor 2 both affect the Growth

# with interaction
fit.anova2 <- aov(Growth ~ fRegion + Year + fRegion:Year)
summary.aov(fit.anova2)
interaction.plot(Year,fRegion,Growth)

# what happens if I'm not in an economic crisis period?
myYear <- c(2000,2003,2006)

# Very interestingly the interaction effect vanishes: if we move from one 
# year to another the growth doesn't change "behaviour" from a region
# to another region. Changing the year there is know region effect.
# the crisis creates a region effect, when things go bad the growth react
# differently depending which region we are considering.


################## three way ANOVA #############

# no interaction
fit.anova3 <- aov(Growth ~ fRegion + Year + fIncome)
summary.aov(fit.anova3)

# with interaction
fit.anova4 <- aov(Growth ~ fRegion + Year + fIncome + Year:fRegion + Year:fIncome + fIncome:fRegion )
summary.aov(fit.anova4)

interaction.plot(fRegion,Year,Growth,type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Regions",
                 ylab="Growth",
                 main="Interaction Plot")

interaction.plot(Year,fRegion,Growth)
# create a dataframe for ggplotting
w <- data.frame(Year = Year, Region = fRegion, Value = Growth)
ppp <- ggplot(data = w,
             aes(x = Year, y = Value, colour = fRegion, group=fRegion)) +
   stat_summary(fun.y=mean, geom="point")+
   stat_summary(fun.y=mean, geom="line") +
   ggtitle("GDP per capita growth (annual %)") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))
ppp

ppp2 <- ggplot(data = w,
              aes(x = fRegion, y = Value, colour = Year, group=Year)) +
   stat_summary(fun.y=mean, geom="point")+
   stat_summary(fun.y=mean, geom="line") + 
   ggtitle("GDP per capita growth (annual %)") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))
ppp2

ppp3 <- ggplot(data = w,
               aes(x = fIncome, y = Value, colour = Year, group=Year)) +
   stat_summary(fun.y=mean, geom="point")+
   stat_summary(fun.y=mean, geom="line")
ppp3

# in questo vedo bene che la crisi ha colpito più gli stati ad high income
ppp4 <- ggplot(data = w,
               aes(x = Year, y = Value, colour = fIncome, group=fIncome)) +
   stat_summary(fun.y=mean, geom="point")+
   stat_summary(fun.y=mean, geom="line")
ppp4

