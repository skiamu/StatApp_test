path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"get_functions.R",sep = "/"))
source(paste(path,"gaussianity/is_gaussian.R",sep = "/"))
source(paste(path,"PCA/PCA_function.R",sep = "/"))
library(ggplot2)
library(directlabels)
# fix the time window
myYear <- 2005
myTopic <-"Public Sector: Policy & institutions"
# select the desired indicators
myInd <- "Market capitalization of listed domestic companies (current US$)"
myInd2 <- "S&P Global Equity Indices (annual % change)"
myInd <- c("CPIA business regulatory environment rating (1=low to 6=high)",
           "CPIA debt policy rating (1=low to 6=high)",
           "CPIA economic management cluster average (1=low to 6=high)",
           "CPIA efficiency of revenue mobilization rating (1=low to 6=high)",
           "CPIA financial sector rating (1=low to 6=high)",
           "CPIA fiscal policy rating (1=low to 6=high)",
           "CPIA macroeconomic management rating (1=low to 6=high)",
           "CPIA property rights and rule-based governance rating (1=low to 6=high)",
           "CPIA quality of public administration rating (1=low to 6=high)",
           "CPIA structural policies cluster average (1=low to 6=high)",
           "CPIA trade rating (1=low to 6=high)")
myInd.small <- c("CPIA economic management cluster average (1=low to 6=high)",
                 "CPIA financial sector rating (1=low to 6=high)",
                 "CPIA efficiency of revenue mobilization rating (1=low to 6=high)"
                 )

# get the specified Indicators
df1 <- getIndicators(myYear = myYear,
                     myInd = myInd.small,
                     agg = F,
                     myTopic = myTopic
)

df <- get_Indicators(myYear = myYear,
                     myInd.Name = myInd.small,
                     clear_name = T)
df <- df[,-(1:2)]
w <- PC(df,print.results = T,graph = T)
S <- cor(df); unname(S)
df <- find_outlier(df,remove = T,plot = T)

# compute the distrance matrix with 3 different distances
df.e <- dist(df, method = "euclidean")
df.m <- dist(df, method = "manhattan")
df.c <- dist(df, method = "canberra")

# run hierarchical clustering with 3 different method for eanch distance:

df.es <- hclust(df.e, method='single')
df.ec <- hclust(df.e, method='complete')
df.ea <- hclust(df.e, method='average')
x11()
par(mfrow=c(1,3))
plot(df.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

df.ms <- hclust(df.m, method='single')
df.mc <- hclust(df.m, method='complete')
df.ma <- hclust(df.m, method='average')
x11()
par(mfrow=c(1,3))
plot(df.ms, main='manhattan-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.mc, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.ma, main='manhattan-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')


df.cs <- hclust(df.c, method='single')
df.cc <- hclust(df.c, method='complete')
df.ca <- hclust(df.c, method='average')
x11()
par(mfrow=c(1,3))
plot(df.cs, main='canberra-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.cc, main='canberra-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(df.ca, main='canberra-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

cluster.ec <- cutree(df.ec, k=2) # euclidean-complete:
cluster.ec
df.c <- mutate(df1[-(c(19,44,63)),], lab = factor(cluster.ec))
