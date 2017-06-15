# regressione per portfolio

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
myYear <- 2003:2010
# select the desired indicators
myYear.v <- c(myYear,myYear-1)
ind1 <- "CPIA property rights and rule-based governance rating (1=low to 6=high)"
ind2 <- "GDP per capita (constant LCU)"
myInd1 <- c(
   "GDP per capita growth (annual %)",
   "Inflation, consumer prices (annual %)",
   "Market capitalization of listed domestic companies (current US$)",
   "Exports of goods and services (current LCU)"
)
ind <- "Stocks traded, total value (current US$)"
myAgg <- c("East Asia & Pacific (all income levels)",
           "Europe & Central Asia (all income levels)",
           "Latin America & Caribbean (all income levels)",
           "Middle East & North Africa (all income levels)",
           "North America" ,
           "Sub-Saharan Africa (all income levels)")

df1 <- getIndicators(myYear = myYear,
                     myInd = ind1,
                     agg = F)
df2 <- getIndicators(myYear = myYear,
                     myInd = ind2,
                     agg = F)

w1 <- getCntYear(df1,ind1)
w2 <- getCntYear(df2,ind2)
# check i dati per vedere come sono definiti
grow.2004 <- w1[1,2]
gdp.2003 <- w2[1,1]
gdp.2004 <- w2[1,2]
((gdp.2004-gdp.2003) / (gdp.2003)) * 100



X <- data.frame(matrix(ncol = dim(w)[2]-1,nrow = dim(w)[1] ))
# costruisco l'indicatore return
for(i in 1:(dim(w)[2]-1)){
   X[,i] <- (w[,i+1] - w[,i]) / w[,i]
}
rownames(X) <- rownames(w)


df1 <- getIndicators(myYear = myYear[2],
                     myInd = myInd1,
                     agg = F)
df.a <- getCntInd(df1,myYear[2])
df.a <- df.a[which(df.a$`Market capitalization of listed domestic companies (current US$)`>1e10),]
w <- w[rownames(w) %in% rownames(df.a),]
df.a <-df.a[rownames(df.a) %in% rownames(w),]

df.a[,4] <- X[rownames(w),1]
df <- df.a
colnames(df) <- gsub("\\(.*$", "",colnames(df) )
nomi <- colnames(df)
colnames(df) <- c("X1","X2","X3","Y")
S <- cor(df); unname(S)
df <- find_outlier(df,remove = T,plot = T)
graphics.off()

# mi serve per capire se i regressori sono ben indipendenti, 
# oltre a guardare la covarianza
w <- PC(df,print.results = T,graph = T)
graphics.off()

########### REGRESSIONE ###########
df <- data.frame(scale(df))
attach(df)
fm <- lm(Y ~ X2 + X3 )
summary(fm)
plot(X1,Y)





