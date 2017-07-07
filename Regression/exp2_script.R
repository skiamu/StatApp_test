################ import stuff ###############################
rm(list = ls())
path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"gaussianity/is_gaussian.R",sep = "/"))
source(paste(path,"Regression/lin_reg.R",sep = "/"))
source(paste(path,"Regression/Design_Matrix.R",sep = "/"))
source(paste(path,"Regression/Design_Matrix_pred.R",sep = "/"))
# attenzione a questi due pacchetti che vanno in conflitto
library(MASS)
library(dplyr)
select <- dplyr::select 

# script regression 2)
# explanatory model, second time interval [1972,2012]. This script is diivided in the 
# following steps
# 1) data extraction: we create the design matrix and the response vector
# 2) modifiche manuali: work on the design matrix to get it ready for the
#                       regression
# 3) model fitting
# 4) model selection
# 
###################### 1) ESTRAZIONE DATASET #####################################
# discrete regressors : the observation of a discrete regressor in at the beginning
# of each time period, for istance if the time period in [1982,2012] the GDP
# is taken in 1975
myInd <- c("Gross enrolment ratio, primary, both sexes (%)",
           "Life expectancy at birth, total (years)",
           "Fertility rate, total (births per woman)",
           "Inflation, GDP deflator (annual %)",
           "GDP per capita (constant 2005 US$)",
           "Household final consumption expenditure, etc. (% of GDP)",
           "Foreign direct investment, net inflows (% of GDP)")

# continuum regressors: the observations of a contunuum regressor is an average on
# 9 years. For example, the trade for the time interval [1975,1985] is the average
# on the interval [1975,1984]. Since this model is for explanation, i'm allowd to do that.
myInd.continum <- c("General government final consumption expenditure (% of GDP)",
                    "Trade (% of GDP)")
# discrete years
myYear <- seq(1983,2013,by = 10)
# continuum years
myYear.continum <- myYear[1]:myYear[length(myYear)]
# compute the design matrix XD and the response vector Y
w <- Design_Matrix(myYear = myYear,myYear.continum = myYear.continum,
                   myInd = myInd, myInd.continum = myInd.continum)
# extract from the output
Y <- w$Y
XD <- w$XD
# number of countries in the analysis
n <- w$n
nazioni <- w$nazioni
M <- w$statistics
# set T if you want to see the relation between the 10-years growth and each
# regressor
marginal.plot = T

###################### 2) MODIFICHE MANUALI ######################
# in this section we have to manually sort out the dataframe XD, for esample
# we need to transform regressors (e.g. take the log(GDP)) change columns name
# and so on

# da cambiare se cambiano i regressori
colnames(XD) <- gsub("\\(.*$", "", colnames(XD))
colnames(XD) <- gsub("\\,.*$", "", colnames(XD))
name <- colnames(XD)

# prendo il log(GDP), always check that the column number is the desired one
XD[,name[3]] <- log(XD[,name[3]])

# prendo 1/life_expectancy = mortality rate, always check that the column number
#  is the desired one
XD[,name[7]] <- 1/XD[,name[7]]
# shorten the names for graphical reasons
colnames(XD)[1:7] <- c("fertility","FDI","GDP","education","consumi","inflation",
                       "health")
colnames(XD)[12:13] <- c("investment","openess")
# if there're NAs there's something wrongm go back and fix it
if(sum(is.na(XD)) != 0){stop("XD contiene na, correggere immediatamente")}


###################### 3) REGRESSION ANALYSIS #################################
# In this section we perform the linear regression on the data extracted in the
# previous one. We check the assumption looking at the diagnosis plots, we performe
# a stepwise model selection, plot the regressors against the response, detect
# and remove outliers
#
# 3.1) regression fitting and outliers
# 
# create the regression formula
formula <- paste("Y ~ ",colnames(XD)[1],sep = "")
for(i in 2:dim(XD)[2]){
   formula <- paste(formula,colnames(XD)[i],sep = "+")
}
# substituite the country names with numbers since it's easier to spot outlier
# in the diagnosis plots if each observation is a number
row.country.name <- rownames(XD)
rownames(XD) <- 1:(3*n)
# fit the first linear regression
fit <- lin_reg(Y,
               XD,
               formula = formula,
               X0.new = NULL,
               interval = NULL,
               print.plot = F,
               print.result = T,
               print.band = F,
               pointwise = F,
               print.plot.DIAGN = F)
# # check the diagnosis plots to detect outlier
# plot(fit$model)
# # remove outlier: in [1982,2012] and [1983,2013] there's no need
# d <- data.frame(Y,XD); rownames(d) <- row.country.name
# XD <- find_outlier(d,remove = T)
# XD <- XD[-c(32,95),]; Y <- Y[-c(32,95)]
# 
# Y <- XD[,1];XD <- XD[,-1]

#######  3.2) model selection + INTERACTION
# given the last regression, let's see if we can reduce the model for the time interval
# [1983, 2013]
# 

# adding the interaction:

formula <- Y ~ fertility+FDI+GDP+education+consumi+inflation+health+R1+R2+I1+I2+
   investment+openess+D1+D2 + 
   # interazioni con gli anni
   D1:GDP + D1:fertility + D1:investment + 
   D2:GDP + D2:fertility + D2:investment + 
   I1:GDP + I1:fertility + I1:investment + 
   I2:GDP + I2:fertility + I2:investment +
   # interazioni con asia
   R1:GDP + R1:fertility + R1:investment+
   # interazioni con africa sub-sahariana
   R2:GDP + R2:fertility + R2:investment
fit3 <- lm(formula,data = XD)
summary(fit3)
stepAIC(fit3)
formula <- Y ~ fertility + FDI + GDP + education + consumi + 
   health + R1 + R2 + I1 + I2 + investment + D1 + D2 + GDP:D1 + 
   investment:D1 + GDP:D2 + GDP:I1 + fertility:I1 + I1:investment + 
   I2:investment + GDP:R1 + fertility:R1 + fertility:R2 + R2:investment
fit4 <- lm(formula,data = XD)
summary(fit4)
plot(fit4)
XD <- XD[-c(109),]; Y <- Y[-c(109)]
# here we analyse this results taking into account the full model, the time interval
# where we are fitting and the stuff on the book
# 
i = 3
d <- data.frame(Y,XD[,i]); colnames(d) <- c("Y",colnames(XD)[i])
pp1 <- ggplot(d,aes_string(x =colnames(d)[2] ,y = colnames(d)[1])) +
   geom_point()  + 
   geom_smooth(method = "lm") +
   ggtitle("GDP per capita growth (annual %)") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm")) 
print(pp1)


# test significatività Income
beta <- coefficients(fit4)
A <- matrix(0,nrow = 5,ncol = 25)
A[1,13]<-1;A[2,14]<-1;A[3,15]<-1;A[4,16]<-1;A[5,17]<- 1
b<- rep(0,5)
linearHypothesis(fit4,A,b)

# test significatività Region
A <- matrix(0,nrow = 6,ncol = 25)
A[1,10]<-1;A[2,11]<-1;A[3,18]<-1;A[4,19]<-1;A[5,20]<- 1;A[6,21]<-1
b<- rep(0,6)
linearHypothesis(fit4,A,b)


# test significatività Period
A <- matrix(0,nrow = 6,ncol = 25)
A[1,8]<-1;A[2,9]<-1;A[3,22]<-1;A[4,23]<-1;A[5,24]<- 1;A[6,25]<-1
b<- rep(0,6)
linearHypothesis(fit4,A,b)




texreg(fit4, booktabs = T, dcolumn = T,single.row = T,digits = 4,
       table = F)



# plotte risposta vs singolo regressore per vedere l'andamento
if(marginal.plot){
   library(ggplot2)
   graphics.off()
   for(i in 1:(length(myInd)+length(myInd.continum))){
      d <- data.frame(Y,XD[,i]); colnames(d) <- c("Y",colnames(XD)[i])
      pp1 <- ggplot(d,aes_string(x =colnames(d)[2] ,y = colnames(d)[1])) +
         geom_point()  + 
         geom_smooth(method = "lm") +
         ggtitle("GDP per capita growth (annual %)") +
         theme(axis.text=element_text(size=12),
               axis.title=element_text(size=14,face="bold"),
               legend.key.size = unit(1.5,"cm")) 
      print(pp1)
      Sys.sleep(3)
   }
}
