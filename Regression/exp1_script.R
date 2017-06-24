################ ###### 0) import stuff ###############################
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

# script regression 1)
# explanatory model, first time interval [1975,2005]. This script is diivided in the 
# following steps
# 1) data extraction: we create the design matrix and the response vector
# 2) modifiche manuali: work on the design matrix to get it ready for the
#                       regression
# 3) model fitting
# 4) model selection
# 
###################### 1) ESTRAZIONE DATASET #####################################
# discrete regressors : the observation of a discrete regressor in at the beginning
# of each time period, for istance if the time period in [1975,1985] the GDP
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
myYear <- seq(1975,2005,by = 10)
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
# set T if you want to see the relation between the 10-years growth and each
# regressor
marginal.plot = F

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
# check the diagnosis plots to detect outlier (uncomment the next line to check)
# plot(fit$model)
# remove outlier
d <- data.frame(Y,XD); rownames(d) <- row.country.name
XD <- find_outlier(d,remove = T)
Y <- XD[,1];XD <- XD[,-1]
# fit the second linear regression 
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
# look again for outlier (uncomment the next line to check)
# plot(fit$model)
# remove outliers: trinidad and burkinafaso
XD <- XD[-c(126,88),]; Y <- Y[-c(126,88)]
# fit the final regression
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

#######  3.2) model selection
# given the last regression, let's see if we can reduce the model for the time interval
# [1975, 2005]
# 
# rifaccio stessa regressione di sopra ma fuori dalla funzione senno casino con step
fit2 <- lm(formula,data = XD)
step(fit2)
# uso formula suggerita dalla procedura step-wise
fit3 <- lm(Y ~ fertility + education + consumi + health + I1 + openess + D1,data = XD)
summary(fit3)
# here we analyse this results taking into account the full model, the time interval
# where we are fitting and the stuff on the book
# 


#################### 4) PLOT RESPONSE VS REGRESSORS
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
