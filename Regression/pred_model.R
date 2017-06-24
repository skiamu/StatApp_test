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

# script regression 3)
# this script performs linear regression using just discrete regressors.
# We use this script to buil the model for prediction.
# Each observation is taken at the beginning of the time interval, as it happens
# in predictions
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
# discrete years: good intervals are [83,13] [82,12]
myYear <- seq(1983,2013,by = 10)
# continuum years
myYear.continum <- myYear[1]:myYear[length(myYear)]
# compute the design matrix XD and the response vector Y
w <- Design_Matrix(myYear = myYear,
                   myYear.continum = NULL,
                   myInd = c(myInd, myInd.continum),
                   myInd.continum = NULL,
                   dummy.period = F)
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
XD[,name[8]] <- 1/XD[,name[8]]
# shorten the names for graphical reasons
colnames(XD)[1:9] <- c("fertility","FDI","GDP","investment","education","consumi",
                       "inflation","health","openess")
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
rownames(XD) <- 1:length(Y)
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
