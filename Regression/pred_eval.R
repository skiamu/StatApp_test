######################## 4) PREDICTION ###################
# in this section we make prediction of the 10-years growth.
# For example, suppose we are in 2001, we wanna forecast the growth over the
# time interval [2001,2011]. Finally we evaluate our predictor computing classical 
# prediction measures (RMSE, MAD, ME).
# Hopefully, we compere our estimate against those made by the OECD
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
# good performance in [1999,2009]
myYear.new <- c(2003,2013)
z <- Design_Matrix_pred(myYear = myYear.new,
                        myInd = c(myInd,myInd.continum),
                        myCountry = NULL,
                        response.vector = T)
# dataframe with the new observations
X0.new <- z$XD
# true growth, it's used to validate the model
Y.true <- z$Y
nations <- z$nations
colnames(X0.new) <- gsub("\\(.*$", "", colnames(X0.new))
colnames(X0.new) <- gsub("\\,.*$", "", colnames(X0.new))
name <- colnames(X0.new)
# prendo il log(GDP)
X0.new[,name[3]] <- log(X0.new[,name[3]])
# prendo 1/life_expectancy = mortality rate
X0.new[,name[8]] <- 1/X0.new[,name[8]]
colnames(X0.new)[1:9] <- c("fertility","FDI","GDP","investment","education","consumi",
                           "inflation","health","openess")
# here we set the formula for the prediction model. By default is the formula 
# of the model built in file pred_model.R , so if u want to change the model
# go there
formula.pred <- formula

# the model is fitted on the data from "pred_model.R" where we built the prediction
# model
fit.pred <- lm(formula.pred, data = data.frame(Y,XD))
summary(fit.pred)
# predict new observations. I can use this model to predict also observation
# in the same time interval where I fit it since the country may change
pred <- predict(fit.pred, newdata = X0.new, interval = "confidence" ) 


######## PREDICTION MODEL VALIDATION
true.pred <- data.frame(True = Y.true,pred = pred[,1])
rownames(true.pred) <- nations
# errore di predizione
e <- Y.true - pred[,1]
# root mean square errors (stima della deviazione standard)
RMSE <- sqrt(sum((pred[,1] - Y.true)^2) / length(Y.true));print(RMSE)
# il modello tende a sovrastimare la crescita
ME <- mean(e);print(ME)
# mean absolute deviation
MAD <- mean(abs(e));print(MAD)
true.pred.e <- cbind(true.pred,e)

############ prediction comparison
source(paste(path,"Regression/manual_stuff.R",sep = "/"))

myYear.new <- c(2010)
z <- Design_Matrix_pred(myYear = myYear.new,
                        myInd = c(myInd,myInd.continum),
                        myCountry = NULL,
                        response.vector = F)
# dataframe with the new observations
X0.new <- z$XD
# true growth, it's used to validate the model
Y.true <- z$Y
nations <- z$nations
X0.new <- manual_stuff(X0.new)
# predict new observations. I can use this model to predict also observation
# in the same time interval where I fit it since the country may change
pred <- predict(fit.pred, newdata = X0.new, interval = "confidence" ) 
# you need to set regression as a working directory to run the script
source(paste(path,"Regression/web_data.R",sep = "/"))






