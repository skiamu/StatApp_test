############ print prediction for thr app ###########
# in realtà si dovrebbe predere il prediction interval, ma vista l'alta variabilità
# ho paura sia tropo grande. 
source(paste(path,"Regression/manual_stuff.R",sep = "/"))
# select the years
year.pred <- 2005:2013
X.pred <- vector("list",length(year.pred))
for(i in 1:length(year.pred)){
   
   z <- Design_Matrix_pred(myYear = year.pred[i],
                           myInd = c(myInd,myInd.continum),
                           myCountry = NULL,
                           response.vector = F)
   # dataframe with the new observations
   X0.new <- z$XD
   # change colnames and so on, always check that it does what u want (non ottimale)
   X0.new <- manual_stuff(X0.new)
   # predict new observations. I can use this model to predict also observation
   # in the same time interval where I fit it since the country may change
   X.pred[[i]] <- predict(fit.pred, newdata = X0.new, interval = "confidence" ) 
   
}
# save the list for the app
save(X.pred,year.pred,file = "prediction.RData")


