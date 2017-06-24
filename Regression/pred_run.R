# run prediction:
# this file runs the following scripts:
# 
# 1) pred_model.R, to crate the prediction model
# 2) predict_eval.R, to evaluate the predictor and make a comparison with
#    OECD prediction for the interval [2010,2020]
# 3) pred_print.R, to actually make the predictions for the specified intervals
# 
# 
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
source(paste(path,"Regression/lin_reg.R",sep = "/"))
source(paste(path,"Regression/Design_Matrix.R",sep = "/"))
source(paste(path,"Regression/Design_Matrix_pred.R",sep = "/"))
# attenzione a questi due pacchetti che vanno in conflitto
library(MASS)
library(dplyr)
select <- dplyr::select 

# the the scripts for the predictions:
source(paste(path,"Regression/pred_model.R",sep = "/"))
source(paste(path,"Regression/pred_eval.R",sep = "/"))
source(paste(path,"Regression/pred_print.R",sep = "/"))
