# script per la regressione che vuole spiegare la crescita
path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"gaussianity/is_gaussian.R",sep = "/"))

############# ESTRAZIONE DATASET #################

# specifico gli indicatori che in teoria dovrebbero entrare nella regressione
myInd <- c("School enrollment, tertiary (% gross)",
           "Life expectancy at birth, total (years)",
           "Fertility rate, total (births per woman)",
           "General government final consumption expenditure (% of GDP)",
           "CPIA property rights and rule-based governance rating (1=low to 6=high)",
           "Trade in services (% of GDP)",
           "Inflation, consumer prices (annual %)",
           "GDP per capita (constant LCU)"
           )
# anni dell'analisi
myYear <- 1975:1995

# estraggo ciò che mi interessa tenendo ancora tutto in una forma
# Indicators-like
df1 <- getIndicators(myYear = myYear,
                     myInd = myInd,
                     agg = F)



