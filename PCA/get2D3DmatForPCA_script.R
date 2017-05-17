# extract 2D and 3D matrix for PCA

# IMPORTANT: changed read_data.R rerun it
#            [added a column in Country to match CountryName (TableName was not working),
#             added two dataframe to pass from Name to Code and viceversa for ind and cnt]

# 00 preliminars ----
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")
#source("ReadData/read_data.R") #It could give some problems with dplyr-plyr
#read_data("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/Playground")
load("ReadData/data.RData")

#library(reshape2)                       # dcast
library(dplyr)                          # %>%

source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
#source("outlier.R")                     # find_outlier
#source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...

# 01 from a previous analysis ----
# fixing the year (fixYear) select the best M indicators after filtering out the countries 
# with a number of indicators below Tind 
df <- extract2DmatrixWithFullestIndicators(Indicators, 
                                           M=200, 
                                           fixYear=2010, 
                                           viewFlag=F, 
                                           Tind=400)

# manual selection of indicators (among the fullest)
myInd <- c('GDP (current LCU)',
           'Health expenditure per capita (current US$)',
           'Population, total',
           'Birth rate, crude (per 1,000 people)',
           'CO2 emissions (kt)',
           'Number of infant deaths')
# keep only the countries with at least Tind indicators in 2010
myCnt <- df$CountryName %>% unique() 
# years
myYears <- c(2005:2010)

# 02 extract from indicators ----
# filter Indicators
indF <- getIndicators(myYear = myYears, 
                      myCnt = myCnt, 
                      myInd = myInd)


# get some 2D matrices
# Cnt vs Ind  for 2010
dcCntInd  <- getCntInd(indF, 2010, dropNA = T, showCnt = T)
# Ind vs Year for Italy
dcIndYear <- getIndYear(indF, 'Italy', dropNA = T, showCnt = T)
# View(dcCntInd); View(dcIndYear)
# REM:you may create similar functions with different combinations of rows and columns

# drop the countries which have a missing values for at least one year and one indicator
indFull <- unifCnt(indF) # it's Indicators-like with the correspondent 3D matrix is Full

# the matrix is full?
fullness(indFull) == 1
fullness(indF)    == 1


# get 3D matrices
indFull3D <- get3D(indFull, myYears) # full
indF3D    <- get3D(indF, myYears)    # some NA, still working only a warning, be aware

# to extract from the list: listName[[pos]]
dc1 <- indFull3D[[1]]


# 03 try the change name-code ----

myCnt_code <- name2codeCnt(myCnt,      cntNameCode)
myCnt_name <- code2nameCnt(myCnt_code, cntNameCode)
setequal(myCnt_name,myCnt) # it works

myInd_code <- name2codeInd(myInd,      indNameCode)
myInd_name <- code2nameInd(myInd_code, indNameCode)
setequal(myInd_name,myInd) # it works


# 04 perform PCA and further analysis [incomplete] ----
# remove outlier
# w <- find_outlier(w, remove = T)
# PCA
# pc_data1<-PC(w,method="eigen",scaled=T,graph=T,rm.na=T,print.results=T)

