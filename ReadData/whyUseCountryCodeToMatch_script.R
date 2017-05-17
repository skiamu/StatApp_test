# there is no one-to-one correspondence between the CountryName in Indicators 
# and the Name of the Countries in Country

# directory with the .csv files
setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/Playground")

Indicators <- read.csv('Indicators.csv')
Country    <- read.csv('Country.csv')

v1 <- unique(Indicators$CountryName)
v2 <- Country$ShortName # try one at the time
# v2 <- Country$TableName
# v2 <- Country$LongName

setdiff(v1,v2)
setdiff(v2,v1)

# solution 
# add a new column 'CountryName' to Country with the same names found in Indicators
# use CountryCode to match

v3 <- unique(Indicators$CountryCode)
v4 <-  Country$CountryCode

setdiff(v3,v4) # OK