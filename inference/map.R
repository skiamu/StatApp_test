library(readr)
library(rworldmap)

# Update these to plot different indicators
indicatorName <- "GDP per capita growth (annual %)"
indicatorYear <- 2009
indicators <- Indicators

filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]

sPDF <- joinCountryData2Map(filtered,
                            joinCode = "ISO3",
                            nameJoinColumn = "CountryCode",
                            verbose = TRUE)

# png("map.png", width=900, height=550)
mapCountryData(sPDF, nameColumnToPlot='Value', mapTitle=indicatorName)