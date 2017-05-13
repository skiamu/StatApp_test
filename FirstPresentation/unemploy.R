
   


library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(dtplyr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
library(maps, warn.conflicts = FALSE, quietly = TRUE)

# df <- fread("/home/andrea/StatApp/StatApp_test/Indicators.csv")

###What can the World Development Indicator dataset tell us about unemployment?

####Where in the world are unemployment rates the highest?


# Correction thanks to Ben Hamner
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
   df[df$CountryName==c,"CountryName"] = correction[c]
}

ue2013<-df %>%
   filter(Year==2014
          & IndicatorCode =="SL.UEM.TOTL.ZS")

map.world <- merge(map_data(map="world"),
                   select(ue2013,CountryName,Value),
                   by.x='region',
                   by.y='CountryName',
                   all.x=TRUE,
                   fill=0)
map.world <- map.world[order(map.world$order),]

p <- ggplot(map.world) +
   geom_map(map=map.world, aes(map_id=region, x=long, y=lat, fill=Value)) +
   scale_fill_gradient(low ="yellow",high="red",guide="colourbar",name="Unemployment Rate (%)")+
   labs(title="Unemployment Rates (2013)",x="Longitude",y="Latitude")+
   theme(legend.position="bottom")

ggsave("unemploy.png", p, width=7, height=4.2, units="in")


