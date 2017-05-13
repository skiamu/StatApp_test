library(ggplot2)
library(maps)
library(readr)

# Update these to plot different indicators
# indicatorName <- "Life expectancy at birth, total (years)"
indicatorName <- "Population ages 65 and above (% of total)"
indicatorYear <- 2010

# read the dataframe indicators
# indicators <- read_csv("/home/andrea/StatApp/StatApp_test/Indicators.csv")

# extract the indicator and the year specified
filtered <- indicators[indicators$IndicatorName==indicatorName & indicators$Year==indicatorYear,]
# write_csv(filtered, "filtered.csv")

# shorten countries' name
correction <- c("Antigua and Barbuda"="Antigua", "Bahamas, The"="Bahamas", "Brunei Darussalam"="Brunei", "Cabo Verde"="Cape Verde", "Congo, Dem. Rep."="Democratic Republic of the Congo", "Congo, Rep."="Republic of Congo", "Cote d'Ivoire"="Ivory Coast", "Egypt, Arab Rep."="Egypt", "Faeroe Islands"="Faroe Islands", "Gambia, The"="Gambia", "Iran, Islamic Rep."="Iran", "Korea, Dem. Rep."="North Korea", "Korea, Rep."="South Korea", "Kyrgyz Republic"="Kyrgyzstan", "Lao PDR"="Laos", "Macedonia, FYR"="Macedonia", "Micronesia, Fed. Sts."="Micronesia", "Russian Federation"="Russia", "Slovak Republic"="Slovakia", "St. Lucia"="Saint Lucia", "St. Martin (French part)"="Saint Martin", "St. Vincent and the Grenadines"="Saint Vincent", "Syrian Arab Republic"="Syria", "Trinidad and Tobago"="Trinidad", "United Kingdom"="UK", "United States"="USA", "Venezuela, RB"="Venezuela", "Virgin Islands (U.S.)"="Virgin Islands", "Yemen, Rep."="Yemen")

for (c in names((correction))) {
  filtered[filtered$CountryName==c,"CountryName"] = correction[c]
}

# new dataframe for plotting
map.world <- merge(x=map_data(map="world"),
                   y=filtered[,c("CountryName","Value")],
                   by.x="region",
                   by.y="CountryName",
                   all.x=TRUE)
map.world <- map.world[order(map.world$order),]

p <- ggplot(map.world) +
  geom_map(map=map.world, aes(map_id=region,fill=Value)) + 
   expand_limits(x = map.world$long, y = map.world$lat) +
  scale_fill_gradient(low = "brown3", high = "green", guide = "colourbar") +
  coord_equal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.title=element_blank(),
        legend.text=element_text(size=9),
        legend.key.size = unit(1.5,"cm"),
        legend.position="bottom") +
  ggtitle(paste0(indicatorName, " in ", indicatorYear))
   

ggsave("ageing.png", p, width=7, height=4, units="in")
