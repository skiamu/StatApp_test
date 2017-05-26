# comparing INDIA and USA on life expectancy

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(scales)
library(data.table)
comp_Country <- c("IND","USA") 

dev<-fread("../input/Indicators.csv")

countryIndex<-dev%>%filter(dev$CountryCode %in% comp_Country) %>% select(5,2,3,4,6)
countryIndex1<-countryIndex %>% select(1,2,4,5) %>% spread(key = IndicatorCode,value = Value)
titleformat1<-theme(plot.title = element_text(colour = "blue",size =16,face ="bold"))


#Life expectancy at birth 
Life_expectancy <- countryIndex1 %>% select(Year, CountryCode, one_of(c("SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN", "SP.DYN.LE00.IN")))
colnames(Life_expectancy)[3:5]<-c("Life_exp of female","Life_exp of male","Total")
Life_expectancy<-melt(Life_expectancy,id=c("Year","CountryCode"))

plot_1 <- ggplot(subset(Life_expectancy,!is.na(value)), aes(Year, value, fill = variable)) + 
  geom_bar(stat = "identity",position = "stack") +
  facet_grid (.~CountryCode) + 
  ylab("Life Expectancy(years)")+
  scale_fill_discrete(name = "age range") +
  ggtitle("Life Expectancy at Birth") + 
  theme(legend.justification = c(1,1), legend.position = c(1,.85), axis.title.x = element_blank()) +
  titleformat1
plot_1

# This plot suggests that there is a significant increase in life expectancy in both the countries.

#--------------------------------------------------------------------------------------------------------------------------------------#

#Percentage of value added to GDP by agriculture in india over a period of time.

agri  <- "IND"
agri1 <- dev%>%filter(dev$CountryCode %in% agri)  %>% select(5,2,3,4,6)
agri2 <- agri1 %>% select(1,2,4,5) %>% spread(key=IndicatorCode,value=Value)
titleformat2<-theme(plot.title = element_text(colour = "black",size =16,face ="bold"))

agri_GDP <- agri2 %>% select(Year, contains("NV.AGR.TOTL.ZS"))
colnames(agri_GDP)[2]<- "value"

plot_2<- ggplot(agri_GDP,aes(Year,value))+
  geom_line(size=0.8)+
  scale_y_continuous(breaks=seq(0,100,2))+
  ylab("% Of GDP")+
  ggtitle("Contribution of agriculture to GDP of India")+
  theme(axis.title.x=element_blank(),legend.position="none",legend.title=element_blank())+
  titleformat2
plot_2
# above plot gives an insight that the contribution of agriculture towards GDP has declined rapidly.

#--------------------------------------------------------------------------------------------------------------------------------------#

#comparing urban population of 3 different countries.

country <- c("ARB","IND","GBR")
country1<- dev%>%filter(dev$CountryCode %in% country) %>% select(5,2,3,4,6)
country2<-country1 %>% select(1,2,4,5) %>% spread(key = IndicatorCode,value = Value)
titleformat3<-theme(plot.title = element_text(colour = "brown",size =14,face ="bold"))

urban_population <- country2 %>% select(Year, CountryCode, contains("SP.URB.TOTL.IN.ZS"))
colnames(urban_population)[3] <- "value"

plot_3 <- ggplot(subset(urban_population,!is.na(value)), aes(Year, value, fill = CountryCode)) + 
  geom_bar(stat = "identity",position = "stack") +
  facet_grid (.~CountryCode) + 
  ylab("urban population")+
  scale_fill_discrete(name = "age range") +
  ggtitle("URBAN POPULATION") + 
  theme(legend.justification = c(1,1), legend.position = c(.8,.8), axis.title.x = element_blank()) +
  titleformat3
plot_3
#The plot concludes that the United kingdom has largest population living in urban areas.

#--------------------------------------------------------------------------------------------------------------------------------------#

#Electricity production from various sources in European union over a period of time.

e_production  <- "EUU"
e_production1 <- dev%>%filter(dev$CountryCode %in% e_production)  %>% select(5,2,3,4,6)
e_production2 <- e_production1 %>% select(1,2,4,5) %>% spread(key=IndicatorCode,value=Value)
titleformat4 <-theme(plot.title = element_text(colour = "black",size =16,face ="bold"))

e_prod_EURO <- e_production2 %>% select(Year, one_of(c("EG.ELC.COAL.ZS","EG.ELC.HYRO.ZS","EG.ELC.NGAS.ZS","EG.ELC.NUCL.ZS","EG.ELC.PETR.ZS")))
colnames(e_prod_EURO)[2:6]<-c("prod_by_coal","prod_by_hydro","prod_by_natural gas","prod_by_nuclear","prod_by_oil")
e_prod_EURO<-melt(e_prod_EURO,id=c("Year"))

##plotting the results
plot_4<- ggplot(e_prod_EURO,na.rm=T,aes(Year,value,col=variable))+
  geom_line(size=0.8) +
  facet_grid(.~variable) +
  scale_y_continuous(breaks=seq(0,100,2))+
  ylab("% OF Total Electricity Production")+
  ggtitle("Breakdown of Electricity Generation by Energy Source in European Union")+
  theme(axis.title.x=element_blank(),legend.justification=c(1,1),legend.position="none",legend.title=element_blank())+
  titleformat4
plot_4
#Above plot gives a clear understanding of production of electricity from various sources.

#--------------------------------------------------------------------------------------------------------------------------------------#

#comaparison of Mortality rate of India  with China

m_country <- c("IND","CHN")
m_country1 <- dev%>%filter(dev$CountryCode %in% m_country)  %>% select(5,2,3,4,6)
m_country2 <- m_country1 %>% select(1,2,4,5) %>% spread(key=IndicatorCode,value=Value)
titleformat5<-theme(plot.title = element_text(colour = "black",size =16,face ="bold"))

mortality_rate <- m_country2 %>% select(Year,CountryCode, one_of(c("SP.DYN.AMRT.FE","SP.DYN.AMRT.MA")))
colnames(mortality_rate)[3:4]<-c("mortality_female","mortality_male")
mortality_rate <- melt(mortality_rate,id=c("Year","CountryCode"))

#plot the result
plot_5<- ggplot(mortality_rate,na.rm=T,aes(Year,value,col=CountryCode))+
  geom_line(size=0.8) +
  facet_grid(.~variable) +
  ylab("Mortality rate per 1000 persons")+
  ggtitle("Mortality rate of male and female")+
  theme(axis.title.x=element_blank(),legend.justification=c(1,1),legend.position=c(1,1),legend.title=element_blank())+
  titleformat5
plot_5
#Mortality rate in both the countries have reduced however, rate is higher in INDIA when compared to CHINA



