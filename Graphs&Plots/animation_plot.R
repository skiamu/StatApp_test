#The script uses a couple of different indicators as proxies for 'gender equality'
#These plots show years-by year results

#Load libraries,
require(ggplot2)
require(animation) 

#Load data,
indicators = read.csv('Indicators.csv', header = T,stringsAsFactors = F) #Load the data


#subset only the relevant data, for speed
indicators <- subset.data.frame(indicators, (IndicatorCode == 'SG.GEN.PARL.ZS' | IndicatorCode == 'SG.VAW.REAS.ZS'))

#Correct some country names
indicators$CountryName[indicators$CountryName == 'Egypt, Arab Rep.'] = 'Egypt'
indicators$CountryName[indicators$CountryName == 'Congo, Dem. Rep.'] = 'Democratic Republic of the Congo'
indicators$CountryName[indicators$CountryName == 'Gambia, The'] = 'Gambia'
indicators$CountryName[indicators$CountryName == 'Macedonia, FYR'] = 'Macedonia'
indicators$CountryName[indicators$CountryName == 'Yemen, Rep.'] = 'Yemen'
indicators$CountryName[indicators$CountryName == 'Bahamas, The'] = 'Bahamas'
indicators$CountryName[indicators$CountryName == 'Venezuela, RB'] = 'Venezuela'
indicators$CountryName[indicators$CountryName == 'Korea, Rep.'] = 'South Korea'
indicators$CountryName[indicators$CountryName == 'Iran, Islamic Rep.'] = 'Iran'

#Create a 'Women who believe a husband is justified...' dataset
indicators_neg = indicators[indicators$IndicatorCode =='SG.VAW.REAS.ZS',]

#Create a 'Proportion of seats held by women in national parlia...' dataset,
indicators_pos = indicators[indicators$IndicatorCode == 'SG.GEN.PARL.ZS',]


#Load the map data, set color value to 0
s = map_data('world')
s$colour = -1

#create a list of years for use in both plots
years <-sort(c(min(indicators$Year):max(indicators$Year)))

saveGIF(
  for (yr in years ){
    print(yr)         # Show progress of script
    
    #Reset values
    #disabling this results in old results staying on the chart until they are replaced. this may be useful, but
    #if the data is not being updated regularly, this could over-represent old data. Use caution.
    s$colour = -1     
    
    #Subset only the relevant years data
    yrsubset <- subset(indicators_neg, Year == yr) 
    
    # set country color values to this year's numbers.
    i=1
    while (i <= length(yrsubset$CountryName)) {
      s$colour[s$region == yrsubset$CountryName[i]] = yrsubset[i,6]
      i=i+1
    }
    print(
      m <-  ggplot(s, 
                   aes(x=long, 
                       y=lat, 
                       group=group, 
                       fill=colour)) + 
        #Plot the Earth    
        geom_path(data = s,                                            
                  aes(x=long, y=lat, group=group), 
                  colour='black') + 
        scale_fill_gradientn(na.value = 'grey50',                           
                             colours=c('white','red','dark red'),
                             guide = 'colourbar',
                             limits=c(0,100))+ 
        geom_polygon(alpha=1, color = 'black')+                                          
        theme(plot.title = element_text(size = rel(2)),                  
              panel.background = element_rect(fill = 'lightskyblue2')) + 
        ggtitle(paste0('Women who believe a husband is justified in beating his wife (%)','\n',yr))
    )
  }
  , movie.name = 'World-Dev1.gif', interval = 0.5, convert = 'convert', ani.width = 1200, 
  ani.height = 900)

saveGIF(
  for (yr in years ){
    print(yr)         # Show progress of script
    
    #Reset values
    #disabling this results in old results staying on the chart until they are replaced. this may be useful, but
    #if the data is not being updated regularly, this could over-represent old data. Use caution.
    s$colour = -1   
    
    #Subset only the relevant years data
    yrsubset <- subset(indicators_pos, Year == yr) 
    
    # set country color values to this year's numbers.
    i=1
    while (i <= length(yrsubset$CountryName)) {
      s$colour[s$region == yrsubset$CountryName[i]] = yrsubset[i,6]
      i=i+1
    }
    print(
      m <-  ggplot(s, 
                   aes(x=long, 
                       y=lat, 
                       group=group, 
                       fill=colour)) +
        geom_path(data = s,                                        
                  aes(x=long, y=lat, group=group), 
                  colour='black') +
        scale_fill_gradientn(na.value = 'grey50',                           
                             colours=c('red','green','black'),
                             guide = 'colourbar',
                             limits=c(0,100))+
        geom_polygon(alpha=1, color = 'black')+                                           
        theme(plot.title = element_text(size = rel(2)),                  
              panel.background = element_rect(fill = 'lightskyblue2')) +     
        ggtitle(paste0('Proportion of seats held by women in national parliaments (%)','\n',yr))
    ) 
  }
  , movie.name = 'World-Dev2.gif', interval = 0.5, convert = 'convert', ani.width = 1200, 
  ani.height = 900)
