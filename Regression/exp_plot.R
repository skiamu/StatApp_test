# plot explanatory part
# we want to see the 10-y growth pattern over the year
# before running this script run exp1_script.R 

source(paste(path,"Regression/make_function.R",sep = "/"))


####### plot example (China, USA)
####### 
# select indicators GDP per capita (used to compute growth)
GDP <- "GDP per capita (constant 2005 US$)"
z <- make.yy(GDP,NULL)
yy <- z$yy
# let's see the different pattern for USA and China, China's been speeding us
# while the USA have been slowing down
yy.plot <- make.ggplot(yy)
cnt <- c("Australia","China","Indonesia","UK","Italy","India","Japan","Zimbawe")
yy.plot <- yy.plot %>% filter(Country %in% cnt)
p.growth <- ggplot(yy.plot,aes(x = Year,y = Value, group = Country,colour = Country)) + 
   geom_line()  +
   geom_point()
p.growth



###### plot for macro-region 
myAgg <- c("East Asia & Pacific (all income levels)",
           "Europe & Central Asia (all income levels)",
           "Latin America & Caribbean (all income levels)",
           "Middle East & North Africa (all income levels)",
           "North America" ,
           "Sub-Saharan Africa (all income levels)")
# GDP <- "GDP per capita (current US$)"
GDP <- "GDP per capita (constant 2005 US$)"
z <- make.yy(GDP,myAgg,flag.my.agg = T)
yy <- z$yy
d <- z$Ind.like
# plot the GDP over the year
p.GDP <- ggplot(d,aes(x = Year,y = Value,colour = CountryName)) + 
   geom_line()  +
   geom_point()
p.GDP

# plot 10-y growth
yy.plot <- make.ggplot(yy)
p.growth <- ggplot(yy.plot,aes(x = Year,y = Value, group = Country,colour = Country)) + 
   geom_line()  +
   geom_point()
p.growth







