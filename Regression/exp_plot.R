# plot explanatory part
# we want to see the 10-y growth pattern over the year
# before running this script run exp1_script.R 

source(paste(path,"Regression/make_function.R",sep = "/"))
library(ggplot2)

####### plot example (China, USA)
####### 
# select indicators GDP per capita (used to compute growth)
GDP <- "GDP per capita (constant 2005 US$)"
z <- make.yy(GDP,NULL)
yy <- z$yy
region <- vector("character",dim(yy)[1])

# histogram iniziale
M <- data.frame(medie = rowMeans(yy), cnt = rownames(yy))
ss <- ggplot(M, aes(x=medie)) + geom_histogram(binwidth=0.1, fill="blue", colour="black") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))+
   ggtitle("average 10-year-growth over [1960,2014]")+
   xlab("average 10-year-growth") + ylab("number of countries")
ss



ggsave(filename = "growth_hist.png",plot = ss, path = paste(path,"/final_presentation",sep = "/"),
       width = 20, height = 10, units = "cm")

   # scale_fill_gradient("Count", low = "green", high = "red")
# let's see the different pattern for USA and China, China's been speeding us
# while the USA have been slowing down
yy.plot <- make.ggplot(yy)
cnt <- c("Japan","USA","UK","Germany")
yy.plot <- yy.plot %>% filter(Country %in% cnt)
p.convergence <- ggplot(yy.plot,aes(x = Year,y = Value, group = Country,colour = Country)) + 
   geom_line()  +
   geom_point()+ggtitle("The catch-up effect") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm")) + 
   scale_x_discrete(breaks=seq(1970, 2010, 10))+
   ylab("10-year-growth")
p.convergence
ggsave(filename = "convergence.png",plot = p.convergence, path = paste(path,"/final_presentation",sep = "/"),
       width = 20, height = 10, units = "cm")



###### plot for macro-region 
myAgg <- c("East Asia & Pacific (all income levels)",
           "Europe & Central Asia (all income levels)",
           "Latin America & Caribbean (all income levels)",
           "Middle East & North Africa (all income levels)",
           "North America",
           "Sub-Saharan Africa (all income levels)")

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
p.growth_region <- ggplot(yy.plot,aes(x = Year,y = Value, group = Country,colour = Country)) + 
   geom_line()  +
   geom_point() + ggtitle("10-year-growth ") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm")) + scale_x_discrete(breaks=seq(1970, 2010, 10))
p.growth_region
ggsave(filename = "growth_region.png",plot = p.growth_region, path = paste(path,"/final_presentation",sep = "/"),
       width = 20, height = 10, units = "cm")

# da questo modello devo trarre delle indicazioni su che modello costruire. Ad esempio
# tre rette per i tre gruppi e quelle dell'anno












myAgg <- c("High income" ,"Low income","Middle income")
z <- Indicators %>% filter(CountryName %in% myAgg,IndicatorName == GDP)
Year <- 1982:2014
# GDP <- "GDP per capita (current US$)"
GDP <- "GDP per capita (constant 2005 US$)"
z <- make.yy(GDP,myAgg,flag.my.agg = T,myYear = Year)
yy <- z$yy
d <- z$Ind.like
# plot the GDP over the year
p.GDP <- ggplot(d,aes(x = Year,y = Value,colour = CountryName)) + 
   geom_line()  +
   geom_point()
p.GDP

# plot 10-y growth
yy.plot <- make.ggplot(yy)
p.growth_income <- ggplot(yy.plot,aes(x = Year,y = Value, group = Country,colour = Country)) + 
   geom_line()  +
   geom_point() +ggtitle("10-year-growth") +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))+scale_x_discrete(breaks=seq(1982, 2014, 5))
p.growth_income
ggsave(filename = "growth_income.png",plot = p.growth_income , path = paste(path,"/final_presentation",sep = "/"),
       width = 20, height = 10, units = "cm")



