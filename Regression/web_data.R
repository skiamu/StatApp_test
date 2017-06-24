# analyse web data: prediction from OECD
library(readr)
Web_data <- read_csv("~/StatApp/StatApp_test/Regression/EO95_LTB_23062017212229531.csv")
myYear <- c(2010,2020)
d1 <- Web_data %>% filter( Time == myYear[1],Variable == "GDP per capita in USA 2005 PPPs") %>%
   select(Country,Value)
d2 <- Web_data %>% filter( Time == myYear[2],Variable == "GDP per capita in USA 2005 PPPs") %>%
   select(Country,Value)
d <- cbind(d1,d2$Value)
colnames(d)[2:3] <- c("GDP.2010","GDP.2020")
d <- d %>% mutate(growth = (GDP.2020 - GDP.2010) / GDP.2010 )

inters <- intersect(d$Country,rownames(X0.new))
q <- d %>% filter(Country %in% inters)
s <- data.frame(my = pred[inters,1], web = q$growth, error = q$growth-pred[inters,1]) 
my.vs.web <- mean(s$error)
my.vs.web.ABS <- mean(abs(s$error))
print("######################################")
print("myprediction vs OECD predictions for [2010,2020]:")
cat("mean deviation = ", my.vs.web,"\n")
cat("mean absolute deviation = ",my.vs.web.ABS,"\n")
