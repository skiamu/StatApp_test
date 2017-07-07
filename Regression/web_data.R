# analyse web data: prediction from OECD
library(readr)
library(ggplot2)
Web_data <- read_csv("~/StatApp/StatApp_test/Regression/EO95_LTB_23062017212229531.csv")

# 2010-2020
myYear <- c(2013,2023)
d1 <- Web_data %>% filter( Time == myYear[1],Variable == "GDP per capita in USA 2005 PPPs") %>%
   select(Country,Value)
d2 <- Web_data %>% filter( Time == myYear[2],Variable == "GDP per capita in USA 2005 PPPs") %>%
   select(Country,Value)
d <- cbind(d1,d2$Value)
colnames(d)[2:3] <- c("GDP.2010","GDP.2020")
d <- d %>% mutate(growth = (GDP.2020 - GDP.2010) / GDP.2010 )

# 2013-2023


inters <- intersect(d$Country,rownames(X0.new))
q <- d %>% filter(Country %in% inters)
s <- data.frame(my = pred[inters,1], web = q$growth, error = q$growth-pred[inters,1]) 
my.vs.web <- mean(s$error)
my.vs.web.ABS <- mean(abs(s$error))
print("######################################")
print("myprediction vs OECD predictions for [2010,2020]:")
cat("mean deviation = ", my.vs.web,"\n")
cat("mean absolute deviation = ",my.vs.web.ABS,"\n")



#### plot
my <- data.frame(pred = s$my, cnt = rownames(s),type = rep("OURS",dim(s)[1]))
web <- data.frame(pred = s$web, cnt = rownames(s),type = rep("OECD",dim(s)[1]))
d <- rbind(my,web)

# d <- ddply(d, "cnt", transform, label_y=cumsum(pred))
# d$label_y <- trunc(d$label_y*10^2)/10^2

pp <- ggplot(d, aes(x=cnt, y=pred, fill=type)) +
   geom_bar(stat="identity", width=0.5, position="dodge")+
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"),
         legend.key.size = unit(1.5,"cm"))+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   ggtitle("prediction Growth 2023")+
   xlab("Country") + ylab("Growth [2013,2023]")
pp

ggsave(filename = "OECD.png",plot =pp , path = paste(path,"/final_presentation",sep = "/"),
       width = 21, height = 10, units = "cm")












