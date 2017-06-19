# script per la regressione che vuole spiegare la crescita
path <- "/home/andrea/StatApp/StatApp_test"
load(paste(path,"ReadData/data.RData",sep = "/"))
load(paste(path,"inference/mcshapiro.test.RData",sep = "/"))
source(paste(path,"function_extract.R",sep = "/"))
source(paste(path,"inference/rep_measure.R",sep = "/"))
source(paste(path,"inference/Inference_functions.R",sep = "/"))
source(paste(path,"inference/test_functions.R",sep = "/"))
source(paste(path,"outlier.R",sep = "/"))
source(paste(path,"gaussianity/is_gaussian.R",sep = "/"))
source(paste(path,"Regression/lin_reg.R",sep = "/"))
# attenzione a questi due pacchetti che vanno in conflitto
library(MASS)
library(dplyr)
select <- dplyr::select 
###################### ESTRAZIONE DATASET #####################################

# specifico gli indicatori che in teoria dovrebbero entrare nella regressione
# con osservazione solo all'inizio del periodo
myInd <- c("Gross enrolment ratio, primary, both sexes (%)",
           "Life expectancy at birth, total (years)",
           "Fertility rate, total (births per woman)",
           "Inflation, GDP deflator (annual %)",
           "GDP per capita (constant LCU)",
           "Household final consumption expenditure, etc. (% of GDP)",
           "Foreign direct investment, net inflows (% of GDP)")
# regressori per cui ho bisogno delle osservazioni in tutto l'intervallo
myInd.continum <- c("General government final consumption expenditure (% of GDP)",
                    "Trade (% of GDP)")
# anni dell'analisi
myYear <- c(1980,1990,2000,2010)
myYear.continum <- 1980:2010
h <- 10
df1 <- getIndicators(myYear = myYear,
                     myInd = myInd,
                     agg = F,
                     ind = Indicators)
# non specifico gli anni prendo tutti quelli disponibili
df.continum <- getIndicators(#myYear = myYear.continum,
   myInd = myInd.continum,
   agg = F,
   ind = Indicators)

# data distribution: vedere la qualità dei regressosi scelti in termini
# di pienezza, eventualmnte prenderne altri
m <- Indicators %>% group_by(IndicatorName,Year) %>% 
   summarise(n = n_distinct(CountryCode))
avg <- m%>%group_by(IndicatorName)%>%summarise(avg = mean(n))%>%dplyr::select(avg)

M <- Indicators %>% group_by(IndicatorName) %>%
   summarise(n_year = n_distinct(Year),n_cnt = n_distinct(CountryCode),y_min = min(Year),
             y_max = max(Year)) 
M <- data.frame(M, avg = avg[,1])


############### COSTRUZIONE REGRESSORI DISCRETI E CONTINUI

###### 1) regressori discreti negli anni
df1 <- unifCnt(df1,showCnt = T, showInd = F)
X.discrete <- get3D(df1,myYear)
# nazioni per cui verrà fatta l'analisi
nazioni <- rownames(X.discrete[[1]])
n <- length(nazioni)

###### 2) regressori continui negli anni
# prendi solo le nazioni che sono sopravvisute al filtraggio congiunto su tutti
# i regressori discreti nel tempo
df.continum <- df.continum %>% group_by(CountryName) %>% 
   filter(CountryName %in% nazioni)
# le nazioni sui regressori discreti e sui quelli continui devono essere le stesse
if(!setequal(unique(df.continum$CountryName),nazioni)){
   stop("errore nel filtraggio delle nazioni")
}
# lista che conterrà i dataframe con le osservazioni dei regressori continui
# nel tempo per gli intervalli di tempo in considerazione
X.continum <- vector("list",length(myYear)-1)
# lista che conterrà i dataframe con le statistiche di qualita delle medie
#  dei regressori continui nel tempo per gli intervalli di tempo in considerazione
X.continum.stat <- vector("list",length(myYear)-1)
for(i in 1:(length(myYear)-1)){
   # inizializzo dataframe
   X <- stat_X <- data.frame(matrix(nrow = length(nazioni),ncol = length(myInd.continum)))
   rownames(X) <- nazioni
   colnames(X) <- myInd.continum;
   colnames(stat_X) <- myInd.continum
   # gli anni cu cui devo prendere la media devono essere tutti quelli presenti
   # nell'intervallo ad esempio [1975,1985] ossia fino ad un anno prima della fine
   # dell'intervallo
   Years <- myYear[i]:(myYear[i+1]-1)
   for(j in 1:(length(nazioni))){
      for(k in myInd.continum){
         # estraggo i valori del k-esimo indicatore continuo in tutti gli anni presenti
         # dentro l'intervallo specidicato in Years
         P <- df.continum %>% filter(CountryName == nazioni[j],IndicatorName == k) %>%
            filter(Year %in% Years) %>% select(CountryName,Value)
         X[j,k] <- mean(P$Value)
         # salvo su quante osservazioni si fa la media per capire quanto è 
         # affidabile il dato, ad esempio se fosse 0 sarebbe un problema perche
         # vorrebbe dire che in quell'intervallo non ho una osservazione per lo stato j
         # e quindi lo devo rimuovere dall'analisi
         stat_X[j,k] <- length(P$Value)
      }
   }
   # salvo nella lista per ogni intervallo temporale
   X.continum[[i]] <- X
   X.continum.stat[[i]] <- stat_X
}
# controllare l'affidabilità delle medie:
# se faccio la media su nessun anno ottengo un NA che va trattato mentre
# se ho slo anno nei 9 mi accontento
param_h <- 4
bad.avg <- vector("character")
for(i in 1:length(X.continum.stat)){
   for(k in myInd.continum){
      for(j in 1:n){
         if(X.continum.stat[[i]][j,k]<param_h){
            cat("ATTENZIONE: la media dell'indicatore ",k, " nel periodo ",i, " è fatta solo
                su ",X.continum.stat[[i]][j,k]," anni"," per lo stato ",nazioni[j],"\n")
            # se non solo il numero è basso ma è proprio 0, significa
            # che ho un NA e quindi devo tirare via quello stato
            if(X.continum.stat[[i]][j,k] == 0){
               bad.avg <- c(bad.avg,nazioni[j])
            }
         }
      }
   }
}
# alcuni potrebbero essere doppi
bad.avg <- unique(bad.avg)
# se ho stati dove la media è NA gli devo togliere
if(length(bad.avg) > 0){
   nazioni <- setdiff(nazioni,bad.avg)
   n <- length(nazioni)
   # devo toglierli anke da tutti i dataframe di X.discrete,
   # valutare se facendo prima questo passaggio e poi quello sulle discrete
   # mi cambia qualcosa (secondo me circa uguale). Qui devo fare due cicli
   # almtrimenti fa casino (bug da mezza giornata)
   for(i in 1:length(X.discrete)){
      X.discrete[[i]] <- X.discrete[[i]][nazioni,]
   }
   for(i in 1:length(X.continum)){
      X.continum[[i]] <- X.continum[[i]][nazioni,]
   }
   
   
}

# REMARK: X.discrete ha un elemento in più perche ha tutte le osservazioni
# anke nel 2005, a me serve solo quella del GDP per il calcolo della Y
# ma tutti gli stati ci devono essere con i dati anke nel 2005.
z <- X.continum[[1]]
z1 <- X.discrete[[1]]

######## CREAZIONE DELLE DUMMY REGIONE ###########
# in output ottengo ancora un dataframe
# nazione.Code <- Indicators %>% filter(CountryName %in% nazioni) %>% select(CountryCode) %>%
#    unique() 
# nazione.regione <- Country %>% filter(CountryCode %in% nazione.Code$CountryCode) %>%
#    select(Region)
# perche non me li mette in ordine?
nazione.income <- nazione.regione <- vector("character",n)
Code <- Indicators %>% filter(CountryName %in% nazioni) %>% select(CountryCode) %>%
   unique() 
region.income <- data.frame()
for(i in 1:n){
   t <- Country %>% filter(CountryCode == Code$CountryCode[i]) %>%
      select(Region,IncomeGroup)
   region.income <- rbind(region.income,t)
}
# nessuna distinzione tra OECD e non OECD
region.income[,2] <- gsub("\\:.*$", "", region.income$IncomeGroup)
# specificare le dummy che si vuole mettere
dummy.region <- c("Sub-Saharan Africa","East Asia & Pacific")
dummy.income <- c("High income","Low income")
R <- data.frame(matrix(0,nrow = n,ncol = length(dummy.region)))
I <- data.frame(matrix(0,nrow = n,ncol = length(dummy.income)))
for(i in 1:length(dummy.region)){
   for(j in 1:n){
      if(region.income[j,1] == dummy.region[i]){
         R[j,i] <- 1
      }
   }
   colnames(R)[i] <- paste("R",i,sep = "")
}
for(i in 1:length(dummy.income)){
   for(j in 1:n){
      if(region.income[j,2] == dummy.income[i]){
         I[j,i] <- 1
      }
   }
   colnames(I)[i] <- paste("I",i,sep = "")
}
rownames(R) <- rownames(I) <- nazioni

# assemblo le dummy con ogni dataframe in X.continum
for(i in 1:length(X.continum)){
   X.continum[[i]] <- cbind(X.continum[[i]],R,I)
}
z <- X.continum[[1]]
############### COSTRUZIONE VETTORE RISPOSTE
# calcolo i 3 vettori separatamente e poi li impilo
Y <- vector("double")
y <- vector("double",n)
y.name <- "GDP per capita (constant LCU)"
for(i in 1:(length(myYear)-1)){# ciclo sugli intervalli decennali
   for(j in 1:n){# ciclo sulle nazioni dentro ogni decennio
      y[j] <- (X.discrete[[i+1]][j,y.name] - X.discrete[[i]][j,y.name]) /
         X.discrete[[i]][j,y.name]
   }
   Y <- c(Y,y)
}  
if(length(Y) != (length(myYear)-1)*n){warning("errore costruzione Y")}



########## ASSEMBLAGGIO DATAFRAMES IN DESIGN MATRIX
# le liste se cui lavorare sono X.continum e X.discrete
X.list <- vector("list",length(myYear)-1)

# fare in modo che il nome delle colonne sia quello desiderato con i nomi
# degli indicatori nelle posizioni che si dersiderano
j1 <- length(myInd)
j2 <- length(myInd.continum)
for(i in 1:(length(myYear)-1)){
   X <- data.frame(matrix(nrow = n, ncol = j1 + j2))
   X.list[[i]] <- cbind(X.discrete[[i]],X.continum[[i]])
}
z2 <- X.list[[1]]

########## 1) incollo dataframe in verticale
# devo lavorare sulla lista X.list
X <- data.frame()
for(i in 1:length(X.list)){
   X <- rbind(X,X.list[[i]])
}
if(length(Y) != dim(X)[1]){stop("errore costruzione X")}

########## 2) creazione delle dummy indicatrici del periodo
# definisco di nuovo tutte le grandezze per evitare sovrascrizioni
j1 <- length(myInd)
j2 <- length(myInd.continum)
# n nodi formano n-1 intervalli
n.periodi <- length(myYear) - 1
n <- length(nazioni)
# se ho g gruppi allora avrò g-1 dummy
n.dummy <- n.periodi - 1
# inizializzazione vettore finale
XD <- data.frame(matrix(nrow = n*n.periodi, ncol = j1 + j2 + n.dummy))
D <- data.frame(matrix(0,nrow = n*n.periodi,ncol = n.dummy))
for(i in 1:n.dummy){
   colnames(D)[i] <- paste("D",i,sep = "")
   D[((i-1)*n+1):(i*n),i] <- rep(1,n)
}
# design matrix quasi finale
XD <- cbind(X,D)


######## 3) modifiche manuali: trasformazione regressori
# da cambiare se cambiano i regressori
colnames(XD) <- gsub("\\(.*$", "", colnames(XD))
colnames(XD) <- gsub("\\,.*$", "", colnames(XD))
name <- colnames(XD)

# prendo il log(GDP)
XD[,name[3]] <- log(XD[,name[3]])

# prendo 1/life_expectancy = mortality rate
XD[,name[7]] <- 1/XD[,name[7]]

colnames(XD)[1:9] <- c("Fertility","FDI","GDP","education","consumi","inflation",
                  "health","investment","openess")

if(sum(is.na(XD)) != 0){stop("XD contiene na, correggere immediatamente")}


#################### REGRESSION ANALYSIS #######################################
source(paste(path,"Regression/lin_reg.R",sep = "/"))
# create the regression formula
formula <- c("Y ~ ")
for(i in 1:dim(XD)[2]){
   formula <- paste(formula,colnames(XD)[i],sep = "+")
}
formula <- "Y ~ Fertility+FDI+GDP+education+consumi+inflation+health+investment+openess+R1+R2+I1+I2+D1+D2"

fit <- lin_reg(Y,
               XD,
               formula = formula,
               X0.new = NULL,
               interval = NULL,
               print.plot = F,
               print.result = T,
               print.band = F,
               pointwise = F,
               print.plot.DIAGN = F)

plot(fit$model)
XD <- find_outlier(data.frame(Y,XD),remove = T)
Y <- XD[,1];XD <- XD[,-1]
fit <- lin_reg(Y,
               XD,
               formula = formula,
               X0.new = NULL,
               interval = NULL,
               print.plot = F,
               print.result = T,
               print.band = F,
               pointwise = F,
               print.plot.DIAGN = F)
plot(fit$model)
# queste correzioni vanno bene solo per anni 1980:2010
XD <- XD[-c(47,159,156),]; Y <- Y[-c(47,159,156)]
fit <- lin_reg(Y,
               XD,
               formula = formula,
               X0.new = NULL,
               interval = NULL,
               print.plot = F,
               print.result = T,
               print.band = F,
               pointwise = F,
               print.plot.DIAGN = F)
plot(fit$model)
XD <- XD[-26,]; Y <- Y[-26]
fit <- lin_reg(Y,
               XD,
               formula = formula,
               X0.new = NULL,
               interval = NULL,
               print.plot = F,
               print.result = T,
               print.band = F,
               pointwise = F,
               print.plot.DIAGN = F)

fit1 <- lm(formula,data = XD)
z <- step(fit1)

library(ggplot2)
graphics.off()
for(i in 1:(length(myInd)+length(myInd.continum))){
   d <- data.frame(Y,XD[,i]); colnames(d) <- c("Y",colnames(XD)[i])
   pp1 <- ggplot(d,aes_string(x =colnames(d)[2] ,y = colnames(d)[1])) +
      geom_point()  + 
      geom_smooth(method = "lm") +
      ggtitle("GDP per capita growth (annual %)") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            legend.key.size = unit(1.5,"cm")) 
   print(pp1)
   Sys.sleep(3)
}



# # 3 stage regression and IV variables
# stateEq <- Y ~ education + health + D1 + D2
# envirEq <- Y ~ GDP + investment + openess + investment:openess + Fertility +
#    D1 + D2
# eqSystem <- list( state = stateEq, env = envirEq )
# fit1 <- systemfit( eqSystem, method = "3SLS", inst = list(~ health, ~ GDP + Fertility),
#                    data = data.frame(Y,XD))
# summary(fit1)
# 
# f <- Y ~ GDP + investment + openess + investment:openess + Fertility +
#    D1 + D2 + education + health
# summary(lm(f,data = data.frame(Y,XD)))
# summary(systemfit(list(f),method = "SUR",data = data.frame(Y,XD)))




