############ ONE-WAY ANOVA ###################
#
# funzione per lo studio dell'ANOVA ONE-WAY

#
# INPUT: 
#       X = dataframe come le seguenti colonne
#           1) Y = vettore delle risposte
#           2) treat = factor vector della stessa lunghezza di Y 
#                      con i trattamenti somministrati alla stat unit
#                      per cui si è registrata la risposta
# 
# OUTPUT:
#       ICrange = dataframe con gli intervalli di confidenza per tutte
#                 le differenze tra i tau_i. Una volta dimostrato che il
#                 fattore ha effetto sulla risposta media mi serve per
#                 capire quale livello/i è/sono quelli determinanti
# 
# 
oneway_anova <- function(X,plot.result = T, alpha = 0.05, print.result = T){
   
   # estraggo dal dataframe vettore delle risposte e dei trattamenti
   Y <- X[,1]; treat <- X[,2]
   
   if(plot.result){
      x11()
      # quando faccio plot su vettori di fattori il default è un barplot, devo 
      # però fare attenzione a mettere come primo argomento il vettore coi fattori.
      # Faccio questo plot per avere un'idea qualitativa della variabilità
      # dentro ciascun gruppo, perchè vorrò poi vedere se questa è statisticamente
      # significativa oppure no
      plot(treat, Y, xlab='treat', col='blue', main='groups variability', las=2)
      # faccio osservazioni qualitative, qui solo in linea di massima
   }
   # This is a case of one-way ANOVA: one variable (Y) observed 
   # over g levels
   n       <- length(treat)      # total number of obs.
   ng      <- table(treat)       # number of obs. in each group
   treat_lev   <- levels(treat)      # levels of the treatment
   g       <- length(treat_lev)     # number of levels (i.e., of groups)
   
   ##### ASSUMPTION VERIFICATION  #####
   # lo posso fare a priori oppure a posteriori sui residui. Le hp da 
   # verificare sono le seguenti:
   # 1) normalità dentro ciascun gruppo
   # 2) uguaglianza delle g matrici di covarianza
   
   # verifico 1)
   Ps <- NULL
   for(i in 1:g){
      Ps <- c(Ps,shapiro.test(Y[ treat == treat_lev[i] ])$p)
   }
   if(any(Ps < alpha)){# se almeno un gruppo ha p.value sotto alpha
      # indici dei gruppi in cui l'ipotesi di gaussianità non è vera
      idx.noGauss <- which(Ps < alpha)
      for(i in 1:length(idx.noGauss)){
         warning("Gaussian hp not verified for groups: ", treat_lev[idx.noGauss[i]],
                 " p.value = ", Ps[idx.noGauss[i]])
      }
   }
   # verifico 2)
   # test of homogeneity of variances
   # H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
   # H1: there exist i,j s.t. sigma.i!=sigma.j
   # sono felice di accettare, come nel caso di gaussianità, l'ipotesi nulle è 
   # invertita rispetto al caso delle scoperte
   cov.test <- bartlett.test(Y, treat)
   if(cov.test$p.value < alpha){
      warning("same covariance hp not verified, p.value = ",cov.test$p.value )
   }
   
   ####### ANOVA FITTING ######
   fit <- aov(Y ~ treat)
   summary(fit)
   
   # se p.value basso significa che rifiutiamo H0 ossia c'è evidenza per
   # affermare che i trattementi hanno effetto sulla media della risposta.
   # Sono interessato a sapere quale tra i g trattamenti ha provocato il rifiuto
   # dell'hp nulla. Devo costruire intervalli di confidenza per la differenza 
   # tra medie nei gruppi
   
   # numero di differenze che posso costruire con un vettore a g componenti
   k <- g*(g-1)/2
   # grand mean
   Media   <- mean(Y)
   # la funzione tapply applica una funzione dentro ogni gruppo,
   # il primo argomento è la risposta il secondo il vettore coi fattori
   Mediag  <- tapply(Y, treat, mean)
   # lo posso anke prendere dal summari di aov
   SSres <- sum(residuals(fit)^2)
   # stima della varianza sigma^2
   S <- SSres/(n-g)
   
   ICrange <- data.frame(CI.name = character(), LB = double(), UB = double(),
                         stringsAsFactors=FALSE)
   count <- 0
   # costruisco intervalli con Bonferroni correction per ogni differenza.
   # Salvo tutto in un dataframe in ordine progressivo: quindi prima avrò
   # tutte le differenze con il primo trattamento e cosi via.
   for(i in 1:(g-1)) {
      for(j in (i+1):g) {
         row.name <- paste(treat_lev[i],"-",treat_lev[j])
         count <- count + 1
         # inserisco separatamente perche non posso fare un vettore misto
         ICrange[count,1] <- c(row.name)
         ICrange[count,2:3] <-
            c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                 sqrt( S * ( 1/ng[i] + 1/ng[j] )),
              Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                 sqrt( S * ( 1/ng[i] + 1/ng[j] )))
      }
   }
   # stampo gli intervalli di confidenza per ogni differenza delle medie
   if(print.result){
      for(i in 1:k){
         print(ICrange[i,1])
         print(as.numeric(ICrange[i,2:3]))
      }
   }
   if(plot.result){
      IC <- as.matrix(ICrange[,2:3])
      x11(width = 14, height = 7)
      par(mfrow=c(1,2))
      plot(treat, Y, xlab='treat', col = rainbow(6), las=2)
      h <- 1
      plot(c(1,g*(g-1)/2),range(IC), pch='',xlab='pairs treat', ylab='Conf. Int. tau')
      for(i in 1:(g-1)){
         for(j in (i+1):g) {
            lines (c(h,h), c(IC[h,1],IC[h,2]), col='grey55'); 
            points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
            points(h, IC[h,1], col=rainbow(6)[j], pch=16); 
            points(h, IC[h,2], col=rainbow(6)[i], pch=16); 
            h <- h+1
         }}
      abline(h=0)
   }
   
   # calcolo ora i p.value per test per tutte le differenze di medie
   # in modo univariato. Voglio vedere la differenza tra i p.value con t,
   # Bonferroni e pdr
   p <- NULL
   for(i in 1:(g-1)){
      for(j in (i+1):g){
         p <- cbind(p,
                    (1-pt(abs((Mediag[i]-Mediag[j]) /
                                 sqrt( S * ( 1/ng[i] + 1/ng[j] ) ) ), n-g))*2)
         
      }
   }
   if(plot.result){
      x11()
      plot(1:k, p, ylim=c(0,1), type='b', pch=16, col='grey55', xlab='pairs treat',
           main='P-values')
      abline(h=alpha, lty=2)
      # Bonferroni correction: voglio controllare globalmete questi intervalli
      # corregge il p-value secondo alcuni criteri
      p.bonf <- p.adjust(p, 'bonf')
      lines(1:k, p.bonf, col='blue', pch=16, type='b')
      # Correction according to the false discovery rate (Benjamini-Hockberg):
      # meno conservativo, cambi criterio nella funzione p.adjust
      q.fdr <- p.adjust(p, 'fdr')
      lines(1:k, q.fdr, col='red', pch=16, type='b')
      legend('topleft', c('Not corr.', 'Bonf.', 'BH'), col=c('grey55', 'blue', 'red'), pch=16)
   } 
   return(ICrange)
   
} # end function

############ TWO-WAY ANOVA ###############
#
# INPUT :
#       X = dataframe con le seguenti colonne
#           Y = vettore numerico delle risposte
#           F1 = vettore di fattori (fattore 1)
#           F2 = vettore di fattori (fattore 2)
#       plot.result = TRUE se si vogliono plot
#       print.result = TRUE se si voglioni i risultati numerici
#             
# OUTPUT:
#       IC_F1 = vettore con gli intervalli di confidenza delle differenze 
#               di medie nel primo fattore
#       C_F2 = vettore con gli intervalli di confidenza delle differenze 
#               di medie nel secondo fattore
#               
# OSS : i CI in output mi servono, una volta dimostrato che uno dei due fattori o 
#       entrambi sono significativi per la media della risposta, 
#       per capire effettivamente quale livelli di un fattore determina
#       un effetto significativo sulle risposta media
#       

twoway_anova <- function(X,plot.result = T, alpha = 0.05, print.result = T){
   # vettore delle risposte
   Y <- X[,1]
   # primo fattore
   F1 <- X[,2]
   # secondo fattore
   F2 <- X[,3]
   # numerosità del campione
   n <- length(Y)
   # numero osservazioni dentro ogni gruppo del primo fattore
   ng <- table(F1)
   # numero osservazioni dentro ogni gruppo del secondo fattore
   nb <- table(F2)
   # numero livelli primo fattore
   g <- length(levels(F1))
   # numero livelli secondo fattore
   b <- length(levels(F2))
   
   ##### ASSUMPTION VERIFICATION  #####
   # lo posso fare a priori oppure a posteriori sui residui. Le hp da 
   # verificare sono le seguenti:
   # 1) normalità dentro ciascun quadrato
   # 2) uguaglianza delle g matrici di covarianza
   Ps <- NULL
   Pcov <- NULL
   enough.data <- T
   for(i in 1:length(levels(F1))){
      for(j in 1:length(levels(F2))){
         if(length(Y[(F1 == levels(F1)[i]) * (F2 == levels(F2)[j])]) > 3)
            Ps <- c(Ps,shapiro.test(Y[ (F1 == levels(F1)[i]) * (F2 == levels(F2)[j])])$p)
         else{
            warning("non ho abbastanza dati nel quadrato ",levels(F1)[i],
                " di F1 ", levels(F2)[i]," di F2 per controllare gaussianità")
            enough.data <- F
         }
      }
   }
   if(any(Ps < alpha)){# se almeno un quadrato ha p.value sotto alpha
      # indici dei gruppi in cui l'ipotesi di gaussianità non è vera
      idx.noGauss <- which(Ps < alpha)
      for(i in 1:length(idx.noGauss)){
         warning("Gaussian hp not verified for groups: ", i,
                 " p.value = ", Ps[idx.noGauss[i]])
      }
   }
   
   # verifico 2)
   # test of homogeneity of variances
   # H0: sigma.1 = sigma.2 = sigma.3 = sigma.4 = sigma.5 = sigma.6 
   # H1: there exist i,j s.t. sigma.i!=sigma.j
   # sono felice di accettare, come nel caso di gaussianità, l'ipotesi nulle è 
   # invertita rispetto al caso delle scoperte
   if(enough.data){
      for(i in 1:length(levels(F1))){
         p.b <- bartlett.test(Y[F1 == levels(F1)[i]],levels(F2))
         Pcov <- c(Pcov,p.b$p.value)
      }
   }
   if(any(Pcov < alpha)){
      warning("non posso assumere sigma uguali")
   }
   
   
   ###### fitting the models #############
   
   # comincio con il modello completo per vedere se interazione significativa
   fit.aov2.int <- aov(Y ~ F1 + F2 + F1:F2)
   print(summary.aov(fit.aov2.int))
   x11()
   par(mfrow=c(2,1), las=2)
   interaction.plot(F1,F2,Y)
   interaction.plot(F2,F1,Y)
   # proseguo togliendo l'interazione
   fit.aov2.ad <- aov(Y ~ F1 + F2)
   print(summary.aov(fit.aov2.ad))
   # faccio un test simultaneo sui due fattori
   M <- mean(Y) # media intero campione
   Mg <- tapply(Y, F1, mean) # media dentro fattore 1
   Mb <- tapply(Y, F2, mean) # media dentro fattore 2
   SS_F1 <- sum(ng %*% (Mg - M)^2)              # or from the summary: 1.53    
   SS_F2 <- sum(nb %*% (Mb  - M)^2)              # or from the summary: 66.70
   SSres   <- sum((Y - M)^2) - (SS_F1+SS_F2)   # or from the summary: 16.37
   check <- abs(SSres - sum((fit.aov2.ad$residuals)^2)) < 0.01
   stopifnot(check)
   Ftot      <- ( (SS_F1 + SS_F2) / ((g-1)+(b-1)))/(SSres / (n-g-b+1))
   Ptot      <- 1 - pf(Ftot, (g-1)+(b-1), n-g-b+1) # attention to the dgf!
   if(print.result)
      cat("test simultaneo sui due  fattori, p.value = ", Ptot)
   
   # mi chiedo ora dentro i due fattori quale livello è il più significativo
   IC_F1 <- oneway_anova(data.frame(Y,F1),
                         plot.result = F,
                         print.result = F,
                         alpha = alpha )
   IC_F2 <- oneway_anova(data.frame(Y,F2),
                         plot.result = F,
                         print.result = F,
                         alpha = alpha)
   
   return(list(IC_F1,IC_F2))
   
   
   
}# end function 
