########### MANOVA ONE-WAY ##########
#
#
# funzione per lo studio della MANOVA ONE-WAY
#
#
# INPUT: 
#       Y = dataframe della risposta, la risposta è vettoriale 
#       treat = fattore 1, stessa lunghezza di Y con livelli
#               per ogni osservazione
# 
# NOTE : manca da fare il check sull'ugluaglianza delle covarianze
# e mettere a posto l'ultimo plot
#
oneway_manova <- function(Y,
                          treat,
                          plot.result = T,
                          alpha = 0.05,
                          print.result = T){
   
   # dimensione della risposta
   p <- dim(Y)[2]
   # numerosità in ciascun gruppo (livello)
   ng <- table(treat)
   # numerosità campionaria
   n <- dim(Y)[1]
   # numero di livelli del fattore
   g <- length(levels(treat))
   
   ####### DATA EXPLORATION #################
   
   colori <- NULL
   for(i in 1:g){
      colori[which(treat == levels(treat)[i])] <- rainbow(g)[i]
   }
   if(plot.result){
      x11()
      pairs(Y, col = colori, pch=16)
   }
   # plotto la variabilità per ogni livello del fattore delle componenti
   # della risposta. Questo plot è da guardare trasversalmente osservando
   # se ci sono differenze significative tra box dello stesso colore, in quanto
   # sotto H0 queste differenze non sono statisticamente significative
   if(plot.result){
      x11()
      par(mfrow = c(1,g), las = 2)
      for(i in 1:g){
         idx <- which(treat == levels(treat)[i])
         boxplot(Y[idx,],main = levels(treat)[i], ylim = range(Y), col = rainbow(p))
      }
   }
   # plotto la variabilità per ogni componente della risposta tra i livelli
   # del fattore. Va guardato riquadro per riguadro, sotto H0 non c'è differenza
   # tra i box dentro un riquadro, immaginando di rifiutare H0 posso 
   # farmi un'ideadi quale componente provova il rifiuto individuando
   # qualle dove i box sono più diversi.
   if(plot.result){
      x11()
      par(mfrow = c(1,p), las = 2)
      for(i in 1:p){
         boxplot(Y[,i] ~ treat, main = colnames(Y)[i], ylim = range(Y), col = rainbow(g))
      }
   }
   
   ########## CHECK ASSUMPTIONS ################
   load("/home/andrea/StatApp/StatApp_test/inference/mcshapiro.test.RData")
   library(ggplot2)
   # controlla la normalità p-dimensionale della risposta
   Ps <- NULL
   for(i in 1:g){
      Ps <- c(Ps,mcshapiro.test(as.matrix(Y[which(treat == levels(treat)[i]),]))$p)
   }
   if(print.result){
      if(any(Ps < alpha)){
         warning("gaussianità non supportata dai dati")
      }
   }
   # controlla che la matrice di covarianza della risposta sia sta stessa
   # dentro ogni gruppo del fattore
   
   
   ########## MODEL FITTING ################
   
   # se rifiuto questo test ho evidenza per dire che il trattamento ha 
   # effetto sul vettore della risposta media
   fit <- manova(as.matrix(Y) ~ treat)
   print(summary.manova(fit,test="Wilks"))
   
   # Si vuole capire quale componente del vettore risposta ha causato
   # il rifiuto del test (se rifiuto c'è stato). Faccio p ANOVA
   print(summary.aov(fit))
   
   # Si vuole capire quale livello inflenza effettivamnte la media delle
   # componenti della risposta. Costruisco intervalli di confidenza
   # per le differenze tre medie per ogni compenente del vettore 
   # delle risposta
   
   # numero di differenze tra medie (g-1) * g / 2 per ogni componente
   k <- p*g*(g-1)/2
   # quantile della t student per Bonferroni CI
   qT <- qt(1-alpha/(2*k), n-g)
   # stima della matrice covarianza
   W <- summary.manova(fit)$SS$Residuals
   # grand mean
   M <- sapply(Y,mean)
   # matrice delle medie nei gruppi
   Mg <- matrix(nrow = g, ncol = p)
   for(i in 1:g){
      Mg[i,] <- sapply(Y[which(treat == levels(treat)[i]),], mean)
   }
   treat_lev <- levels(treat)
   CI <- list()
   for(i in 1:(g-1)){
      for(j in (i+1):g){
         inf <- Mg[i,]-Mg[j,] - qT * sqrt( diag(W)/(n-g) * (1/ng[i]+1/ng[j]) )
         sup <- Mg[i,]-Mg[j,] + qT * sqrt( diag(W)/(n-g) * (1/ng[i]+1/ng[j]) )
         CI[[paste0(treat_lev[i],"-",treat_lev[j])]] <- cbind(inf,sup)
      }
   }
   
   # if(plot.result){
   #    x11()
   #    par(mfrow = c(2,p), las = 2)
   #    for(i in 1:p){
   #       boxplot(Y[,i] ~ treat, main = colnames(Y)[i], ylim = range(Y), col = rainbow(g))
   #    }
   #    sp.name <- colnames(Y)
   #    for(k in 1:p){
   #       plot(c(1,g*(g-1)/2),ylim=range(Y), xlim=c(1,g), pch='', 
   #            xlab='pairs treat', ylab=paste('CI tau',k), 
   #            main=paste('CI tau',sp.name[k]))
   #       for(j in 1:(g-1)){
   #          for(q in (j+1):g){
   #             lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
   #             points(1, Mg[1,k]-Mg[2,k], pch=16); 
   #             points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
   #             points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);
   #          }
   #       }
   #       lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
   #       points(1, Mg[1,k]-Mg[2,k], pch=16); 
   #       points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
   #       points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
   #       lines (c(2,2), c(CI[[2]][k,1],CI[[2]][k,2])); 
   #       points(2, Mg[1,k]-Mg[3,k], pch=16);
   #       points(2, CI[[2]][k,1], col=rainbow(g)[3], pch=16); 
   #       points(2, CI[[2]][k,2], col=rainbow(g)[1], pch=16);
   #       lines (c(3,3), c(CI[[3]][k,1],CI[[3]][k,2])); 
   #       points(3, Mg[2,k]-Mg[3,k], pch=16);
   #       points(3, CI[[3]][k,1], col=rainbow(g)[3], pch=16); 
   #       points(3, CI[[3]][k,2], col=rainbow(g)[2], pch=16);  
   #       abline(h=0)
   #    }
   # }
   # 
   
   
   return(CI)
   
} # end function