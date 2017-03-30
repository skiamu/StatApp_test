######## lettura del dataset  #############

Country <- read.csv("Country.csv", header= T)
colnames(Country)
attach(Country)
# contiene dati qualitativi per ogni nazione

CountryNotes <- read.csv("CountryNotes.csv", header= T)
colnames(CountryNotes)
# contiene le informazioni tecnico su ogni nazioni, codifica nome
# e fonte dei dati ( non caricarlo)

# Footnotes <- read.csv("Footnotes.csv", header = T)
# boh non se ne capisce l'utilità (non caricarlo)

Indicators <- read.csv("Indicators.csv", header = T)
colnames(Indicators)
attach(Indicators)

Series <- read.csv("Series.csv", header = T)
colnames(Series)
attach(Series)
# descrive nel dettaglio tutti gli indicatori


# SeriesNotes <- read.csv("SeriesNotes.csv", header = T)
# colnames(SeriesNotes)
# cita la fonte degli indicatori


######### estrazione descrizione degli indici su file txt  ########

# connettersi al fine
# fileConn<-file("output.txt")

# write.table(Series[c(3,5)], fileConn, append = TRUE, quote = TRUE, sep = " ",
#            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
#            col.names = TRUE, qmethod = c("escape", "double"))

# chiudo il file
#close(fileConn)


########### dataset per gruppi ##############












