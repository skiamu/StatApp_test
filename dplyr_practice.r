###### Intro du dply ##########
library(dplyr)
library(nycflights13)

# dimensione del dataframe
dim(flights)

# nome delle colonne
colnames(flights)

head(flights)

# seleziono le righe del dataframe "flights" che hanno voli il 
# primo di gennaio.
# in plain R sarebbe stato:
# flights[flights$month == 1 & flights$day == 1, ]
filter(flights, month == 1, day == 1)

# oppure usando il pipe operator
flights %>% filter(month == 1, day == 1)

# il primo argomento è il nome del dataframe, i successivi sono i 
# parametri del filtraggio, sono tutti legati da un AND logico.
# per usare un OR posi fare nella seguente maniera
t <- filter(flights, month == 1 | month == 2, day == 1)


# per selezionare righe usando gli indici riga posso esare slice:
# supponiamo di volere le prima 10 righe del dataframe
t <- slice(flights,1:10)

head(t)

# oppure usando il pipe operator
flights %>% slice(1:10)

# le stesse operazioni possono essere fatte sulle colonne.
# Si supponga di avere un grande dataframe ma di volerlo ridurre 
# a solo 2 colonne (month e day)
t <- select(flights,month,day)

head(t)

# seleziona tutte le colonne tranne year e day
t <- flights %>% select(-(year),-(day))
head(t)

# estrarre i valori unici da un dataframe
t <- distinct(flights,dest)

# aggiungere nuove colonne ad un dataframe che sono funzione di quelle
# esistenti
t <- flights %>% 
     mutate(gain = arr_delay - dep_delay,speed = distance / air_time * 60) %>% 
     select(day,gain,speed)

head(t)

# se voglio tenere solamente le colonne appena create
t <- transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)
head(t)

# sintetizza il dataframe in una riga applicando una certa funzione
# sulle colonne indicate
summarise(flights, media = mean(distance),n = n_distinct(flight))

############## operazioni by groups ##############
# These verbs are useful on their own, but they become really powerful 
#when you apply them to groups of observations within a dataset. 
#In dplyr, you do this by with the group_by() function. It breaks down a 
#dataset into specified groups of rows. When you then apply the verbs above
#on the resulting object they’ll be automatically applied “by group”

# rinomina una colonna
rename(flights, tail_num = tailnum)

# raggruppo il dataframe per targa di aereo, tutte le operazioni fatte
# su by_targa saranno fatte a parità di targa
by_targa <- group_by(flights,tailnum)

delay <- summarise(by_targa,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))










