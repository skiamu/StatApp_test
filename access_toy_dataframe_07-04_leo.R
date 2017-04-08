# Test how to manage efficiently a DataFrame


# ---- Intro #####
df = data.frame(
  NAT  = factor(c('ITA','USA','CHI','ITA','USA','CHI')),
  GDP  = c(10, 12, 13, 9, 11, 15),
  Year = c(2000,2000,2000,2001,2001,2001)
  )

df

# Access a columns 
df['Year']        # by its name
df[1]             # by its position
df[c(1,3)]
df[c('Year','NAT')]
df[2:3]

# Access rows
df[1,]
df[1:2,]
df[c(1,3),]

# Access subtables
df[1:2,c('NAT','Year')]

# Access by value (for our query)
df[df$NAT == 'ITA',]               # Access all the data with nation == ITA
df[df$Year == 2001,c('NAT','GDP')] # Access nations and GDP for year 2001
# extend with logical operators

# ---- Manage Dataframe #####
# which indicators are common to each state?

v_n = c('ITA','FRA','SPA','GER')
v_i = c('GDP','GDP','GDP','GDP',
        'C02','C02','C02','C02',
        'AGR','AGR','AGR','AGR')
v_y = c(2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
        2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001)
v_v = c(10,9,11,12,        .67,.87,.52,.45,        101,108,130,94,
        11,7,10,11,        .63,.90,.56,.42,        104,107,120,96)


df <- data.frame(
  NAT   = c(v_n,v_n,v_n,v_n,v_n,v_n),
  IND   = c(v_i,v_i),
  Year  = v_y,
  Value = v_v
)

df
summary(df)

# Select one year, drop the year column
df_2000 = df[df$Year==2000,-3]

# delete one row
df_2000 <- df_2000[-10,]
df_2000

library(reshape2)

# Indicator as columns
df_ind <- dcast(df_2000, NAT ~ IND)
df_ind

# Delete the columns with a NA value
#http://stackoverflow.com/questions/12454487/remove-columns-from-dataframe-where-some-of-values-are-na
df_ind1 <- df_ind[,colSums(is.na(df_ind))==0] 
df_ind1

# Delete the rows with a NA value
df_ind2 <- df_ind[rowSums(is.na(df_ind))==0,] 
df_ind2


# ---- Grouping Dataframe ####
library(dplyr)
library(readr)

df
df1 <- df[c(-10,-22),] # drop the AGR ind for all year for FRA
df1
# Group by nation
counts1 <- df1 %>%
  group_by(NAT) %>%
  summarise(NumIndicators = n_distinct(IND),
            NumYears      = n_distinct(Year),
            FirstYear     = min(Year),
            LastYear      = max(Year))
counts1
# for an indicator to be included in the count it has to happear at least one time
# so FRA has 2 indicators

# Group by indicators
counts2 <- df %>%
  group_by(IND) %>%
  summarise(NumCountries = n_distinct(NAT),
            NumYears     = n_distinct(Year),
            FirstYear    = min(Year),
            LastYear     = max(Year))
counts2

# filter using dyplr
test <- df %>%
  filter(Year==2000)

# ---- Plot a graph #####

df <- data.frame(
  NAT   = c('ITA','ITA','ITA','ITA','FRA','FRA','FRA','FRA'),
  Year  = c(2000,2001,2002,2003,2000,2001,2002,2003),
  Value = c(1,3,4,2,5,6,8,7)
)
df

library(ggplot2)

x11()
ggplot(df, aes(x = Year, y = Value, colour = NAT)) + geom_line()

df
df[df$Year==2000 & df$NAT=='FRA',]

# ---- Merge two table ####
df1 <- data.frame(
  NAT = c('A','B','C','A','B','C'),
  IND = c('I','I','I','J','J','J'),
  Val = c( 1 , 2 , 3 , 2 , 4 , 0 )
)
df1

df2 <- data.frame(
  NAT = c('A','B','C'),
  REG = c('X','Y','X')
)
df2

merge(df1,df2,by='NAT')

v <- unique(df2[,'REG'])


# ---- plot in a for cycle #####
df <- data.frame(
  NAT   = c('ITA','ITA','ITA','ITA','FRA','FRA','FRA','FRA'),
  Year  = c(2000,2001,2002,2003,2000,2001,2002,2003),
  Value = c(1,3,4,2,5,6,8,7)
)
df

v <- unique(df$NAT)

for (i in v){
  df1 <- df %>%
    filter(NAT==i)
  x11()
  # REM inside a for loop for the ggplot i need the print for the plot no
  print(ggplot(df1, aes(x = Year, y = Value, colour = NAT),
               main=i,
               xlab='Year',ylab="Value") + geom_line())
}

# ---- select from a dataframe with %in% ####
v_n = c('ITA','FRA','SPA','GER')
v_i = c('GDP','GDP','GDP','GDP',
        'C02','C02','C02','C02',
        'AGR','AGR','AGR','AGR')
v_y = c(2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
        2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001)
v_v = c(10,9,11,12,        .67,.87,.52,.45,        101,108,130,94,
        11,7,10,11,        .63,.90,.56,.42,        104,107,120,96)
df <- data.frame(
  NAT   = c(v_n,v_n,v_n,v_n,v_n,v_n),
  IND   = c(v_i,v_i),
  Year  = v_y,
  Value = v_v
)
df

nations <- data.frame(
  NAT   =c('ITA','FRA')
  )

df_fil <- df[df$NAT %in% nations$NAT,]
df_fil

df_fil <- df %>%
  filter(df$NAT %in%  nations$NAT)
df_fil

# ---- plot a dataframe with its mean ####
df <- data.frame(
  NAT   = c('ITA','FRA','ITA','FRA','ITA','FRA'),
  Year  = c(2000,2000,2001,2001,2002,2002),
  Value = c(7,8,9,6,5,7)
)
df

m <- df %>%
  group_by(Year) %>%
  summarise(Mean = mean(Value))
m

x11()
ggplot() +
  geom_line(data = df, aes(x = Year, y = Value, colour = NAT)) + 
  geom_line(data = m,  aes(x = Year, y = Mean),size = 4, linejoin = "round", lineend = "round")




# ---- intersect indicators ####

v_n = c('ITA','FRA','SPA','GER')
v_i = c('GDP','GDP','GDP','GDP',
        'C02','C02','C02','C02',
        'AGR','AGR','AGR','AGR')
v_y = c(2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,
        2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001,2001)
v_v = c(10,9,11,12,        .67,.87,.52,.45,        101,108,130,94,
        11,7,10,11,        .63,.90,.56,.42,        104,107,120,96)


df <- data.frame(
  NAT   = c(v_n,v_n,v_n,v_n,v_n,v_n),
  IND   = c(v_i,v_i),
  Year  = v_y,
  Value = v_v
)

df1 <- df[-10,]

dd <- df1 %>%
  filter(Year == 2000) %>%
  select(-Year) %>%
  dcast(NAT ~ IND) 
dd <- dd[,colSums(is.na(dd))==0]