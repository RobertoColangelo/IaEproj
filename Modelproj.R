library(readr)
library(dplyr)
library(ggplot2)
library(MASS)
library(tidyverse)
library(corrplot)

#partendo dal dataset dei PIL completo dell'istat creiamo il df con Regione e rispettivo gdp nel 2019
gdp <- read_csv("DCCN_PILT_14112021165406467.csv")
colnames(gdp)[4] <- c("Tipo_aggregato")
gdp_2019 <- gdp %>%
  filter(TIME == 2019) %>%
  filter(Tipo_aggregato == "prodotto interno lordo ai prezzi di mercato") %>%
  filter(Valutazione == "prezzi correnti")
gdp_2019=gdp_2019[,-c(1,3,4,5,6,7,8,9,10,11,12,14,15)]
gdp_2019 = gdp_2019[-c(5,12,17,18,19,26,28,29,31,32),]
gdp_2019 <- gdp_2019 %>%
  mutate(gdp_Value = Value) %>%
  mutate(Regione = Territorio)
gdp_2019=gdp_2019[,-c(1,2)]


#plot gdp per regione
ggplot(gdp_2019,aes(x=Regione,y=gdp_Value,fill=Regione))+
  geom_bar(stat='identity',position='dodge')


#Partendo dal dataset ICT dell'Istat creiamo il df con le nuove variabili
res<- read_csv("DCSP_ICT_14112021171706711.csv")
res<-res[,-c(1,3,5,6,7,8,9,10,12,13)]
colnames(res)[2] <- c("Data")
res<-res%>%
  pivot_wider(names_from=Data, values_from=Value)
colnames(res)[1] <- c("Regione")
colnames(res)[2] <- c("Firms_with_broadband") #percentuale di imprese che hanno una connessione a banda larga
colnames(res)[3] <- c("Fixed_connection") #percentuale di imprese con banda larga fissa
colnames(res)[4] <- c("Mobile_connection") #percentuale di imprese che forniscono dispositivi e connessioni mobili ai dipendenti
colnames(res)[5] <- c("At_least_30_MB") #percentuale di imprese con velocità massima download di almeno 30mb/s
colnames(res)[6] <- c("At_least_100_MB") #percentuale di imprese con velocità massima download di almeno 100mb/s


#______________DATASET E MODELLO FIXED____________

#riorganizzazione dati in regioni per il dataset di speed test da reti fisse
fixed_data <- read_csv("fixed_tiles_all_nuts.csv")
df_fixed <- fixed_data %>%
  dplyr::select(name_latn, avg_d_kbps, avg_u_kbps, urbn_desc, urbn_type, mount_type, coast_type) %>%
  mutate(Regione = case_when(
    name_latn == "Firenze" ~ "Toscana",
    name_latn == "Lucca" ~ "Toscana",
    name_latn == "Prato" ~ "Toscana",
    name_latn == "Livorno" ~ "Toscana",
    name_latn == "Pisa" ~ "Toscana",
    name_latn == "Arezzo" ~ "Toscana",
    name_latn == "Siena" ~ "Toscana",
    name_latn == "Grosseto" ~ "Toscana",
    name_latn == "Pistoia" ~ "Toscana",
    name_latn == "Massa-Carrara" ~ "Toscana",
    name_latn == "Macerata" ~ "Marche",
    name_latn == "Ancona" ~ "Marche",
    name_latn == "Ascoli Piceno" ~ "Marche",
    name_latn == "Fermo" ~ "Marche",
    name_latn == "Pesaro e Urbino" ~ "Marche",
    name_latn == "Roma" ~ "Lazio",
    name_latn == "Rieti" ~ "Lazio",
    name_latn == "Viterbo" ~ "Lazio",
    name_latn == "Latina" ~ "Lazio",
    name_latn == "Frosinone" ~ "Lazio",
    name_latn == "Perugia" ~ "Umbria",
    name_latn == "Terni" ~ "Umbria",
    name_latn == "Piacenza" ~ "Emilia-Romagna",
    name_latn == "Parma" ~ "Emilia-Romagna",
    name_latn == "Reggio nell'Emilia" ~ "Emilia-Romagna",
    name_latn == "Modena" ~ "Emilia-Romagna",
    name_latn == "Bologna" ~ "Emilia-Romagna",
    name_latn == "Ferrara" ~ "Emilia-Romagna",
    name_latn == "Ravenna" ~ "Emilia-Romagna",
    name_latn == "ForlÃ¬-Cesena" ~ "Emilia-Romagna",
    name_latn == "Rimini" ~ "Emilia-Romagna",
    name_latn == "Verona" ~ "Veneto",
    name_latn == "Vicenza" ~ "Veneto",
    name_latn == "Belluno" ~ "Veneto",
    name_latn == "Treviso" ~ "Veneto",
    name_latn == "Venezia" ~ "Veneto",
    name_latn == "Padova" ~ "Veneto",
    name_latn == "Rovigo" ~ "Veneto",
    name_latn == "Udine" ~ "Friuli-Venezia Giulia",
    name_latn == "Pordenone" ~ "Friuli-Venezia Giulia",
    name_latn == "Gorizia" ~ "Friuli-Venezia Giulia",
    name_latn == "Trieste" ~ "Friuli-Venezia Giulia",
    name_latn == "Trento" ~ "Trentino Alto Adige / Südtirol",
    name_latn == "Bolzano-Bozen" ~ "Trentino Alto Adige / Südtirol",
    name_latn == "Sassari" ~ "Sardegna",
    name_latn == "Nuoro" ~ "Sardegna",
    name_latn == "Cagliari" ~ "Sardegna",
    name_latn == "Oristano" ~ "Sardegna",
    name_latn == "Trapani" ~ "Sicilia",
    name_latn == "Palermo" ~ "Sicilia",
    name_latn == "Messina" ~ "Sicilia",
    name_latn == "Agrigento" ~ "Sicilia",
    name_latn == "Caltanissetta" ~ "Sicilia",
    name_latn == "Enna" ~ "Sicilia",
    name_latn == "Catania" ~ "Sicilia",
    name_latn == "Ragusa" ~ "Sicilia",
    name_latn == "Siracusa" ~ "Sicilia",
    name_latn == "Cosenza" ~ "Calabria",
    name_latn == "Crotone" ~ "Calabria",
    name_latn == "Catanzaro" ~ "Calabria",
    name_latn == "Reggio di Calabria" ~ "Calabria",
    name_latn == "Vibo Valentia" ~ "Calabria",
    name_latn == "Potenza" ~ "Basilicata",
    name_latn == "Matera" ~ "Basilicata",
    name_latn == "Taranto" ~ "Puglia",
    name_latn == "Brindisi" ~ "Puglia",
    name_latn == "Lecce" ~ "Puglia",
    name_latn == "Foggia" ~ "Puglia",
    name_latn == "Bari" ~ "Puglia",
    name_latn == "Barletta-Andria-Trani" ~ "Puglia",
    name_latn == "Caserta" ~ "Campania",
    name_latn == "Benevento" ~ "Campania",
    name_latn == "Napoli" ~ "Campania",
    name_latn == "Avellino" ~ "Campania",
    name_latn == "Salerno" ~ "Campania",
    name_latn == "Campobasso" ~ "Molise",
    name_latn == "Isernia" ~ "Molise",
    name_latn == "Teramo" ~ "Abruzzo",
    name_latn == "Pescara" ~ "Abruzzo",
    name_latn == "Chieti" ~ "Abruzzo",
    name_latn == "L'Aquila" ~ "Abruzzo",
    name_latn == "Varese" ~ "Lombardia",
    name_latn == "Como" ~ "Lombardia",
    name_latn == "Lecco" ~ "Lombardia",
    name_latn == "Sondrio" ~ "Lombardia",
    name_latn == "Bergamo" ~ "Lombardia",
    name_latn == "Brescia" ~ "Lombardia",
    name_latn == "Pavia" ~ "Lombardia",
    name_latn == "Lodi" ~ "Lombardia",
    name_latn == "Cremona" ~ "Lombardia",
    name_latn == "Mantova" ~ "Lombardia",
    name_latn == "Milano" ~ "Lombardia",
    name_latn == "Monza e della Brianza" ~ "Lombardia",
    name_latn == "Imperia" ~ "Liguria",
    name_latn == "Savona" ~ "Liguria",
    name_latn == "Genova" ~ "Liguria",
    name_latn == "La Spezia" ~ "Liguria",
    name_latn == "Valle d'Aosta / Vallée d'Aoste" ~ "Valle d'Aosta / Vallée d'Aoste",
    name_latn == "Torino" ~ "Piemonte",
    name_latn == "Vercelli" ~ "Piemonte",
    name_latn == "Biella" ~ "Piemonte",
    name_latn == "Cuneo" ~ "Piemonte",
    name_latn == "Novara" ~ "Piemonte",
    name_latn == "Asti" ~ "Piemonte",
    name_latn == "Alessandria" ~ "Piemonte",
    name_latn == "Verbano-Cusio-Ossola" ~ "Piemonte",
))

ordered_na_percentages = colMeans(is.na(df_fixed))[order(-colMeans(is.na(df_fixed)))]
head(ordered_na_percentages, 20)
#no missing values


#creato il dataframe con gdp, regioni, e valore di up e down
df1 <- merge(x = df_fixed, y = gdp_2019, by = "Regione")

#dataframe con le medie di up and down in ogni regione con gdp
df_avg_fix = df1 %>%
  group_by(Regione, gdp_Value) %>%
  summarise(mean_d = mean(avg_d_kbps), mean_u = mean(avg_u_kbps))

#Uniamo anche le variabili del df res
df_avg_fix<-merge(x=df_avg_fix,y=res,by='Regione')

#correlazione tra variabili
df_avg_fix<-df_avg_fix[,-1] #escludiamo regioni
cor1<-cor(df_avg_fix)
corrplot(cor1, method="circle")

#modelli, linear regression & negative binomial regression
lm1=lm(df_avg_fix$gdp_Value~., data=df_avg_fix)
summary(lm1)

nb1=glm.nb(df_avg_fix$gdp_Value~., data=df_avg_fix)
summary(nb1)


#______________DATASET E MODELLO MOBILE____________

#riorganizzazione dati in regioni per il dataset di speed test da reti mobili
mobile_data <- read_csv("mobile_tiles_all_nuts.csv")
df_mobile <- mobile_data %>%
  dplyr::select(name_latn, avg_d_kbps, avg_u_kbps, urbn_desc, urbn_type, mount_type, coast_type) %>%
  mutate(Regione = case_when(
    name_latn == "Firenze" ~ "Toscana",
    name_latn == "Lucca" ~ "Toscana",
    name_latn == "Prato" ~ "Toscana",
    name_latn == "Livorno" ~ "Toscana",
    name_latn == "Pisa" ~ "Toscana",
    name_latn == "Arezzo" ~ "Toscana",
    name_latn == "Siena" ~ "Toscana",
    name_latn == "Grosseto" ~ "Toscana",
    name_latn == "Pistoia" ~ "Toscana",
    name_latn == "Massa-Carrara" ~ "Toscana",
    name_latn == "Macerata" ~ "Marche",
    name_latn == "Ancona" ~ "Marche",
    name_latn == "Ascoli Piceno" ~ "Marche",
    name_latn == "Fermo" ~ "Marche",
    name_latn == "Pesaro e Urbino" ~ "Marche",
    name_latn == "Roma" ~ "Lazio",
    name_latn == "Rieti" ~ "Lazio",
    name_latn == "Viterbo" ~ "Lazio",
    name_latn == "Latina" ~ "Lazio",
    name_latn == "Frosinone" ~ "Lazio",
    name_latn == "Perugia" ~ "Umbria",
    name_latn == "Terni" ~ "Umbria",
    name_latn == "Piacenza" ~ "Emilia-Romagna",
    name_latn == "Parma" ~ "Emilia-Romagna",
    name_latn == "Reggio nell'Emilia" ~ "Emilia-Romagna",
    name_latn == "Modena" ~ "Emilia-Romagna",
    name_latn == "Bologna" ~ "Emilia-Romagna",
    name_latn == "Ferrara" ~ "Emilia-Romagna",
    name_latn == "Ravenna" ~ "Emilia-Romagna",
    name_latn == "ForlÃ¬-Cesena" ~ "Emilia-Romagna",
    name_latn == "Rimini" ~ "Emilia-Romagna",
    name_latn == "Verona" ~ "Veneto",
    name_latn == "Vicenza" ~ "Veneto",
    name_latn == "Belluno" ~ "Veneto",
    name_latn == "Treviso" ~ "Veneto",
    name_latn == "Venezia" ~ "Veneto",
    name_latn == "Padova" ~ "Veneto",
    name_latn == "Rovigo" ~ "Veneto",
    name_latn == "Udine" ~ "Friuli-Venezia Giulia",
    name_latn == "Pordenone" ~ "Friuli-Venezia Giulia",
    name_latn == "Gorizia" ~ "Friuli-Venezia Giulia",
    name_latn == "Trieste" ~ "Friuli-Venezia Giulia",
    name_latn == "Trento" ~ "Trentino Alto Adige / Südtirol",
    name_latn == "Bolzano-Bozen" ~ "Trentino Alto Adige / Südtirol",
    name_latn == "Sassari" ~ "Sardegna",
    name_latn == "Nuoro" ~ "Sardegna",
    name_latn == "Cagliari" ~ "Sardegna",
    name_latn == "Oristano" ~ "Sardegna",
    name_latn == "Trapani" ~ "Sicilia",
    name_latn == "Palermo" ~ "Sicilia",
    name_latn == "Messina" ~ "Sicilia",
    name_latn == "Agrigento" ~ "Sicilia",
    name_latn == "Caltanissetta" ~ "Sicilia",
    name_latn == "Enna" ~ "Sicilia",
    name_latn == "Catania" ~ "Sicilia",
    name_latn == "Ragusa" ~ "Sicilia",
    name_latn == "Siracusa" ~ "Sicilia",
    name_latn == "Cosenza" ~ "Calabria",
    name_latn == "Crotone" ~ "Calabria",
    name_latn == "Catanzaro" ~ "Calabria",
    name_latn == "Reggio di Calabria" ~ "Calabria",
    name_latn == "Vibo Valentia" ~ "Calabria",
    name_latn == "Potenza" ~ "Basilicata",
    name_latn == "Matera" ~ "Basilicata",
    name_latn == "Taranto" ~ "Puglia",
    name_latn == "Brindisi" ~ "Puglia",
    name_latn == "Lecce" ~ "Puglia",
    name_latn == "Foggia" ~ "Puglia",
    name_latn == "Bari" ~ "Puglia",
    name_latn == "Barletta-Andria-Trani" ~ "Puglia",
    name_latn == "Caserta" ~ "Campania",
    name_latn == "Benevento" ~ "Campania",
    name_latn == "Napoli" ~ "Campania",
    name_latn == "Avellino" ~ "Campania",
    name_latn == "Salerno" ~ "Campania",
    name_latn == "Campobasso" ~ "Molise",
    name_latn == "Isernia" ~ "Molise",
    name_latn == "Teramo" ~ "Abruzzo",
    name_latn == "Pescara" ~ "Abruzzo",
    name_latn == "Chieti" ~ "Abruzzo",
    name_latn == "L'Aquila" ~ "Abruzzo",
    name_latn == "Varese" ~ "Lombardia",
    name_latn == "Como" ~ "Lombardia",
    name_latn == "Lecco" ~ "Lombardia",
    name_latn == "Sondrio" ~ "Lombardia",
    name_latn == "Bergamo" ~ "Lombardia",
    name_latn == "Brescia" ~ "Lombardia",
    name_latn == "Pavia" ~ "Lombardia",
    name_latn == "Lodi" ~ "Lombardia",
    name_latn == "Cremona" ~ "Lombardia",
    name_latn == "Mantova" ~ "Lombardia",
    name_latn == "Milano" ~ "Lombardia",
    name_latn == "Monza e della Brianza" ~ "Lombardia",
    name_latn == "Imperia" ~ "Liguria",
    name_latn == "Savona" ~ "Liguria",
    name_latn == "Genova" ~ "Liguria",
    name_latn == "La Spezia" ~ "Liguria",
    name_latn == "Valle d'Aosta / Vallée d'Aoste" ~ "Valle d'Aosta / Vallée d'Aoste",
    name_latn == "Torino" ~ "Piemonte",
    name_latn == "Vercelli" ~ "Piemonte",
    name_latn == "Biella" ~ "Piemonte",
    name_latn == "Cuneo" ~ "Piemonte",
    name_latn == "Novara" ~ "Piemonte",
    name_latn == "Asti" ~ "Piemonte",
    name_latn == "Alessandria" ~ "Piemonte",
    name_latn == "Verbano-Cusio-Ossola" ~ "Piemonte",
  ))

ordered_na_percentages = colMeans(is.na(df_mobile))[order(-colMeans(is.na(df_mobile)))]
head(ordered_na_percentages, 20)
#no missing values


#creato il dataframe con gdp, regioni, e valore di up e down
df2 <- merge(x = df_mobile, y = gdp_2019, by = "Regione")

#dataframe con le medie di up and down in ogni regione con gdp
df_avg_mob = df2 %>%
  group_by(Regione, gdp_Value) %>%
  summarise(mean_d = mean(avg_d_kbps), mean_u = mean(avg_u_kbps))

#Uniamo anche le variabili del df res
df_avg_mob<-merge(x=df_avg_mob,y=res,by='Regione')

#correlazione tra variabili
df_avg_mob<-df_avg_mob[,-1] #escludiamo regioni
cor2 <- cor(df_avg_mob)
corrplot(cor2, method="circle")

#modelli, linear regression & negative binomial regression
lm2=lm(df_avg_mob$gdp_Value~., data=df_avg_mob)
summary(lm2)

nb2=glm.nb(df_avg_mob$gdp_Value~., data=df_avg_mob)
summary(nb2)
