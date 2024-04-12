data <- read.csv2("Synthese_Pheno_20230724.csv")
# Nom unique des espèces
SP<-paste(data$Genus,data$Species)
SP
unique(SP)
# nombre d'espèces
Nbs=unique(SP)
length(Nbs)
# Filtrer les données pour n'afficher que l'espèce "Vouacapoua americana"
data_Vouacapoua_americana <- subset(data, Species == "americana")
data_Vouacapoua_americana
# Filtrer les données pour n'afficher que l'espèce "Symphonia globulifera"
data_Symphonia_globulifera <- subset(data, Species == "globulifera")
data_Symphonia_globulifera
#Filtrer les données pour n'afficher que les Clusiaceae
data_Clusiaceae <-subset(data, Family == "Clusiaceae")
data_Clusiaceae
# Noms unique des familles
SP<-paste(data$Family)
SP
unique(SP)
#Jeu de données Synthese_Pheno_20230724.csv
 data <- data.csv2
nrow(data)
#différentes variables
str(data)
install.packages("tidyverse")
library("tidyverse")
## Filtrer que les Symphonia globulifera
data %>%
filter(Genus=="Symphonia" & Species=="globulifera") %>%
select (Family:Species, X23.10.2020 : X23.01.2024) %>%
print()-> data_Symphonia
## Filtrer que les Goupia glabra
data %>%
  filter(Genus=="Goupia" & Species=="glabra") %>%
  select (Family:Species, X23.10.2020 : X23.01.2024) %>%
  print()-> data_Goupia
## Filtrer que les Pradosia cochlearia
data %>%
  filter(Genus=="Pradosia" & Species=="cochlearia") %>%
  select (Family:Species, X23.10.2020 : X23.01.2024) %>%
  print()-> data_Goupia_cochlearia
## Sélectionner des colonnes
data %>%
  select(Num_crown, Family:Species, Code.sp) %>%
  print() ->data_Goupia_cochlearia_2
