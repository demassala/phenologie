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

## phenologie
install.packages("pacman")
pacman::p_load("plotly","strucchange","timeSeries","lubridate","bfast","tidyverse",
               "data.table","ggplot2","ggfortify","zoo","readxl","readr","cluster",
               "stringr","bookdown","ggpubr","knitr","kableExtra","tibbletime","pracma",
               "imputeTS","TraMineR","clValid","cluster","FactoMineR","factoextra","dunn.test",
               "ggrepel")
# Source custom functions
source("Func_dataPrepExplo.R")
source("Func_analyse.R")
source("myTheme.R")
install.packages("tidyverse")
library("tidyverse")

##PHENOLOGIE
## Lecture du jeu de données
read_csv2("Synthese_Pheno_20230724.csv") ->
  pheno
# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno <- pheno[,-c(1,4)]
# Formatage des donnees
PrepPhase(pheno) -> pheno2 #Preparation des données brutes
# Formatage des colonnes
pheno2 = pheno2 %>% mutate(CrownID = as.factor(CrownID), # Pour être sure que ce soit considérer comme un facteur
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), # au cas-où il y a des NA mal écrits
                           PPVeg = as.factor(PPVeg), # Pour être sure que ce soit considérer comme un facteur
                           Update = as.Date(Update,format = "%d/%m/%Y")) # Pour être sure de la bonne date au bon format

# Ligne de code problematique
Leaf_Pattern(
  Data = filter(pheno2, Usable == 1) ,
  Obs_Veg = "PPVeg",
  Spec = "Symphonia_globulifera",
  fertility = TRUE
)[[2]]


