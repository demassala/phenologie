data <- read.csv2("Syn_Pheno_final.csv")
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

#Jeu de données Synthese_pheno_01.csv
 data <("Synthese_pheno_01.csv")
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
# Les noms de tous les genres
data %>% select(Genus)  %>% pull() %>% unique()  %>% sort() %>% print()
# Les noms de toutes les especes
data %>% select(Genus,Species)  %>% pull() %>% unique()  %>% sort() %>% print()
#tableau synthese pour le calcule des metriques

pheno <-read_csv2("Synthese_Pheno_223.csv")
pheno %>%
  select(Family,Species,Genus, `23/10/2020` : `23/01/2024`) %>%
  print () ->
  synthese_tab
#Pivotement du tableau synthese_tab en tableau long
synthese_tab %>%
  pivot_longer(
    cols =c(`23/10/2020` : `23/01/2024`),
    names_to = "date",
    values_to = "phenophases") %>%
  print()->
  synthese_tab1
## Filtrer que les Symphonia globulifera
synthese_tab1 %>%
  filter(Genus=="Symphonia" & Species=="globulifera") %>%
  select (Family,Species,Genus, date,phenophases) %>%
  print()-> synthese_Sympho




                     ###### PREMIERE PARTIE: pHENOLOGIE FLORALE ######

          #### ANALYSE DE LA PHENOLOGIE FLORALE DE QUELQUES ARBRES DE LA FORET GUYANAINE ####

  ## INSTALLATION DES PACKAGES NECESAIRES POUR L'EXECUTION DU CODE
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
library(tidyverse)

                     #### LECTURE DE LA BASE DE DONNEES ####

data <- read_csv2("Syn_Pheno_final.csv")
  data
# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
data <- data[,-c(1,4)]

# Formatage des donnees
PrepPhase(data) -> data2
#Preparation des données brutes
# Formatage des colonnes
data2 = data2 %>% mutate(CrownID = as.factor(CrownID), # Pour être sure que ce soit considérer comme un facteur
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), # au cas-où il y a des NA mal écrits
                           PPVeg = as.factor(PPVeg), # Pour être sure que ce soit considérer comme un facteur
                           Update = as.Date(Update,format = "%d/%m/%Y")) # Pour être sure de la bonne date au bon format


                 #### DIFFERENTES DATES DE FLORAISON DES SIX ESPECES CHOISI: SCHEMA DE FLORAISON GENERAL####

  ## Pour Symphonia globulifera
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
  Spec ="Symphonia_globulifera",fertility = TRUE)[[2]]

## Pour Symphonia sp.1
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Symphonia_sp.1",fertility = TRUE)[[2]]

## Pour Vouacapoua americana
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Vouacapoua_americana",fertility = TRUE)[[2]]

## Pour Couma guianensis
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Couma_guianensis",fertility = TRUE)[[2]]

## Pour Moronobea coccinea
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Moronobea_coccinea",fertility = TRUE)[[2]]

## Pour Platonia insignis
Leaf_Pattern(Data = filter(data2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Platonia_insignis",fertility = TRUE)[[2]]


                     ####  TEMPS DE SEJOUR DE LA FLORAISON ####
       ## DEFINITION :Correspond à la difference entre le debut et la fin d'un evenement de floraison) ##
                ### CALCUL DU TEMPS DE SEJOUR DE LA FLORAISON POUR LES SIX ESPECES CHOISI ###

## Pour Symphonia globulifera
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

## Pour  Symphonia.sp1
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

## Pour Vouacapoua americana
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

## Pour Couma guianensis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

## Pour Moronobea coccinea
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]

## Pour Platonia insignis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
  Obs_Veg = "PPFlo",
  markers = "Residence_time"
)[[2]]


                        ####  TEMPS DE RETOUR DE LA FLORAISON ####
## DEFINITION : temps entre la fin d'un evenement de floraison et le debut du prochain evenement de floraison)

                    ### CALCUL DU TEMPS DE RETOUR DE LA FLORAISON POUR LES SIX ESPECES CHOISI ###

## Pour Symphonia globulifera
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

## Pour Symphonia sp.1
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

## Pour Vouacapoua americana (Il n’y a pas assez de données pour le calcul d'un cycle du Vouacapoua_americana)
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

## Pour Couma guianensis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

## Pour Moronobea coccinea
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]

## Pour Platonia insignis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
  Obs_Veg = "PPFlo",
  markers = "Return_time"
)[[2]]


                                       #### TEMPS DES CYCLES ####
             ## DEFINITION : LE TEMPS DES CYCLES CORRESPOND A LA SOMME ENTRE LE TEMPS DE SEJOUR ET LE TEMPS DE RETOUR
              ### CALCUL DU TEMPS DES CYCLES DE LA FLORAISON POUR LES SIX ESPECES CHOISI ###

## Pour Symphonia globulifera
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_globulifera",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

## Pour Symphonia sp.1
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Symphonia_sp.1",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

## Pour Vouacapoua americana
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Vouacapoua_americana",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

## Pour Couma guianensis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Couma_guianensis",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

## Pour Moronobea coccinea
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Moronobea_coccinea",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]

## Pour Platonia insignis
PhenoPhase_Time(
  Data = data2,
  Pattern = "Fl",
  Spec = "Platonia_insignis",
  Obs_Veg = "PPFlo",
  markers = "Cycle_time"
)[[2]]


                        #### NOMBRE D'INVIVIDUS EN FLEUR PAR DATE ####

## Pour Symphonia globulifera
GraphPropF_globu <- LeafedOTim(Data=data2,
                               Spec = "Symphonia_globulifera",
                               Pattern = c("Fl"),
                               Obs_Veg = "PPFlo")
GraphPropF_globu[[2]]

## Pour Symphonia sp.1
GraphPropF_sp1 <- LeafedOTim(Data=data2,
                             Spec = "Symphonia_sp.1",
                             Pattern = c("Fl"),
                             Obs_Veg = "PPFlo")
GraphPropF_sp1[[2]]

## Pour Vouacapoua americana
GraphPropF_americana<- LeafedOTim(Data=data2,
                                  Spec = "Vouacapoua_americana",
                                  Pattern = c("Fl"),
                                  Obs_Veg = "PPFlo")
GraphPropF_americana[[2]]

## Pour Couma guianensis
GraphPropF_guianensis <- LeafedOTim(Data=data2,
                                    Spec= "Couma_guianensis",
                                    Pattern=c("Fl"),
                                    Obs_Veg = "PPFlo")
GraphPropF_guianensis[[2]]

## Pour Moronobea coccinea
GraphPropF_coccinea <- LeafedOTim(Data=data2,
                                  Spec= "Moronobea_coccinea",
                                  Pattern=c("Fl"),
                                  Obs_Veg = "PPFlo")
GraphPropF_coccinea[[2]]

## Pour Platonia insignis
GraphPropF_insignis <- LeafedOTim(Data=data2,
                                  Spec= "Platonia_insignis",
                                  Pattern=c("Fl"),
                                  Obs_Veg = "PPFlo")
GraphPropF_insignis[[2]]


               #### PROPORTION DE PHENOPHASE PAR MOI ####
# Il s’agit du nombre d’un phénophase donné à chaque mois par an.Nous regroupons d’abord par espèce, par année et après mois et après nouscalculons le nombre de phénophases d’événement ensuite, le nombre total de phénophases par mois et ensuite, la proportion de phénophases données.


# Pour Symphonia globulifera
Leaf_Circular(Data = data2,
              Spec = "Symphonia_globulifera",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

#Pour Symphonia sp1
Leaf_Circular(Data = data2,
              Spec = "Symphonia_sp.1",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = F)[[2]]

# Pour Vouacapoua americana
Leaf_Circular(Data = data2,
              Spec = "Vouacapoua_americana",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour Couma guianensis
Leaf_Circular(Data = data2,
              Spec = "Couma_guianensis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour Moronobea coccinea
Leaf_Circular(Data = data2,
              Spec = "Moronobea_coccinea",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

# Pour Platonia insignis
Leaf_Circular(Data = data2,
              Spec = "Platonia_insignis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = FALSE)[[2]]

                    ##### PROPORTION DE LA FLORAISON PAR MOIS ET PAR ANNEES #####

#Pour Symphonia globulifera
Leaf_Circular(Data = data2,
              Spec = "Symphonia_globulifera",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]

#Pour Symphonia sp.1
Leaf_Circular(Data = data2,
              Spec = "Symphonia_sp.1",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]

# Pour Vouacapoua americana
Leaf_Circular(Data = data2,
              Spec = "Vouacapoua_americana",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = TRUE)[[2]]

# Pour Couma guianensis
Leaf_Circular(Data = data2,
              Spec = "Couma_guianensis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]

# Pour Moronobea coccinea
Leaf_Circular(Data = data2,
              Spec = "Moronobea_coccinea",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]

# Pour Platonia insignis
Leaf_Circular(Data = data2,
              Spec = "Platonia_insignis",
              Pattern = c("Fl"),
              Obs_Veg = "PPFlo",
              perYears = T)[[2]]

#### Temporalité de la proportion d'individu en floraison ####
## Pour S.globulifera

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_globu = LeafedOTim(Data=data2 %>%
                                 filter(Usable==1),
                               Spec= "Symphonia_globulifera",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_globu = data_signal_globu %>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_globu = moving_average(data_signal_globu %>%
                                        select(prop) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_globu = data_signal_globu %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_globu = sort(findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_globu = findpeaks(moyenne_mobile_globu,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_globu = signal_globu[dates_max_globu]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2 ###

Plot = ggplot(data_signal_globu,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_globu,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_globu[dates_max_globu],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_begin_globu],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_globu[dates_end_globu],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Symphonia globulifera",
       x = "Time", y = "Value") +
  annotate("text",x = dates_globu[dates_max_globu],
           y= 100,label = paste(round(amplitude_real_globu,1),"%"),
           col = "grey40")
Plot


#### Temporalité de la proportion d'individu en floraison ####
## Pour Symphonia sp.1

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_sp = LeafedOTim(Data=data2 %>%
                                 filter(Usable==1),
                               Spec= "Symphonia_sp.1",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_sp = data_signal_sp%>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_sp = moving_average(data_signal_sp %>%
                                        select(prop) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_sp = data_signal_sp %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_sp = sort(findpeaks(moyenne_mobile_sp,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_sp = sort(findpeaks(moyenne_mobile_sp,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_sp = sort(findpeaks(moyenne_mobile_sp,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_sp = findpeaks(moyenne_mobile_sp,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_sp = signal_sp[dates_max_sp]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2

Plot = ggplot(data_signal_sp,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_sp,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_sp.1[dates_max_sp],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_sp[dates_begin_sp],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_sp.1[dates_end_sp],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Symphonia sp.1",
       x = "Time", y = "Value") +
  annotate("text",x = dates_sp[dates_max_sp],
           y= 100,label = paste(round(amplitude_real_sp,1),"%"),
           col = "grey40")
Plot


##### Temporalité de la proportion d'individu en floraison ####
## Pour Vouacapoua_americana

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_voua = LeafedOTim(Data=data2 %>%
                              filter(Usable==1),
                            Spec= "Vouacapoua_americana",
                            Pattern=c("Fl"),
                            Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_voua = data_signal_voua%>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_voua = moving_average(data_signal_voua%>%
                                     select(prop) %>%
                                     pull(),
                                   filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_voua= data_signal_voua %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_voua = sort(findpeaks(moyenne_mobile_voua,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_voua = sort(findpeaks(moyenne_mobile_voua,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_voua = sort(findpeaks(moyenne_mobile_voua,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_voua = findpeaks(moyenne_mobile_voua,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_voua = signal_voua[dates_max_voua]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2

Plot = ggplot(data_signal_voua,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_voua,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_ameri[dates_max_voua],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_voua[dates_begin_voua],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_ameri[dates_end_voua],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Vouacapoua_americana ",
       x = "Time", y = "Value") +
  annotate("text",x = dates_voua[dates_max_voua],
           y= 100,label = paste(round(amplitude_real_voua,1),"%"),
           col = "grey40")
Plot



##### Temporalité de la proportion d'individu en floraison ####
## Pour Couma_guianensis

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_Cou = LeafedOTim(Data=data2 %>%
                              filter(Usable==1),
                            Spec= "Couma_guianensis",
                            Pattern=c("Fl"),
                            Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_Cou = data_signal_Cou%>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_Cou = moving_average(data_signal_Cou %>%
                                     select(prop) %>%
                                     pull(),
                                   filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_Cou = data_signal_Cou %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_Cou = sort(findpeaks(moyenne_mobile_Cou,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_Cou = sort(findpeaks(moyenne_mobile_Cou,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_Cou = sort(findpeaks(moyenne_mobile_Cou,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_Cou = findpeaks(moyenne_mobile_Cou,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_Cou = signal_Cou[dates_max_Cou]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2

Plot = ggplot(data_signal_Cou,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_Cou,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_Cou[dates_max_Cou],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_Cou[dates_begin_Cou],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_Cou[dates_end_Cou],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Couma_guianensis",
       x = "Time", y = "Value") +
  annotate("text",x = dates_Cou[dates_max_Cou],
           y= 100,label = paste(round(amplitude_real_Cou,1),"%"),
           col = "grey40")
Plot


##### Temporalité de la proportion d'individu en floraison ####
## Pour Moronobea coccinea

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_Moro = LeafedOTim(Data=data2 %>%
                               filter(Usable==1),
                             Spec= "Moronobea_coccinea",
                             Pattern=c("Fl"),
                             Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_Moro = data_signal_Moro%>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_Moro = moving_average(data_signal_Moro %>%
                                      select(prop) %>%
                                      pull(),
                                    filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_Moro = data_signal_Moro  %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_Moro = sort(findpeaks(moyenne_mobile_Moro ,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_Moro= sort(findpeaks(moyenne_mobile_Moro ,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_Moro= sort(findpeaks(moyenne_mobile_Moro ,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_Moro  = findpeaks(moyenne_mobile_Moro ,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_Moro  = signal_Moro [dates_max_Moro ]

# Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2

Plot = ggplot(data_signal_Moro ,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_Moro ,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_Moro [dates_max_Moro ],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_Moro [dates_begin_Moro ],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_Moro [dates_end_Moro ],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Moronobea coccinea ",
       x = "Time", y = "Value") +
  annotate("text",x = dates_Moro [dates_max_Moro ],
           y= 100,label = paste(round(amplitude_real_Moro ,1),"%"),
           col = "grey40")
Plot


##### Temporalité de la proportion d'individu en floraison ####
## Pour Platonia insignis

# 1) Signal de floraison au cours du temps (nombre d'individu en fleur au cours du temps).
# (plot the number of each individual in the given State (Pattern) across sampling time.)
data_signal_Plato = LeafedOTim(Data=data2 %>%
                                 filter(Usable==1),
                               Spec= "Platonia_insignis",
                               Pattern=c("Fl"),
                               Obs_Veg = "PPFlo")[[1]]
# 2) Selection de la colonne prop (proportion) issue des donnees de signaux de floraison
signal_Plato = data_signal_Plato %>%
  select(prop) %>%
  pull() # pour extraire une seule colonne

# Calcul d'une nouvelle sequence de donnes avec moins de fluctuations temporelle
# grace a la technique de la moyenne mobile.
moyenne_mobile_Plato = moving_average(data_signal_Plato %>%
                                        select(prop) %>%
                                        pull(),
                                      filter = fpoids(n=2,p=2,q=2)$y)

#   On indentifie les différents pics positif et négatif de la floraison
dates_Plato = data_signal_Plato %>%
  select(date) %>%
  pull() # Extraction des différentes dates

# the maximum of pics
# sort () permet d'ordonner dans l'ordre croissant les elements d'un vecteur
# findpeaks () premet de trouver les pics de maximum de floraison
dates_max_Plato = sort(findpeaks(moyenne_mobile_Plato,minpeakheight  = 10,nups =1)[,2])

# When the pics begin
dates_begin_Plato = sort(findpeaks(moyenne_mobile_Plato,minpeakheight  = 10,nups = 1)[,3])

# When the pics end
dates_end_Plato = sort(findpeaks(moyenne_mobile_Plato,minpeakheight  = 10,nups = 1)[,4])

# Percent of ind by peaks
amplitude_peaks_Plato= findpeaks(moyenne_mobile_Plato,minpeakheight  = 10,nups = 1)[,1]

# Compilation des amplitudes relles des pics du signal
amplitude_real_Plato = signal_globu[dates_max_Plato ]

#### Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2 ###

Plot = ggplot(data_signal_Plato ,
              aes(x = date)) +
  geom_line(aes(y = prop,
                color = "original signal")) +
  geom_line(aes(y = moyenne_mobile_Plato ,
                color = "processed signal"))+
  geom_point(aes(y = prop,
                 color = "original signal")) +
  scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept = dates_Plato [dates_max_Plato ],
             col = "tomato3" ,
             linetype = "dashed") +
  geom_vline(xintercept = dates_Plato [dates_begin_Plato ],
             col = "grey30", linetype = "dashed") +
  geom_vline(xintercept = dates_Plato [dates_end_Plato ],
             col = "grey30", linetype = "dashed") +
  scale_x_date(date_breaks = "2 month",
               date_labels = "%b-%Y") +
  labs(title = "original and processed signal by Moving average for Platonia insignis",
       x = "Time", y = "Value") +
  annotate("text",x = dates_Plato [dates_max_Plato ],
           y= 100,label = paste(round(amplitude_real_Plato ,1),"%"),
           col = "grey40")
Plot






      ##### DEUXIEME PARTIE: RELATION DISTANCE ENTRE LES ARDRES ET LA FLORAISON #####

## INSTALLATION  ET CHARGEMENT DES PACKEGES ##
install.packages("geosphere")
install.packages("ggrepel")
install.packages("ggplot2")
install.packages("readxl")
install.packages("nlme")
install.packages("corrplot")
install.packages("readr")
install.packages("tidyverse")
library(geosphere)
library(ggplot2)
library(ggrepel)
library(readxl)
library(nlme)
library(corrplot)
library("tidyverse")
library(readr)


                ####  CORDONNEES GPS DES PIEDS DE Symphonia globulifera ####


distan <-read.csv2("Distance_sym.csv")

        #### Graphique de dispersion des coordonnées GPS de chaque pieds de Symphonia glo.####
P1 <- ggplot(distan, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Symphonia gl.",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P1)

 ### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_1<- as.matrix(distan[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance1 <- distm(Distan_1)
### Afficher la matrice de distances###
print(distance1)

#### CORRELATION ENTRE LES DIFFERENTES FLORAISONS ###
   ### POUR  Symphonia globulifera.###
Simple<-read_csv2("Symp_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Simple)

### CORRELAGTION DE LA FLORAISON DES Symphonia gl.###
# Restructurer les données pour une analyse de corrélation
floraison_wide <- Simple %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_Mat <- cor(floraison_wide[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_Mat)
# Visualiser la matrice de corrélation
corrplot(correlation_Mat, method = "circle")

#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance1, correlation_Mat)



                ###  CORDONNEES GPS DES PIEDS DE Couma_guianensis ####

dist_2<-read.csv2("Distance_Couma.csv")

#### Graphique de dispersion des coordonnées GPS de chaque pieds de Couma_guianensis###
P2 <- ggplot(dist_2, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Couma_guianensis",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P2)
### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_3<- as.matrix(dist_2[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance2 <- distm(Distan_3)
### Afficher la matrice de distances###
print(distance2)
#### CORRELATION ENTRE LES DIFFERENTES FLORAISONS ###
### POUR  Couma_guianensis ###
Couma<-read_csv2("Couma_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Couma)

### CORRELAGTION DE LA FLORAISON DES Couma_guianensis.###
# Restructurer les données pour une analyse de corrélation
floraison_couma<- Couma %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_couma <- cor(floraison_couma[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_couma)
# Visualiser la matrice de corrélation
corrplot(correlation_couma, method = "circle")
#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance2, correlation_couma)


               ###  CORDONNEES GPS DES PIEDS DE Moronobea coccinea ####

dist_3<-read.csv2("Distance_Morono.csv")
#### Graphique de dispersion des coordonnées GPS de chaque pieds de Moronobea coccinea###
P3 <- ggplot(dist_3, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Moronobea coccinea",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P3)
### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_4<- as.matrix(dist_3[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance3 <- distm(Distan_4)
### Afficher la matrice de distances###
print(distance3)


### CORRELATION ENTRE LES DIFFERENTES FLORAISONS ###
### POUR  Couma_guianensis ###
Morono<-read_csv2("Morono_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Morono)

### CORRELAGTION DE LA FLORAISON DES Moronobea coccinea###
# Restructurer les données pour une analyse de corrélation
floraison_Morono<- Morono%>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_Morono <- cor(floraison_Morono[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_Morono)
# Visualiser la matrice de corrélation
corrplot(correlation_Morono, method = "circle")
#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance3, correlation_Morono)


                ###  CORDONNEES GPS DES PIEDS DE Platonia insignis ###

dist_4<-read.csv2("Distance_Plato.csv")

#### Graphique de dispersion des coordonnées GPS de chaque pieds de Platonia insignis###
P4 <- ggplot(dist_3, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Platonia insignis",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P4)

### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_5<- as.matrix(dist_4[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance4 <- distm(Distan_5)
### Afficher la matrice de distances###
print(distance4)

### CORRELATION ENTRE LES DIFFERENTES FLORAISONS ###
### POUR  Platonia_insignis ###
Plato <-read_csv2("Plato_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Plato)

### CORRELAGTION DE LA FLORAISON DES Platonia insignis###
# Restructurer les données pour une analyse de corrélation
floraison_Plato<- Plato %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_Plato <- cor(floraison_Plato[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_Plato)
# Visualiser la matrice de corrélation
corrplot(correlation_Plato, method = "circle")
#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance4, correlation_Plato)


              ###  CORDONNEES GPS DES PIEDS DE Vouacapoua_americana ###

dist_4<-read.csv2("Distance_Vouaca.csv")

#### Graphique de dispersion des coordonnées GPS de chaque pieds de Platonia insignis###
P5 <- ggplot(dist_3, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Vouacapoua_americana",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P5)

### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_6<- as.matrix(dist_4[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance5 <- distm(Distan_6)
### Afficher la matrice de distances###
print(distance5)

### CORRELATION ENTRE LES DIFFERENTES FLORAISONS ###
### POUR  Vouacapoua_americana ###
Vouaca <-read_csv2("Vouaca_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Vouaca)
# Restructurer les données pour une analyse de corrélation
floraison_Vouaca<- Vouaca %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_Vouaca <- cor(floraison_Vouaca[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_Vouaca)
# Visualiser la matrice de corrélation
corrplot(correlation_Vouaca, method = "circle")
#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance5, correlation_Vouaca)


                 ###  CORDONNEES GPS DES PIEDS DE Symphonia sp.1 ###

dist_sp<-read.csv2("Distance_sp.1.csv")

#### Graphique de dispersion des coordonnées GPS de chaque pieds de Platonia insignis###
P6 <- ggplot(dist_sp, aes(x = X, y = Y)) +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(aes(label = arbres), vjust = -1, color = "black") +
  labs(title = "Dispersion des differents pieds de Symphonia sp.1",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Afficher le graphique
print(P6)

### CONVERTIR LES COORDONNEES GPS EN MATRICE ###
Distan_7<- as.matrix(dist_sp[, c("X", "Y")])
###CALCULER LA MATRICE DE DISTANCE###
distance6 <- distm(Distan_7)
### Afficher la matrice de distances###
print(distance6)

### CORRELATION DE LA FLORAISON DES Symphonia sp.1 ###
### POUR  Symphonia sp.1 ###
Sp.1 <-read_csv2("Sp.1_2024.csv")
# Afficher les premières lignes pour vérifier le chargement
head(Sp.1)

# Restructurer les données pour une analyse de corrélation
floraison_Sp.1 <- Sp.1 %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison)) %>%
  pivot_wider(names_from = Arbres, values_from = Floraison)
# Calculer les corrélations entre les arbres
correlation_Sp.1 <- cor(floraison_Sp.1[,-(1:2)], use="complete.obs")
# Afficher la matrice de corrélation
print(correlation_Sp.1)
# Visualiser la matrice de corrélation
corrplot(correlation_Sp.1, method = "circle")

#### PLOT MATRICE DISTANCE ARBRES ET MATRICE CORRELATION ####
plot(distance6, correlation_Sp.1)










            ##### VERIFION SI CHAQUE ARBRE A FLEURIT CHAQUE ANNEE SUR LES 4 ans #####

# Calculer le nombre de mois de floraison pour chaque arbre et chaque année
floraison_summary <- Simple %>%
  group_by(Arbres, Annee) %>%
  summarise(months_in_floraison = sum(Floraison), .groups = 'drop')
# Afficher les premières lignes pour vérifier
print(head(floraison_summary))
# Créer une table pivot pour visualiser la floraison par arbre et par année
floraison_pivot <- Simple %>%
  group_by(Arbres, Annee, Mois) %>%
  summarise(Floraison = mean(Floraison), .groups = 'drop') %>%
  pivot_wider(names_from = Mois, values_from = Floraison)
# Afficher la table pivot
print(floraison_pivot)
# Vérifier la continuité de la floraison pour chaque arbre
continuous_floraison <- floraison_pivot %>%
  rowwise() %>%
  mutate(continuous = all(c_across(1:12) == 1))
# Afficher les résultats pour chaque arbre
print(continuous_floraison)
# Créer une visualisation de la floraison pour chaque arbre
ggplot(Simple, aes(x = Mois, y = factor(Annee), fill = as.factor(Floraison))) +
  geom_tile(color = "white") +
  facet_wrap(~ Arbres, ncol = 5) +
  scale_fill_manual(values = c("0" = "red", "1" = "green"),
                    name = "Floraison",
                    labels = c("No", "Yes")) +
  labs(title = "Continuité de la Floraison des pieds de Symphonia gl. Arbres sur 4 ans",
       x = "Mois",
       y = "Année") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(1, "lines"))


# Calculer la proportion de floraison pour chaque mois et chaque année
floraison_summary <- Simple %>%
  group_by(Annee, Mois) %>%
  summarise(proportion_floraison = mean(Floraison)) %>%
  ungroup()
# Visualiser les proportions de floraison
ggplot(floraison_summary, aes(x=Mois, y=proportion_floraison, color=factor(Annee))) +
  geom_line(size = 1,) +
  geom_point() +
  labs(title="Proportion de Floraison par Mois et par Année des Symphonia gl.",
       x="Mois",
       y="Proportion de Floraison",
       color="Année") +
  theme_minimal()









































































































































































































































