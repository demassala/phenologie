data <- read.csv2("Synthese_Pheno_24.csv")
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
 data <("Synthese_Pheno_24.csv")
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
library(tidyverse)

          ##PHENOLOGIE
## Lecture du jeu de données
data <- read.csv2("Synthese_Pheno_24.csv")
  pheno
# On ajuste en supprimant les colonnes qu'on veut garder (dans ton jeux de données adapté tu as deux colonnes factices encore)
pheno <- pheno[,-c(1,4)]

   ## Formatage des donnees
PrepPhase(pheno) -> pheno2
#Preparation des données brutes
# Formatage des colonnes
pheno2 = pheno2 %>% mutate(CrownID = as.factor(CrownID), # Pour être sure que ce soit considérer comme un facteur
                           PPVeg = str_replace_all(PPVeg,"(NA|Na|Na;|NA;)", NA_character_), # au cas-où il y a des NA mal écrits
                           PPVeg = as.factor(PPVeg), # Pour être sure que ce soit considérer comme un facteur
                           Update = as.Date(Update,format = "%d/%m/%Y")) # Pour être sure de la bonne date au bon format

#Differentes dates de floraison de Symphonia globufera
Leaf_Pattern(Data = filter(pheno2, Usable == 1) ,Obs_Veg = "PPVeg",
  Spec = "Symphonia_globulifera",fertility = TRUE)[[2]]
#Differentes dates de floraison de Vouacapoua americana
Leaf_Pattern(Data = filter(pheno2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Vouacapoua_americana",fertility = TRUE)[[2]]
#Differentes dates de floraison de Symphonia sp.1
Leaf_Pattern(Data = filter(pheno2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Symphonia_sp.1",fertility = TRUE)[[2]]
#Differentes dates de floraison de Sterculia_pruriens
Leaf_Pattern(Data = filter(pheno2, Usable == 1) ,Obs_Veg = "PPVeg",
             Spec = "Sterculia_pruriens",fertility = TRUE)[[2]]

# Les noms de tous les genres
data %>% select(Genus)  %>% pull() %>% unique()  %>% sort() %>% print()
# Les noms de toutes les especes
data %>% select(Genus,Species)  %>% pull() %>% unique()  %>% sort() %>% print()

# Floraison
GraphPropF <- LeafedOTim (Data=pheno2
                         Spec= "americana",
                         Pattern=c("Fl"),
                         Obs_Veg = "PPFlo")
GraphPropF[[2]]
install.packages("nlme")
library(nlme)

#tableau synthese pour le calcule des metriques

pheno <-read_csv2("Synthese_Pheno_20230724.csv")
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


               # Floraison du flux de travail
#Nous n’acceptons d’évaluer le flux de travail de floraison que si nous
#avons au moins trois événements de floraison par datechez tous les individus de l’espèce

              # premiers calculs
n_event_flo_spec = pheno2 %>%
  filter(Genus_Spec =="Symphonia_globulifera") %>%
  distinct(CrownID,date,PPFlo) %>%
  group_by(CrownID,PPFlo) %>%
  summarise(n = n()) %>% filter(PPFlo == "Fl") %>% ungroup() %>% pull(n) %>% sum()

              #Schéma général
Leaf_Pattern(Data =pheno2  %>% filter(Usable ==1), Obs_Veg ="PPVeg", Spec = "Symphonia_globulifera", fertility = TRUE)[[2]]

         #Temps des phénophases

          ##Temps de séjour
    # C’est la différence entre le début d’un événement et la fin du même événement
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Symphonia_globulifera",
       Obs_Veg = "PPFlo",
       markers = "Residence_time"
     )[[2]]

     ##Temps de séjour
     # C’est la différence entre le début d’un événement et la fin du même événement
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Symphonia_sp.1",
       Obs_Veg = "PPFlo",
       markers = "Residence_time"
     )[[2]]

     ##Temps de séjour
     # C’est la différence entre le début d’un événement et la fin du même événement
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Vouacapoua_americana",
       Obs_Veg = "PPFlo",
       markers = "Residence_time"
     )[[2]]

     ### Temps de retour
   ## C’est la différence entre la fin d’un événement et le début de l’événement suivant
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Symphonia_globulifera",
       Obs_Veg = "PPFlo",
       markers = "Return_time"
     )[[2]]

     ### Temps de retour
     ## C’est la différence entre la fin d’un événement et le début de l’événement suivant
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Symphonia_sp.1",
       Obs_Veg = "PPFlo",
       markers = "Return_time"
     )[[2]]

     ### Temps de retour
     ## C’est la différence entre la fin d’un événement et le début de l’événement suivant
     PhenoPhase_Time(
       Data = pheno2,
       Pattern = "Fl",
       Spec = "Vouacapoua_americana",
       Obs_Veg = "PPFlo",
       markers = "Return_time"
     )[[2]]


     ## Proportion d’individus dans une floraison par date
     GraphPropF <- LeafedOTim(Data=pheno2,
                              Spec= "Symphonia_globulifera",
                              Pattern=c("Fl"),
                           Obs_Veg = "PPFlo")
     GraphPropF[[2]]

## Proportion de phénophase
# par mois: Il s’agit du nombre d’un phénophase donné à chaque mois par an.Nous regroupons d’abord par espèce, par année et après mois et après nous
  calculons le nombre de phénophases d’événement ensuite, le nombre total de phénophases par mois et ensuite, la proportion de phénophases données.

   #Nous comptons donc : $$ \frac{\text{number of the given phenophases in a month}}{\text{all
       phenophases in the same month}}$$
# Si une personne a deux Fl obs au cours d’un même mois, nous comptons comme deux.

   Leaf_Circular(Data = pheno2, Spec = Symphonia_globulifera,Pattern = c("Fl"),
                 Obs_Veg = "PPFlo",perYears = FALSE)[[2]]

  # par mois et par année
   Leaf_Circular(Data = Full, Spec = WantedSpec,Pattern = c("Fl"),
                 Obs_Veg = "PPFlo",perYears = TRUE)[[2]]

 ## Proportion d’individus dans une phénophase donnée
   #Nous détectons le pic (début, max, fin) en fonction des paramètres et du signal traité (Moyenne mobile)

                              # signal

   data_signal = LeafedOTim(Data=pheno2 %>% filter(Usable==1),
                            Spec= "Symphonia_globulifera",
                            Pattern=c("Fl"),
                            Obs_Veg = "PPFlo")[[1]]

   signal = data_signal %>% select(prop) %>% pull()
   moyenne_mobile <- moving_average(data_signal %>% select(prop) %>%
                                      pull(),filter = fpoids(n=2,p=2,q=2)$y)
   #Vous pouvez régler le nombre de voisines pour le signal traité sur les paramètres "n"


   #   On indentifie les différents pics positif et négatif

   dates = data_signal %>% select(date) %>% pull() # Extracting the differents dates
   # the maximum of pics
   dates_max = sort(findpeaks(moyenne_mobile,minpeakheight  = 10,nups =1)[,2])
   # When the pics begin
   dates_begin =sort(findpeaks(moyenne_mobile,minpeakheight  = 10,nups = 1)[,3])
   # When the pics end
   dates_end = sort(findpeaks(moyenne_mobile,minpeakheight  = 10,nups = 1)[,4])
   # Percent of ind by peaks
   amplitude_peaks = findpeaks(moyenne_mobile,minpeakheight  = 10,nups = 1)[,1]

   #Il faut ensuite calculer l’amplitude réelle des crêtes
           déterminées sur le signal traité

           amplitude_real = signal[dates_max]

   ## Tracé du signal d'origine, de la moyenne mobile et de la dérivée avec ggplot2
           Plot = ggplot(data_signal, aes(x = date)) +
             geom_line(aes(y = prop, color = "original signal")) +
             geom_line(aes(y = moyenne_mobile, color = "processed signal"))+
             geom_point(aes(y = prop, color = "original signal")) +
             scale_color_manual(values = c("original signal" = "blue","processed signal" = "red"))+
             theme(axis.text.x = element_text(angle = 90),
                   panel.background = element_rect(fill = "white")) +
             geom_vline(xintercept = dates[dates_max],col = "tomato3" , linetype = "dashed") +
             geom_vline(xintercept = dates[dates_begin],col = "grey30", linetype = "dashed") +
             geom_vline(xintercept = dates[dates_end],col = "grey30", linetype = "dashed") +
             scale_x_date(date_breaks = "2 month", date_labels = "%b-%Y") +
             labs(title = "original and processed signal by Moving average", x = "Time", y = "Value") +
             annotate("text",x = dates[dates_max], y= 100,label = paste(round(amplitude_real,1),"%"),col = "grey40")

           Plot

           Leaf_Pattern(Data = Full %>% filter(Usable ==1), Obs_Veg ="PPVeg", Spec = WantedSpec, fertility = TRUE)[[2]] +    geom_vline(xintercept = dates[dates_max], col = "white" , linetype = "dashed") +
             geom_vline(xintercept = dates[dates_begin], col = "black" , linetype = "dashed") +
             geom_vline(xintercept = dates[dates_end], col = "black" , linetype = "dashed")

           summary_table = tibble(Genus_Spec = data_signal$Genus_Spec %>% unique(),
                                  max = dates[dates_max],
                                  range = abs(difftime(dates[dates_begin],dates[dates_end])),
                                  start = dates[dates_begin],
                                  end = dates[dates_end])kable(summary_table)



           #tableau synthese pour le calcule des metriques


           ## Filtrer que les Symphonia globulifera
            pheno2 %>%
             filter(Genus=="Symphonia" & Species=="globulifera") %>%
             select (Genus_Spec,date,PPFlo) %>%
             print()-> synthese_Sympho





           ## Métriques pour la floraison
  # Coefficient de variation:  Le coefficient de variation mesure la variabilité relative des dates de floraison par rapport à la moyenne
      # Supposons que vous avez un vecteur ou une colonne de dates de floraison dans votre dataframe appelé "dates_floraison"
     # Calcul du coefficient de variation (CV)
           cv_dates_floraison <- sd(dates_floraison) / mean(dates_floraison) * 100





