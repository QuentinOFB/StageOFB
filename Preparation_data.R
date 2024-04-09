setwd("C:/Users/quentin.petit/Documents/Git/StageOFB")

library(lubridate)
library(dplyr)
library(tydyr)
library(stringr)
# Ouverture du jeu de données estuaire de la Loire : 
data <- read.csv("Data/Comptage_estuaire_2004_2024.csv", header = T, fileEncoding = "utf-8", sep = ";")
View(data)
str(data)

# Ouverture donnees Baie Aiguillon :  
Baie <- read.csv("Data/donnees_aiguillon.csv",header = T, fileEncoding = "utf-8", sep = ";")
View(Baie)
str(Baie)

# Ouverture donnees Camargue : 
Camargue <- read.csv("Data/donnees_camargue.csv",header = T,fileEncoding = "utf-8",sep = ";")
View(Camargue)
str(Camargue)

# Ouverture donnees cotentin : 
Cotentin <- read.csv("Data/donnees_cotentin.csv",header = T,fileEncoding = "utf-8",sep = ";")
View(Cotentin)
str(Cotentin)

### Unifier les noms des colonnes entre les différents tableaux de données : 

      ######### 1. Estuaire Loire : ###########

colnames(data)[5] <- "site"
colnames(data) [4] <- "secteur"
colnames(data) [6] <- "espece"
colnames(data) [7] <- "abondance"
colnames(data) [8] <- "obs" 

# -> Noms de colonnes (pas de point, pas d'espace, pas d'accents) : 
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ","_",colnames(data))
colnames(data) <- gsub("\\.","_", colnames(data))
colnames(data) <-iconv(colnames(data), from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Noms des espèces:
data[,6] <- tolower(data[,6])
data[,6] <- gsub(" ","_",data[,6])
data[,6] <- gsub("\\.","", data[,6])
data[,6] <- gsub("'","_",data[,6])
data[,6] <-iconv(data[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(data$espece))

# -> Remplacer chevalier combattant par combattant varié :

data[,6] <- gsub("chevalier_combattant","combattant_varie",data[,6])
data[,6] <- gsub("bernache_nonette","bernache_nonnette", data[,6])
data[,6] <- gsub("tournepierre","tournepierre_a_collier", data[,6])

# Forme des dates: regarder les formats (JJ/MM/YYYY)
class(data$date)
unique(data$date)
# Enlever les lignes qui ne continnent pas des dates : "comptage annulé + date) 
data <- subset(data, !(data$date=="Jeudi 15 mars 2018 comptage annulé"|data$date=="Comptage octobre 2013 : annulé cause de mauvais temps"
                       |data$date=="Comptage du 19 avril 2016 annulé condition climatique défavorable"|data$date=="Comptage du 19 mai 2016 annulé pas de pilote bateau"
                       |data$date=="Comptage du 20 juin 2016 annulé pas de pilote bateau"|data$date=="Vendredu 13 avril  2018 comptage annulé"
                      |data$date=="Comptage du lundi 11 juin 2018 annulé"|data$date=="Comptage du lundi 13 juin 2019 annulé"
                      |data$date=="Comptage du lundi 14 août 2019 annulé"))


data$date <- dmy(data$date)

#  Nom des sites :

unique(data$site)
data[,5] <- tolower(data[,5])
data[,5] <- gsub(" ","_",data[,5])
data[,5] <- gsub("-","_", data[,5])
data[,5] <- gsub("/","", data[,5])
data[,5] <-iconv(data[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Problèmes avec certains noms :

data[,5] <- gsub("paimboeuf___corsept","paimboeuf_corsept",data[,5])
data[,5] <- gsub("paimboeuf_corsept_","paimboeuf_corsept",data[,5])
data[,5] <- gsub("saint_brevin__mean","saint_brevin_mean",data[,5])
# Corsept et Paimboeuf_corsept sont les mêmes sites : 

data$site[data$site=="corsept"] <- "paimboeuf_corsept"


#  Nom du secteur : (enlever les majuscules)
unique(data$secteur)
data[,4] <- tolower(data[,4])

#Remplacement des cases vides par le nom du site :
data$secteur[data$secteur == ""] <- "estuaire"

# Uniformiser les noms des observateurs : ## Compliqué d'unifier les noms (des fois il y'a les initiales du prénom, des fois pas, plusieurs cas de figures)
data[,8] <- tolower(data[,8])
data[,8] <-iconv(data[,8], from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(data$obs)

#Création d'une colonne qualité comptage : ok et douteux 
data[,9] <- tolower(data[,9])
unique(data$remarques)
data$qualite_comptage <- with(data, ifelse(data$remarques=="comptage partiel",'douteux', ifelse(data$remarques=="compatge partiel, manque partie est de saint nicaols",'douteux',
                                                                                                ifelse(data$remarques=="houle, les compteurs ont dénombrés depuis le bateau donc comptage partiel",'douteux',
                                                                                                       ifelse(data$remarques==" comptage partiel",'douteux',
                                                                                                              ifelse(data$remarques=="pollution hydrocarbures du 16/03/08 (180tonnes) + hélicot survol",'douteux',
                                                                                                              ifelse(data$remarques=="dérangement : kite surf et chien non tenu en laisse",'douteux',
                                                                                                              ifelse(data$remarques=="comptage trop tardif par rapport à la marée",'douteux',
                                                                                                                    ifelse(data$date=="2021-08-20" & data$site =="saint_brevin",'douteux',
                                                                                                                            ifelse(data$date=="2017-09-19" & data$site=="migron",'douteux','ok'))))))))))
unique(data$qualite_comptage)


#Vérifier les abondances : 
unique(data$abondance) 
class(data$abondance)
# Enlever les NC (sites non comptés) à cause d'une remorque cassée, ou annulé pour cause de tir d'ibis la veille du comptage : 
data <- subset(data, !(data$abondance=="NC"|data$abondance=="nc"|data$abondance=="Nc"|data$abondance=="non compté"|data$abondance=="Non dénombré"|data$abondance=="w"))

# Supprimer les données agrégées entre Imperlay et Saint-Nicolas (une autre option serait d'agrégér Imperlay/St Nicolas pour toutes les dates ?)
 
data <- subset(data, !(data$date=="2017-01-10" & data$site=="imperlay"|data$date=="2017-01-10" & data$site=="saint_nicolas")) 
data <- subset(data, !(data$date=="2017-02-09" & data$site=="imperlay"|data$date=="2017-02-09" & data$site=="saint_nicolas")) 
data <- subset(data, !(data$date=="2022-01-20" & data$site=="imperlay"|data$date=="2022-02-20" & data$site=="saint_nicolas")) 

#Les erreurs d'entrée de données qui bloquent la création des tables inv et sites : 

# Lié au nom d'observateur (avec des espaces avant et après)
data[,8] <- gsub("guenezan m ","guenezan m",data[,8])
data[,8] <- gsub(" guenezan m","guenezan m",data[,8])

# Enlever les noms de sites absents : 
data <- subset(data, !(data$site==""))

# Colonne mois + années 
unique(data$mois)
data$mois <- month(data$date)

unique(data$annee)
data$annee <- year(data$date)

#Création d'une colonne site_retenu 
help("count")

nb_suivi_site <-  
  data %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

data <- merge(data,nb_suivi_site, by.x = "site", by.y = "site")

colnames(data)[18] <- "occurence_site"

data$site_retenu <- with(data, ifelse(data$occurence_site < 3,"non",
                                ifelse(data$site=="migron","non",
                                 ifelse(data$site=="baracon","non",
                                  ifelse(data$site=="estuaire","non","oui")))))  


  
# Enlever le comptage du 10/11/2008 : X2 passage sur tous les sites, avec des conditions météo nulles
# + comptage partiel -> remplacé par les comptages du 12/11 et du 24/11 : 
data <- subset(data,!(data$date=="2008-11-10"))

# Double comptage Corsept 18/07/2008 -> erreur de saisi des données, c'est un doublon !
data <- distinct(data)
help("distinct")

data <- subset(data,!(data$site=="saint_brevin"& data$date=="2008-07-18"& data$obs=="potiron"))

# Voir pour les doubles comptages avec Matthieu Bécot sur Pierre Rouge : 

##Selectionner les espèces limicoles et anatidés et enlever les autres : 
espece <- read.csv("Data/espece.csv")
sort(unique(espece$french_name)) 

View(espece)
espece[,5] <- tolower(espece[,5])
espece[,5] <- gsub(" ","_",espece[,5])
espece[,5] <- gsub("\\.","_",espece[,5])
espece[,5] <- gsub("é","e",espece[,5])
espece[,5] <- gsub("à","a",espece[,5])
espece[,5] <- gsub("'","_",espece[,5])
espece[,5] <-iconv(espece[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')
#Retirer la ligne de la sous-espèce de la bernache cravant (sinon ça dédouble les données pour cette espèce)
espece <- espece[-c(98),]

#Combinaison des deux tableaux : 
help("merge")
data_esp <- merge(data,espece, by.x = "espece", by.y = "french_name", all.x = TRUE)
View(data_esp)
unique(data_esp$espece)
unique(data_esp$order_tax)
unique(data$family_tax)
data_esp$order_tax[data_esp$espece=="barges_sp"] <- "Charadriiformes"
data_esp$order_tax[data_esp$espece=="becasseau_sp"] <- "Charadriiformes"
data_esp$order_tax[data_esp$espece=="canard_sp"] <- "Ansériformes"
data_esp$order_tax[data_esp$espece=="courlis_sp"] <- "Charadriiformes"
data_esp$order_tax[data_esp$espece=="gravelot_sp"] <- "Charadriiformes"

data_esp$family_tax[data_esp$espece=="barges_sp"] <- "Scolopacidés"
data_esp$family_tax[data_esp$espece=="becasseau_sp"] <- "Scolopacidés"
data_esp$family_tax[data_esp$espece=="canard_sp"] <- "Anatidés"
data_esp$family_tax[data_esp$espece=="courlis_sp"] <- "Scolopacidés"
data_esp$family_tax[data_esp$espece=="gravelot_sp"] <- "Charadriidés"

# Selectionnner les espèces qui nous intéressent : Anatidés + Limicoles
# -> Choix des Ansériformes + Charadriiformes

data <- subset(data_esp, data_esp$order_tax == "Ansériformes"|data_esp$order_tax == "Charadriiformes")
View(data)
unique(data$espece)

# Dégager les laridés + la sterne naine (Sternidés) :

data <- subset(data, !(data$family_tax=="Laridés"|data$family_tax=="Sternidés"))
unique(data$espece)
# -> 53 espèces anatidés/limicoles recensées sur tous les comptages estuaire Loire (+ 5 sp indeterminé)

#Ajouter une colonne pour le nombre d'observation : 

nb_observation <- data %>%
  count(espece)

data <- merge(data,nb_observation, by.x = "espece",by.y = "espece")
colnames(data)[34] <- "nb_observations"

#Valeur médiane des abondances 

data$abondance <- as.numeric(data$abondance)

median_ab <- data %>%
group_by(espece,mois,site,annee) %>%
summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

data$id_ab <- paste(data$espece,data$site,data$mois,data$annee)

data <- merge(data,median_ab,by.x = "id_ab",by.y="id")


## Retrier les colonnes qui ne servent pas à grand chose : 

data <- data[,-c(1,16,17,21,23:32,36:39)]

colnames(data)[1] <- "espece"
colnames(data)[2] <- "site"
colnames(data)[13] <- "mois"
colnames(data)[14] <- "annee"

data[,18] <- tolower(data[,18])
data[,18] <- gsub(" ","_",data[,18])

#Ajouter une colonne groupe fonctionnel : 

data$grp_fonctionnel <- with(data, ifelse(data$order_tax=="Charadriiformes","limicole","anatidae"))

#Ajouter colonne protocole 

data$protocole <- with(data, ifelse(data$site=="pierre_rouge","bateau",ifelse(
                                    data$site=="nord_cordemais","bateau", ifelse(data$site=="carnet","bateau",
                                              ifelse(data$site=="pipy","bateau",
                                                ifelse(data$site=="marechale","bateau",
                                                ifelse(data$site=="donges","bateau","terrestre")))))))
#Ajouter une colonne "Voie de migration 

data$voie_migr <- "est_atlantique"  


#Jeux de données ok (si rien n'a été oublié) pour estuaire de la Loire ! 


      ######## 2. La Camargue #############
## -> Pour le protocole, il s'agit bien d'un comptage mensuel réalisé par avion 
## -> Ecarte les 10 premières années en raison de changement de méthodologie 
## Les comptages sont effectués sur 150 polygones avec certains qui ont été rajouté au cours des suivis : 
Camargue <- read.csv("Data/donnees_camargue.csv",header = T,fileEncoding = "utf-8",sep = ";")
View(Camargue)
str(Camargue)

#Changer le nom des colonnes : 
colnames(Camargue)[2] <- "date"
colnames(Camargue) [4] <- "site"
colnames(Camargue) [5] <- "mois"
colnames(Camargue) [6] <- "annee"
colnames(Camargue) [7] <- "saison"
colnames(Camargue) [8] <- "espece" 
colnames(Camargue) [9] <- "abondance" 
colnames(Camargue) [10] <- "obs"
colnames(Camargue) [11] <- "niveau_eau"
colnames(Camargue) [12] <- "gel"

colnames(Camargue) <- tolower(colnames(Camargue))
colnames(Camargue) <-iconv(colnames(Camargue), from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Nom des espèces : 
Camargue[,8] <- tolower(Camargue[,8])
Camargue[,8] <-iconv(Camargue[,8], from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(Camargue$espece)

#Nom des sites : 
Camargue[,4] <- tolower(Camargue[,4])
Camargue[,4] <-iconv(Camargue[,4], from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(Camargue$site)

Camargue[,4] <- gsub("/","_",Camargue[,4])
Camargue[,4] <- gsub("-","_",Camargue[,4])

#Format de la date: 
Camargue$date <- dmy(Camargue$date)

#Observateur : 
Camargue[,10] <- tolower(Camargue[,10])
## Initiales des observateurs : AT = Alain Tamisier ; JBM = Jean-Baptiste Mouronval ; Michel Gauthier-Clerc 

# -> Avoir les noms vernaculaires des espèces : 
espece[,3] <- tolower(espece[,3])
espece[,3] <- gsub(" ","_",espece[,3])
# Prendre "scientific name 2 car il y a d'anciens noms latins dans le jeu de données Camargue : 
#  Mareca penelope et mareca strepera 
# Attention au nom latin du cygne de Bewick : 
Camargue[,8] <- gsub("cygnus_columbianus_bewickii","cygnus_columbianus",Camargue[,8])
Camargue <- merge(Camargue,espece, by.x = "espece", by.y = "scientific_name_2")
unique(Camargue$espece)
# -> Retirer la foulque macroule : 
Camargue <- subset(Camargue,!(Camargue$espece=="fulica_atra"))

#Retirer les colonnes dont on a pas besoin : 
Camargue <- Camargue[,-c(1,2,4,13,14,16,18:25)]

# Remettre les noms des obs et des colonnes au propre 
colnames(Camargue)[11] <- "espece"
Camargue[,11] <- tolower(Camargue[,11])
Camargue[,11] <- gsub(" ","_",Camargue[,11])

Camargue[,10] <- tolower(Camargue[,10])
Camargue[,10] <- gsub(" ","_",Camargue[,10])

#Création d'une colonne "secteur" pour la Camargue : 
Camargue$secteur <- "camargue"

#Sélection des années qui correspondent avec celle de l'estuaire de la Loire 
# à partir de 2004-2005 
# Sélectionner saison 2004 : 

Camargue <- subset(Camargue, !(Camargue$saison<2004))
unique(Camargue$date)

# NA Dans les abondances, pour le coups il s'agit de vrais NA, dans la mesure ou quand une espèce n'est pas comptée elle est notée
unique(Camargue$abondance)

#Faut-il retirer ces données NA ? 
Camargue <- subset(Camargue, !(Camargue$abondance=="NA"))

#Ajout colonne protocole : 
Camargue$protocole <- "avion"


      ######## 3. La Baie de l'Aiguillon #############
#Attention depuis 2020 seulement les sites sans effectifs sont saisis !!! 


Baie <- read.csv("Data/donnees_aiguillon.csv",header = T, fileEncoding = "utf-8", sep = ";")
View(Baie)
str(Baie)

#Changer le nom des colonnes : 
colnames(Baie)[3] <- "espece"
colnames(Baie)[5] <- "site"
colnames(Baie)[6] <- "obs"
colnames(Baie)[7] <- "observateur_det"
colnames(Baie)[8] <- "date"
colnames(Baie)[9] <- "confidentialite"
colnames(Baie)[10] <- "validation"
colnames(Baie)[11] <- "type_protocole"
colnames(Baie)[12] <- "abondance"
colnames(Baie)[13] <- "precision"
colnames(Baie)[14] <- "remarques"
colnames(Baie)[16] <- "id_habitat"
colnames(Baie)[17] <- "family_tax"
colnames(Baie)[18] <- "mois"
colnames(Baie)[19] <- "annee"

# Nom des colonnes : 
colnames(Baie) <- tolower(colnames(Baie))
colnames(Baie) <-iconv(colnames(Baie), from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Nom des especes : 
Baie[,3] <- tolower(Baie[,3])
Baie[,3] <- gsub(" ","_",Baie[,3])
Baie[,3] <- gsub("-","_",Baie[,3])
Baie[,3] <- gsub("'","_",Baie[,3])
Baie[,3] <- gsub("é","e",Baie[,3])
Baie[,3] <- gsub("î","i",Baie[,3])
Baie[,3] <- gsub("à","a",Baie[,3])
Baie[,3] <- gsub("ê","e",Baie[,3])
Baie[,3] <- gsub("â","a",Baie[,3])
Baie[,3] <- gsub("\\.","",Baie[,3])
Baie[,3] <-iconv(Baie[,3], from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(Baie$espece))

#Ne sélectionner que les anatidés/limicoles : 

Baie <- subset(Baie, Baie$family_tax=="Anatidae"|Baie$family_tax=="Charadriidae"|Baie$family_tax=="Scolopacidae"|
                 Baie$family_tax=="Recurvirostridae"|Baie$family_tax=="Haematopodidae"|Baie$family_tax=="Glareolidae")
unique(Baie$family_tax)

sort(unique(Baie$espece))
# règler les problèmes des noms d'especes : 
Baie[,3] <- gsub("chevalier_combattant,_combattant_varie","combattant_varie",Baie[,3])
Baie[,3] <- gsub("becasseau_rousset,_becasseau_roussatre","becasseau_rousset",Baie[,3])
Baie[,3] <- gsub("canard_a_front_blanc,_canard_d_amerique,_canard_siffleur_d_amerique","canard_a_front_blanc",Baie[,3])
Baie[,3] <- gsub("fuligule_a_bec_cercle,_fuligule_a_collier,_morillon_a_collier","fuligule_a_bec_cercle",Baie[,3])
Baie[,3] <- gsub("gravelot_a_collier_interrompu,_gravelot_de_kent","gravelot_a_collier_interrompu",Baie[,3])
Baie[,3] <- gsub("harelde_de_miquelon,_harelde_boreale","harelde_de_miquelon",Baie[,3])
Baie[,3] <- gsub("ouette_d_egypte,_oie_d_egypte","ouette_d_egypte",Baie[,3])
Baie[,3] <- gsub("petit_chevalier_a_pattes_jaunes,_chevalier_a_pattes_jaunes,_pattes_jaunes","chevalier_a_pattes_jaunes",Baie[,3])
Baie[,3] <- gsub("sarcelle_a_ailes_vertes,_sarcelle_de_la_caroline","sarcelle_a_ailes_vertes",Baie[,3])
Baie[,3] <- gsub("tadorne_casarca,_casarca_roux","tadorne_casarca",Baie[,3])
Baie[,3] <- gsub("tournepierre_a_collier,_pluvier_des_salines","tournepierre_a_collier",Baie[,3])

# retirer les espèces inderterminées : 
Baie <- subset(Baie, !(Baie$espece=="anatides_sp"|Baie$espece=="barge_sp"|Baie$espece=="canard_sp"|Baie$espece=="courlis_sp"|
                       Baie$espece=="macreuse_sp"|Baie$espece=="oie_sp"|Baie$espece=="becasses"|Baie$espece=="fuligule_sp"|Baie$espece=="limicole_sp"))

# retirer les hybrides : 
Baie <- subset(Baie, !(Baie$espece=="hybride_tadorne_de_casarca_x_belon"))

#noms des sites : 
unique(Baie$site)
Baie[,5] <- tolower(Baie[,5])
Baie[,5] <- gsub(" ","_",Baie[,5])
Baie[,5] <- gsub("'","_",Baie[,5])
Baie[,5] <- gsub("-","_",Baie[,5])
Baie[,5] <- gsub("à","a",Baie[,5])
Baie[,5] <- gsub("é","e",Baie[,5])
Baie[,5] <- gsub("è","e",Baie[,5])
Baie[,5] <- gsub("ç","c",Baie[,5])
Baie[,5] <-iconv(Baie[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')
# est-ce qu'il faut enlever les paranthèses ? 

# -> Ne sélectionner que les sites avec plus de 3 comptages sur plusieurs années : 
Baie <- subset(Baie,!(Baie$site=="communal_d_anais"|Baie$site=="communal_d_angliers"|Baie$site=="communal_de_courcon"|
                        Baie$site=="communal_de_langon"|Baie$site=="communal_de_sainte_gemme_la_plaine"|Baie$site=="communal_du_gue_d_allere"
                      |Baie$site=="etang_de_la_sabliere"|Baie$site=="la_dive"|Baie$site=="la_terriere"
                      |Baie$site=="lagunage_de_l_ile_d_elle"|Baie$site=="lagunage_de_saint_michel_en_l_herm"
                      |Baie$site=="marais_de_la_bretonniere"|Baie$site=="plan_d_eau_des_guifettes"
                      |Baie$site=="pointe_du_payre"))

Baie$site[Baie$site=="la_mare_a_2000"] <- "mare_a_2000"

#Format date 
unique(Baie$date) #de 1977 à 2024
Baie$date <- dmy(Baie$date)

#Nom des observateurs : 
Baie[,6] <- tolower(Baie[,6])
Baie[,6] <- iconv(Baie[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')


Baie[,6] <- gsub(" lpo 17","lpo 17",Baie[,6])

#Les différents protocoles : 
unique(Baie$type_protocole)
# -> Dans comptage simultané grues : 2 observations :  bécassine des marais + colvert 
Baie <- subset(Baie, !(Baie$type_protocole=="Comptage simultané grues"))

# Selection des années : 
Baie <- subset(Baie, !(Baie$annee<2004))
unique(Baie$date)
#Enlever Janvier, février, mars, avril, mai, juin, juillet et aout 2004
# Car le début du jeu de donnée estuaire correspond à la saison 2004-2005 (sept)

Baie <- subset(Baie, !(Baie$annee=="2004"& Baie$mois=="1"|Baie$annee=="2004"& Baie$mois=="2"|Baie$annee=="2004"& Baie$mois=="3"
                       |Baie$annee=="2004"& Baie$mois=="4"|Baie$annee=="2004"& Baie$mois=="5"|Baie$annee=="2004"& Baie$mois=="6"|Baie$annee=="2004"& Baie$mois=="7"
                       |Baie$annee=="2004"& Baie$mois=="8"))

#Sélection des mois qui nous intéressent, c'est à dire les mois où on voit les hivernants : 
Baie <- subset(Baie, !(Baie$mois=="5"|Baie$mois=="6"|Baie$mois=="7"|Baie$mois=="8"))

#Voir les abondance : 
unique(Baie$abondance)
unique(Baie$abondance[1000:1595])

# Ils ont mis les valeurs de comptage dans les remarques.... 



#Présence de NA : les supprimer ? (correspond à une "non prospection")
Baie <- subset(Baie,!(Baie$abondance=="NA"))

#Prendre en compte les remarques : 
unique(Baie$remarques)
Baie[,14]<- iconv(Baie[,14], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Baie[,14] <- gsub(" ","_",Baie[,14])
#Ajouter la colonne qualité du comptage : 

# Uniformisation des remarques (pour que ça passe avec ifelse) 

Baie$remarques[Baie$remarques=="Comptage_un_peu_galere_because_meteo._J'ai_essaye_de_tout_compter_avant_l'orage_(fin_de_comptage_vers_15H)._Par_contre,_j'ai_fait_la_partie_plage_sous_l'orage_et_le_vent_donc_les_effectifs_de_sanderling_sont_clairement_sous-estimes_(voir_Grand_Gravelot_et_BV)."] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/20219"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="la,_sous-estimation_claire_(je_n'ai_pas_pu_aller_a_la_prairie_la_ou_elles_se_tiennent_-_Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="la_sous_estime_severe_-_Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="sous_estime_-_Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="un_melange_de_PA,_BV_et_maubeche_certainement._Comptage_a_partir_de_la_rade_d'amour_et_de_l'estuaire_du_Lay_cote_Aiguillon_(battue_sanglier_sur_le_reste)_-_Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="sous-estimes_car_il_y_en_avait_pose_sur_les_pres_sales_-_Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="Attention_difference_avec_les_coef_habituels_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019._Eau_trop_haute_du_aux_basses_pressions,_oiseaux_deja_sur_les_mizottes_a_9h"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="sous-estime_-_Attention_difference_avec_les_coef_habituels_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019._Eau_trop_haute_du_aux_basses_pressions,_oiseaux_deja_sur_les_mizottes_a_9h"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="site_en_travaux"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="Attention_difference_avec_les_coef_habituels_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2019._Eau_trop_haute_du_aux_basses_pressions,_oiseaux_deja_sur_les_mizottes_a_9h."] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="mauvaise_visibilite"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="comptage_partiel_(travaux)"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="4_camping_car_sur_site"] <- "derangement_loisir"
Baie$remarques[Baie$remarques=="5_camping_car_sur_site"] <- "derangement_loisir"
Baie$remarques[Baie$remarques=="8_filets_sur_les_vases,_2_pecheurs"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="Derangement_le_matin"] <- "derangement"
Baie$remarques[Baie$remarques=="Derange_par_chasseur,_comptage_non_exhaustif"] <- "derangement_chasse"
Baie$remarques[Baie$remarques=="Travaux_en_cours_sur_la_digue"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="Attention_difference_avec_les_coef_habituel_de_comptage_(59),_comptage_de_rattrapage_suite_a_l'annulation_du_comptage_12/12/2022"] <- "diff_coef_maree"
Baie$remarques[Baie$remarques=="Par_ailleurs_j'ai_un_vol_d'environ_1000_oiseaux_(maubeches_et_PA_a_priori)_a_14h47_partant_vers_Charlotte._Je_n'ai_pas_pu_le_compter_correctement_mais_a_priori_Charlotte_non_plus._"] <- "comptage_partiel"
Baie$remarques[Baie$remarques=="Comptage_partiel,_brume"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Comptage_partiel,_brume._Sur_les_mizottes"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Comptage_partiel,_brume,_dont_30_dans_les_mizottes"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="au_moins_un_pecheur_au_filet_fixe"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="au_moins_3_pecheurs_au_filet_fixe_entre_la_Marina_et_les_Chaines"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="a_14h50._au_moins_3_pecheurs_au_filet_fixe_entre_la_Marina_et_les_Chaines._"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="a_15h._au_moins_3_pecheurs_au_filet_fixe_entre_la_Marina_et_les_Chaines._"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="a_15h05._au_moins_3_pecheurs_au_filet_fixe_entre_la_Marina_et_les_Chaines._"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="1_pecheur_a_l'haveneau"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="baccage_sur_le_canal_de_Lucon"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="Impossible_de_donner_un_chiffre_car_mer_agitee"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="tres_mauvaises_conditions_(pluie_et_vent),_effectifs_minimum"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="en_vol,_tres_mauvaises_conditions_(pluie_et_vent),_effectifs_minimum"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="plus_de_100_le_matin_au_niveau_digue,_tres_mauvaises_conditions_(pluie_et_vent),_effectifs_minimum"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="Mauvaise_visibilite_nord-ouest_du_poste_(comptage_realise_par_Anaide)"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Oiseaux_ont_probablement_glisses_sur_la_Sevre_avant_que_le_brouillard_se_leve"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Dont_50_jeunes._Mauvaise_visibilite_nord-ouest_du_poste_(comptage_realise_par_Anaide)"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Baccage_du_Cure_en_cours"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="tonnes_forts_dans_les_polders_de_Triaize"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="Baccage_du_cure"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="comptage_complique,_la_plupart_des_oiseaux_en_amont_de_l'estuaire"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Mauvaises_conditions_de_visibilite_apres_16h15"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="Mauvaises_conditions_de_visibilite"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="pecheurs_entre_Pte_et_Cure"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="manque_d'eau_et_travaux_de_remplacement_de_l'ouvrage_hydraulique_principal"] <- "derangement_travaux"
Baie$remarques[Baie$remarques=="Visibilite_difficile,_comptage_partiel_pour_les_especes_les_plus_petites."] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="pecheurs_au_carrelet"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="mauvaises_conditions_meteo"] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="2_bateaux_civelle_canal_de_lucon,_peu_d'anatides._400_sarcelles_en_arrivant_sur_poste,_parties_et_non_revu_par_les_voisins"] <- "derangement_peche"
Baie$remarques[Baie$remarques=="Temps_maussade_et_couvert_au_debut,_mais_devenant_lumineux_et_ensoleille."] <- "conditions_meteo_pas_fav"
Baie$remarques[Baie$remarques=="des_chasseurs_en_bordure_de_RNR__lors_du_comptage_donc_pas_mal_de_derangement_observe_"] <- "derangement_chasse"
Baie$remarques[Baie$remarques=="mauvaise visibilite"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="sous_estime"] <- "sous_estimation"
Baie$remarques[Baie$remarques=="Sous_estimation_avec_poste_des_Chaines"] <- "sous_estimation"
Baie$remarques[Baie$remarques=="vol_15h02_sous_estimation_avec_poste_du_Cure"] <- "sous_estimation"
Baie$remarques[Baie$remarques=="Comptage_partiel,_brume._Dans_les_mizottes"] <- "mauvaise_visibilite"
Baie$remarques[Baie$remarques=="possible_sous_estimation,_difficiles_a_compter"] <- "sous_estimation"
Baie$remarques[Baie$remarques=="en_vol,_peut-etre_une_sous_estimation"] <- "sous_estimation"
Baie$remarques[Baie$remarques=="condition_meteo_pas_fav"] <- "conditions_meteo_pas_fav"

#Rajout de la colonne qualité de comptage : 
Baie$qualite_comptage <- with(Baie, ifelse(Baie$site=="arcay" & Baie$date=="2019-10-14" , "douteux",
                                      ifelse(Baie$remarques=="diff_coeff_maree","douteux",
                                      ifelse(Baie$remarques=="conditions_meteo_pas_fav","douteux",
                                      ifelse(Baie$remarques=="derangement_travaux","douteux",
                                      ifelse(Baie$remarques=="derangement_peche","douteux",
                                      ifelse(Baie$remarques=="sous_estimation","douteux",
                                      ifelse(Baie$remarques=="mauvaise_visibilite","douteux",
                                      ifelse(Baie$remarques=="derangement_loisir","douteux",
                                      ifelse(Baie$remarques=="brume_de_chaleur","douteux",
                                      ifelse(Baie$remarques=="derangement","douteux",
                                      ifelse(Baie$remarques=="derangement_chasse","douteux",
                                      ifelse(Baie$remarques=="comptage_partiel","douteux",
                                      ifelse(Baie$remarques=="mer_montee_vite","douteux",
                                      ifelse(Baie$remarques=="en_vol,_peut-etre_une_sous_estimation","douteux","ok")))))))))))))))

# Rajouter une colonne avec le nom du secteur : 
Baie$secteur <- "baie_aiguillon"

#Créer une colonne protocole : 
Baie$protocole <- "terrestre"

# Ajouter les sites non prospectés et non renseignés pour toutes les dates :  
#site <- expand.grid(Baie$date,Baie$site)
#id$site <- paste(Baie$date,Baie$site)
#id$Baie <- paste(Baie$date,Baie$site)
# Le combiner au tableau original 
#Baie <- merge(Baie,site,by.x = "id",by.y = "id",all.x = T)


#Enlever les colonnes qui ne servent pas à grand chose : 
Baie <- Baie[,-c(1,2,4,9,10,15,16,20,21)]

# Vérification des doublons :
duplicated(Baie)
duplicated(Baie[c(1000:2000),])
duplicated(Baie[c(2000:3000),])
duplicated(Baie[c(3000:4000),]) # Doublon pour le bécasseau variable (12) le 2013-03-11 pour le site Communal de Lairoux
duplicated(Baie[c(4000:5000),])
duplicated(Baie[c(5000:6000),])
duplicated(Baie[c(6000:7000),])
duplicated(Baie[c(7000:8000),])
duplicated(Baie[c(8000:9000),])
duplicated(Baie[c(9000:10000),]) # 14 doublons 
duplicated(Baie[c(10000:11000),])# 1 doublon : bernache cravant 
duplicated(Baie[c(11000:12000),])
duplicated(Baie[c(12000:13000),])
duplicated(Baie[c(13000:14000),])
duplicated(Baie[c(14000:15000),])# 31 doublons 
duplicated(Baie[c(15000:16000),])
duplicated(Baie[c(16000:17000),])#16 doublons 
duplicated(Baie[c(17000:18000),])
duplicated(Baie[c(18000:19000),])#10 doublons 
duplicated(Baie[c(19000:20000),])
duplicated(Baie[c(20000:21000),])#19 doublons 
duplicated(Baie[c(21000:22000),])
duplicated(Baie[c(22000:23000),])
duplicated(Baie[c(23000:24000),])
duplicated(Baie[c(24000:24715),])

Baie <- distinct(Baie)   


      ########## 4. Le cotentin : #############

Cotentin <- read.csv("Data/donnees_cotentin.csv",header = T,fileEncoding = "utf-8",sep = ";")
str(Cotentin)
View(Cotentin)
# Deux types de suivis : 
  # -> Remises diurnes 
    # Compte les anatidés + comptage simultané + exhaustif sur la remise de Beauguillot (décompté de manière décadaire) 
    # Attention données agrégées pour 2004 et 2008 (sous l'entité RNN_Beauguillot) + certaines limicoles (terrestre et côtiers à affinité terrestre)
  # -> Limicoles côtiers : 
    # Exhaustif + certains anatidés de manière exhaustif et mensuel 
# ATTENTION : pour le site : Polder Sainte Marie : gestion hydraulique et agropastorale favorisant l'accueil des oiseaux depuis 2010

#Changer le nom des colonnes + enlever la première ligne (redite des titres des colonnes)

Cotentin <- Cotentin[-c(1),]
Cotentin <- Cotentin[,-c(1,2,3,4,5,6,7,10,15,17,19,21,24)]

colnames(Cotentin)[1] <- "family_tax"
colnames(Cotentin) [2] <- "order_tax"
colnames(Cotentin) [3] <- "espece"
colnames(Cotentin) [4] <- "suivi"
colnames(Cotentin) [5] <- "obs"
colnames(Cotentin) [6] <- "observateur_org"
colnames(Cotentin) [7] <- "date"
colnames(Cotentin) [8] <- "site"
colnames(Cotentin) [9] <- "abondance"
colnames(Cotentin) [10] <- "observation_protocole"
colnames(Cotentin) [12] <- "remarques"

#Format de la date : 
unique(Cotentin$date)
Cotentin$date <- ymd(Cotentin$date)

#Nom des espèces : 
Cotentin[,3] <- tolower(Cotentin[,3])
Cotentin[,3] <- gsub(" ","_",Cotentin[,3])
Cotentin[,3] <- gsub("-","_",Cotentin[,3])
Cotentin[,3] <- gsub("'","_",Cotentin[,3])
Cotentin[,3] <- gsub("é","e",Cotentin[,3])
Cotentin[,3] <- gsub("è","e",Cotentin[,3])
Cotentin[,3] <- gsub("î","i",Cotentin[,3])
Cotentin[,3] <- gsub("à","a",Cotentin[,3])
Cotentin[,3] <- gsub("ê","e",Cotentin[,3])
Cotentin[,3] <- gsub("â","a",Cotentin[,3])
Cotentin[,3] <- gsub("\\.","",Cotentin[,3])
Cotentin[,3] <-iconv(Cotentin[,3], from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(Cotentin$espece))

#Cotentin <- subset(Cotentin,!(Cotentin$espece=="")) 

Cotentin[,3] <- gsub("bernache_cravant_du_pacifique,_bernache_du_pacifique","bernache_du_pacifique",Cotentin[,3])
Cotentin[,3] <- gsub("canard_des_bahamas,_pilet_des_bahamas","canard_des_bahamas",Cotentin[,3])
Cotentin[,3] <- gsub("combattant_varie,_chevalier_combattant","combattant_varie",Cotentin[,3])
Cotentin[,3] <- gsub("gravelot_a_collier_interrompu,_gravelot_de_kent","gravelot_a_collier_interrompu",Cotentin[,3])
Cotentin[,3] <- gsub("guignard_d_eurasie,_pluvier_guignard","pluvier_guignard",Cotentin[,3])
Cotentin[,3] <- gsub("oie_de_taiga,_oie_des_moissons","oie_des_moissons",Cotentin[,3])
Cotentin[,3] <- gsub("ouette_d_egypte,_oie_d_egypte","ouette_d_egypte",Cotentin[,3])
Cotentin[,3] <- gsub("tadorne_casarca,_casarca_roux","tadorne_casarca",Cotentin[,3])
Cotentin[,3] <- gsub("tournepierre_a_collier,_pluvier_des_saline","tournepierre_a_collier",Cotentin[,3])
Cotentin[,3] <- gsub("sarcelle_a_ailes_vertes,_sarcelle_de_la_caroline","sarcelle_a_ailes_vertes",Cotentin[,3])
# Mystère... mets un s à tournepierre...
Cotentin[,3] <- gsub("tournepierre_a_colliers","tournepierre_a_collier",Cotentin[,3])

# Enlever guillemot, pinguin torda, labbe parasite :
Cotentin <- subset(Cotentin,!(Cotentin$espece=="pingouin_torda,_petit_pingouin"|Cotentin$espece=="guillemot_de_troil"|Cotentin$espece=="labbe_parasite"))

# -> Protocole remise diurne -> Anatidés comptés de manières exhaustive (pas les limicoles - sauf vanneau et pluvier doré)
# -> Protocole limicoles côtiers -> Limicoles cotiers compté de manière exaustive + 4 anatidés : Bernache cravant ; Tadorne de B ; Eider à D ; Harle huppé
    # -> En revanche autres anatidés pas de manière exhaustive (donc les retirer) idem pour les limicoles terrestre (bécassines + pluvier doré + vanneau)



#Nom famille et ordre :

Cotentin[,1] <- tolower(Cotentin[,1])
Cotentin[,2] <- tolower(Cotentin[,2])
Cotentin[,4] <- tolower(Cotentin[,4])
Cotentin[,5] <- tolower(Cotentin[,5])
Cotentin[,6] <- tolower(Cotentin[,6])
Cotentin[,8] <- tolower(Cotentin[,8])

#Nom observateurs 
unique(Cotentin$obs)
Cotentin[,5] <- tolower(Cotentin[,5])
Cotentin[,5] <-iconv(Cotentin[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Nom des sites : 
unique(Cotentin$site)
# C'est ok ! 
# Supprimer le Polder_Sainte_Marie : changement de gestion au cours du temps 

Cotentin <- subset(Cotentin, !(Cotentin$site=="polder_ste_marie_cel"))
unique(Cotentin$site)

#Supprimer les données agrégées 2004 et 2008 : 
Cotentin <- subset(Cotentin, !(Cotentin$site=="rnn_beauguillot"))

#Séparer les données des deux protocoles 
# Suivi des remises -> Se concentrer uniquement sur les anatidés 
# Suivi limicoles côtiers -> se concentrer uniquement sur les limicoles 


#Vérification abondance : 
unique(Cotentin$abondance)
# Attention à certains moments des fourchettes sont données ex : 420-440 
# Que faire ? Prendre la moyenne, le min, le max, ou supprimer ? 

Cotentin$moy <- apply(Cotentin$abondance,FUN = mean)

help("apply")

#sélection des années et des mois : 

Cotentin$annee <- year(Cotentin$date)
Cotentin$mois <- month(Cotentin$date)

unique(Cotentin$annee) #De 2004 à 2023
unique(Cotentin$mois)
# -> Ne sélectionner que les mois où on trouve les hivernants 

Cotentin <- subset(Cotentin,!(Cotentin$mois=="5"|Cotentin$mois=="6"|Cotentin$mois=="7"|Cotentin$mois=="8"))




#####Prendre en compte les remarques : (à compléter)
unique(Cotentin$remarques)

Cotentin[,12] <- gsub(":","_",Cotentin[,12])
Cotentin[,12] <- gsub("/","_",Cotentin[,12])
Cotentin[,12] <- gsub(" ","_",Cotentin[,12])
Cotentin[,12] <- iconv(Cotentin[,12], from = 'UTF-8', to ='ASCII//TRANSLIT')
#Uniformisation des remarques : 

Cotentin$remarques[Cotentin$remarques== "Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_Travaux_terrassement_chemin_des_observatoires"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mauvaises___Facteur_1___Vent___Decompte___Decompte_total___Remarque___fosses,_prairies_gelees,_froid_intense,_mer_agitee"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="___derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Petites_chutes_de_neige___"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="travaux_dans_le_polder"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="seulement_canaux_en_eau___le_suivi_de_la_mare_de_gabion_reste_difficile_et_partiel_a_partir_de_l'observatoire_de_la_dune_sud_(niveau_d'eau_faible)"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="seulement_canaux_en_eau___suivi_difficile,_probablement_moins_exhaustif_qu'habituellement_a_la_meme_epoque_(niveau_d'eau_faible)"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="seulement_canaux_en_eau"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_90%_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>site_partiellement_gele_<br>_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres<br>Decompte_total<br>Site_gele_en_grande_partie._Bonne_precision__derangement__"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_80%<br>Neige_au_sol_(5-6cm),_sol_couvert_a_60%_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_partiel<br>Niveau_d'eau_tres_faible_Bonne_precision__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_80%_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_en_presque_totalite_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(derangement)<br>Decompte_total<br>Chiens_en_divagation._Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(derangements)<br>Decompte_total<br>Envols_massifs_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluies)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent_et_derangements)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(derangements)<br>Decompte_total_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Effectif_sous-estime_par_manque_d'experience_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_partiel<br>Reserve_naturelle_non_comptee._Bonne_precision__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)<br>Decompte_total_Bonne_precision__derangement__"] <- "dérangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres<br>Decompte_total_Bonne_precision__derangement__"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(brouillard)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie)<br>Decompte_total<br>Plans_d'eau_geles_a_80%_sur_la_reserve_naturelle._Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie)<br>Decompte_total<br>Plans_d'eau_geles_a_80%_sur_la_reserve_naturelle._Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vents)<br>Decompte_total_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Decompte_total_Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Decompte_total<br>Nappe_affleurante_sur_le_polder_communal._Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_50%_Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_50%_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes___Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Sec"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Pluie_Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Inondes"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes__Derangements_Decompte_total___etat_plan_d'eau__Geles_partiel__etat_hors_plan_d'eau___Geles"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Derangements_Decompte_total_helicoptere__etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Inondes"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Pluie_Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Secs"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes__vent_Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Inondes"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Vent_Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Inondes"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_PluieVent_Decompte_total___etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Secs"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Derangements_Decompte_total_Fauche_en_cours__etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Secs"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Derangements_Decompte_total_broyage_sud_RNB__etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Secs"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Vent_Decompte_total_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Vent_Decompte_total_Wetlands_Int."] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Vent_Decompte_total_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Normaux_Secs_Bonne_precision_Conditions_de_denombrement_bonnes__Decompte_total_Travaux_cloture_digue_de_mer"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Geles_partiel_Geles_Bonne_precision_Conditions_de_denombrement_bonnes__Decompte_total_"] <- "condition_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="ABIO_ANAT_Plansdeau___Geles_partiel___ABIO_ANAT_Horsplansdeau___Geles___Bonne_precision"] <- "condition_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Secs_Conditions_de_denombrement___Bonnes_____Decompte___Decompte_totalTravaux_en_cours_(clotures,_broyage_gabion)"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Secs_Conditions_de_denombrement___Bonnes_____Decompte___Decompte_totalEvacuation_bovins_en_cours_sur_RNN"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Inondes_Conditions_de_denombrement___Mediocres_Pluie___Decompte___Decompte_total"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Mauvaises___Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque__"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Geles_partiel___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_42"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours,_comptage_apres_battue_au_sanglier"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Mauvaises___Facteur_1___Pluie___Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1___Vent___Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_chantier_observatoires_Taret"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_Travaux_terrassement_chemin_des_observatoires"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="seulement_canaux_en_eau___"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_presque_en_totalite_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(derangements)<br>Decompte_partiel<br>Envols_massifs_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_partiel<br>Date_de_comptage_decale._Bonne_precision__derangement__"] <- "comptage_pariel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Decompte_total_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent_et_derangements)<br>Decompte_total_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Normaux_Secs_Ordre_de_grandeur_Conditions_de_denombrement_bonnes__Decompte_total_Travaux_cloture_digue_de_me"] <- "derabgement_travaux"
Cotentin$remarques[Cotentin$remarques=="Normaux_Secs_Sous_estimation_Conditions_de_denombrement_bonnes__Decompte_total_Travaux_agricoles_en_cours"] <- "derabgement_travaux"
Cotentin$remarques[Cotentin$remarques=="Normaux_Secs_Sous_estimation_Conditions_de_denombrement_bonnes__Decompte_total_"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Geles_partiel_Geles_Sous_estimation_Conditions_de_denombrement_bonnes__Decompte_total_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Secs_Conditions_de_denombrement___Bonnes_____Decompte___Decompte_totalEvacuation_balles_de_foin_quart_nord_ouest"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Secs_Conditions_de_denombrement___Bonnes_____Decompte___Decompte_totalTravaux_agricoles_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___travaux_agricoles_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___2eme_fauche_en_cour"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_12"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_19"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_28"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_38"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_41"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Mauvaises___Facteur_1___Pluie___Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Ordre_de_grandeur___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN__estimation_en_vol"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_chantier_observatoires_Taret"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Niveaux_d'eau_tres_bas_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)<br>Decompte_total_Sous_estimation__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(brouillard)<br>Decompte_total_Non_compte__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vents)<br>Decompte_total_Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total_Sous_estimation__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_10"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_17"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_25"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_34"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_40"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total<br>1_marquee_\"BZH\"_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>En_groupe_avec_les_oies_cendrees_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Sous_estimation___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(brouillard)_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="nappe_affleurante___Sous_estimation"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Decompte_total<br>Double_comptage_probable_!_Sous_estimation__derangement__"] <- "condition_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes___Decompte_total_mauvaise_visibilite__etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Secs"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_20"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement__pluie"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision_Conditions_de_denombrement_moyennes_Pluie+Vent_Couverture_du_site_totale_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision_Conditions_de_denombrement_mediocres_Pluie+Vent_Couverture_du_site_totale_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision___Conditions_de_denombrement_mediocres_Vent_Couverture_du_site_totale_"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Sous_estimation___Conditions_de_denombrement_bonnes___Couverture_du_site_totale_"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mediocres___Facteur_1___Vent+Pluie___Decompte___Decompte_total___Remarque__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mediocres___Facteur_1___Vent___Decompte___Decompte_total___Remarque___tempete"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mediocres___Facteur_1___Vent___Decompte___Decompte_total___Remarque__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mauvaises___Facteur_1___Pluie___Decompte___Decompte_total___Remarque__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mauvaises___Facteur_1___Vent___Decompte___Decompte_total___Remarque__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="_Sous_estimation__derangement__pluie"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Comptage_incomplet_Sous_estimation__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes__vent_Decompte_total_non_comptes__etat_plan_d'eau__Normaux__etat_hors_plan_d'eau___Inondes"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(brouillard)<br>Decompte_total<br>Couple_nicheur_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Decompte_total<br>Nid_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total<br>4_ad_et_2_imm_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_35"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_11"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_18"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_26"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_36"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total_Non_compte__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)___Normaux_Eau-milieux_(hors_plans_d'eau)___Inondes_Conditions_de_denombrement___Bonnes_____Decompte___Decompte_partiel"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_33"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)<br>Couverture_du_site_totale_Sous_estimation__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluies_et_vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie_et_vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Decompte_total<br>Femelle_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision___Conditions_de_denombrement_bonnes___Couverture_du_site_totale_rapaces,_promeneurs"] <- "derangement_loisirs"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mauvaises___Facteur_1___Derangements___Decompte___Decompte_total___Remarque___Jet-skis_(X4)"] <- "derangement_loisirs"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_27"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Secs___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte______Remarque___battue_administrative_le_08_37"] <- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Pluie_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes__Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes___Couverture_du_site_partielle"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Derangements_Couverture_du_site_totale"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres__Couverture_du_site_totale"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes__Vent_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Vent+Pluie_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Conditions_de_denombrement __Mediocres___Facteur_1 __Vent___Decompte __Decompte_total___Remarque __en_vol"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Conditions_de_denombrement __Bonnes____Facteur_1 _____Decompte __Decompte_partiel___Remarque _"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_partiel<br>Reserve_naturelle_non_comptee._Non_compte__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Couverture_du_site_partielle_Bonne_precision__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Couverture_du_site_totale___derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(brouillard)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Couverture_du_site_totale_Sous_estimation__derangement__"] <- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(derangements)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"] <- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie,_vent_et_derangements)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent_et_turbulences)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(derangements)<br>Couverture_du_site_totale_Sous_estimation__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(turbulences_et_derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(turbulences_et_derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "mauvaise_condition_comptage"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(turbulences)<br>Couverture_du_site_totale___derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(pluie,_vent_et_derangements)<br>Decompte_partiel_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(vent)<br>Couverture_du_site_totale___derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Conditions_de_denombrement __Bonnes____Facteur_1 _____Decompte __Decompte_total___Remarque __2eme_fauche_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Eau-milieux_(hors_plans_d'eau) _____Conditions_de_denombrement __Bonnes____Facteur_1 _____Decompte __Decompte_total___Remarque __Travaux_amenagement_reserve_en_cours"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision_Conditions_de_denombrement_moyennes_Pluie+Vent_Couverture_du_site_totale_en_vol"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Sous_estimation_Conditions_de_denombrement_bonnes___Couverture_du_site_totale_"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Vent_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Pluie+Vent_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Niveau_d'eau_en_hausse_sur_le_polder_communal._Sous_estimation__derangement__"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Pluie_Couverture_du_site_partielle"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Decompte_total<br>+_de_2000_oiseaux_Non_compte__derangement__"]<- "conditions__meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_(derangements)<br>Decompte_total_Sous_estimation__derangement__"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres<br>Decompte_total_Sous_estimation__derangement__"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Decompte_total<br>Nappe_affleurante_sur_le_polder_communal._Non_compte__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _Normaux___Eau-milieux_(hors_plans_d'eau) __Secs___Precision_du_denombrement __Bonne_precision___Conditions_de_denombrement __Bonnes____Facteur_1 _____Facteur_2 _____Facteur_3 _____Decompte _____Remarque __battue_administrative_le_08_15"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _Normaux___Eau-milieux_(hors_plans_d'eau) __Secs___Precision_du_denombrement __Bonne_precision___Conditions_de_denombrement __Bonnes____Facteur_1 _____Facteur_2 _____Facteur_3 _____Decompte _____Remarque __battue_administrative_le_08_24"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _Normaux___Eau-milieux_(hors_plans_d'eau) __Secs___Precision_du_denombrement __Bonne_precision___Conditions_de_denombrement __Bonnes____Facteur_1 _____Facteur_2 _____Facteur_3 _____Decompte _____Remarque __battue_administrative_le_08_30"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _Normaux___Eau-milieux_(hors_plans_d'eau) __Secs___Precision_du_denombrement __Bonne_precision___Conditions_de_denombrement __Bonnes____Facteur_1 _____Facteur_2 _____Facteur_3 _____Decompte _____Remarque __battue_administrative_le_08_39"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(derangements)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(vent_et_derangements)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie)<br>Couverture_du_site_totale<br>Juveniles_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)<br>Couverture_du_site_totale<br>En_vol_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie,_vent_et_derangements)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(vent)<br>Couverture_du_site_totale___derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Couverture_du_site_totale<br>Coefficient_de_maree_de_49_!_Bonne_precision__derangement__"]<- "diff_coeff_maree"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Pluie+Derangements_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Pluie_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)___derangement__pluie"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)_Bonne_precision__derangement__pluie"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie_et_vent)_Bonne_precision__derangement__pluie"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)_Bonne_precision__derangement___derangement_(inconnu)"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement__vent"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="___derangement__vent"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement__Chasseurs"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="ras___derangement__vent"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="___derangement__activites_de_plage"]<- "derangement_loisirs"
Cotentin$remarques[Cotentin$remarques=="visibilite_moyenne_Bonne_precision__derangement__indetermine"]<- "mauvaise_visibilite"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement__brouillard"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement___vent"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Sous_estimation_Conditions_de_denombrement_mediocres_Pluie+Vent_Couverture_du_site_totale_"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="_Bonne_precision__derangement___pluie"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_Bonne_precision__derangement__pluie"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)_Bonne_precision__derangement__derangement_(inconnu)"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(turbulences)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluie_et_vent)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent+Pluie_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Decompte_total<br>Site_gele_a_90%_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) _Normaux___Eau-milieux_(hors_plans_d'eau) __Secs___Precision_du_denombrement __Bonne_precision___Conditions_de_denombrement __Bonnes____Facteur_1 _____Facteur_2 _____Facteur_3 _____Decompte _____Remarque __battue_administrative_le_08_16"]<- "derangement_chasse"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)<br>Couverture_du_site_totale<br>En_vol_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(derangements)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(pluie)<br>Couverture_du_site_totale<br>dont_3_juv_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(turbulences)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vents)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="_Precision_<_10%__derangement__"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Vent_et_turbulences)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Vent_et_turbulences)<br>Couverture_du_site_totale___derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(vent)<br>Couverture_du_site_partielle_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(pluie_et_vent)<br>Couverture_du_site_totale<br>En_vol_Sous-estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluies_et_vent)<br>Couverture_du_site_totale___derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluies_et_vent)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mauvaises_Vent_Decompte_total_au_dortoir_(16h00)"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(turbulences)<br>Couverture_du_site_totale___derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(turbulences)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(brouillard,_pluie_et_vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_vent)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluie)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluie)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Couverture_du_site_partielle<br>En_vol_Bonne_precision__derangement__"]<- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes<br>Couverture_du_site_totale<br>En_vol_Sous_estimation__derangement__"]<- "sous_estimation"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_vents)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(Pluie_et_vents)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_(vent)<br>Couverture_du_site_partielle_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_derangements)<br>Couverture_du_site_totale_Sous_estimation__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_et_brouillard)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Pluie_)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_totale_En_vol_vers_le_nord"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_totale_Poses_sur_la_partie_terrestre_lors_de_la_passee."]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_totale_En_vol_vers_l'est"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_totale_Poses_sur_le_polder_lors_de_la_passee"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Bonne_precision_Conditions_de_denombrement_mediocres_Pluie+Vent_Couverture_du_site_totale_a_la_passee_du_matin_(74_NO,_11_SE,_78_E)"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes__Derangements_Decompte_total_4060_a_la_passee_du_matin___etat_plan_d'eau__Geles_partiel__etat_hors_plan_d'eau___Geles"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau) __Normaux_Eau-milieux_(hors_plans_d'eau) __Inondes_Conditions_de_denombrement __Bonnes_____Decompte __Decompte_partielvers_le_sud"]<- "comptage_partiel"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_bonnes_(Vent)<br>Couverture_du_site_totale_Bonne_precision__derangement__"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Brouillard_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Brouillard_Couverture_du_site_totale"]<- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Brouillard_Couverture_du_site_partielle"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_(Derangements)<br>Couverture_du_site_totale_Bonne_precision__derangement__"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Pluie+Vent_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Derangements_Couverture_du_site_totale"] <- "derangement"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Pluie+Vent_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Brouillard+Derangements_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes__Couverture_du_site_partielle"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_mediocres_Pluie+Vent+Derangements_Couverture_du_site_totale"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Conditions_de_denombrement_moyennes_Vent_Couverture_du_site_partielle"] <- "conditions_meteo_pas_fav"
Cotentin$remarques[Cotentin$remarques=="Sous_estimation_Conditions_de_denombrement_moyennes_Pluie+Vent_Couverture_du_site_totale_"] <- "conditions_meteo_pas_fav"

Cotentin$qualite_comptage <- with(Cotentin, ifelse(Cotentin$remarques=="derangement","douteux",
                                            ifelse(Cotentin$remarques=="derangement_chasse","douteux",
                                            ifelse(Cotentin$remarques=="derangement_travaux","douteux",
                                            ifelse(Cotentin$remarques=="conditions_meteo_pas_fav","douteux",
                                            ifelse(Cotentin$remarques=="comptage_partiel","douteux",
                                            ifelse(Cotentin$remarques=="mauvaise_condition_comptage","douteux",
                                            ifelse(Cotentin$remarques=="derangement_loisirs","douteux",
                                            ifelse(Cotentin$remarques=="mauvaise_visibilite","douteux",
                                            ifelse(Cotentin$remarques=="sous_estimation","douteux",
                                            ifelse(Cotentin$remarques=="diff_coeff_maree","douteux","ok")))))))))))

#Création d'une colonne secteur

Cotentin$secteur <- "cotentin"

# Ajout protocole : 
Cotentin$protocole <- "terrestre"

            ###################### 5. Bassin Arcachon ###########

Arcachon <- read.csv("Data/donnees_arcachon_limicoles.csv", header = T, fileEncoding = "UTF-8", sep = ";")
View(Arcachon)
str(Arcachon)

#Nom des colonnes : 

colnames(Arcachon)[1] <- "date"
colnames(Arcachon) [2] <- "annee"
colnames(Arcachon) [3] <- "mois_let"
colnames(Arcachon) [4] <- "espece"
colnames(Arcachon) [5] <- "nom_latin"
colnames(Arcachon) [6] <- "lieu_dit"
colnames(Arcachon) [7] <- "site"
colnames(Arcachon) [8] <- "commune"
colnames(Arcachon) [9] <- "abondance"
colnames(Arcachon) [10] <- "heure_bm"
colnames(Arcachon) [11] <- "coef_de_marree"
colnames(Arcachon) [12] <- "obs"
colnames(Arcachon) [19] <- "derangement"                               
colnames(Arcachon) [18] <- "remarques"

#Nom des especes: 

Arcachon[,4] <- tolower(Arcachon[,4])
Arcachon[,4] <- gsub(" ","_",Arcachon[,4])
Arcachon[,4] <- gsub("é","e",Arcachon[,4])
Arcachon[,4] <- gsub("î","i",Arcachon[,4])
Arcachon[,4] <- gsub("è","e",Arcachon[,4])
Arcachon[,4] <- gsub("à","a",Arcachon[,4])
Arcachon[,4] <-iconv(Arcachon[,4], from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(Arcachon$espece))

# Problème des espaces en fin de noms 

Arcachon[,4] <- gsub("avocette_elegante_","avocette_elegante",Arcachon[,4])
Arcachon[,4] <- gsub("becasseau_maubèche_","becasseau_maubèche",Arcachon[,4])
Arcachon[,4] <- gsub("becasseau_variable_","becasseau_variable",Arcachon[,4])
Arcachon[,4] <- gsub("becassine_des_marais_","becassine_des_marais",Arcachon[,4])
Arcachon[,4] <- gsub("chevalier_aboyeur_","chevalier_aboyeur",Arcachon[,4])
Arcachon[,4] <- gsub("chevalier_gambette_","chevalier_gambette",Arcachon[,4])
Arcachon[,4] <- gsub("vanneau_huppe_","vanneau_huppe",Arcachon[,4])
Arcachon[,4] <- gsub("barge_rousse_","barge_rousse",Arcachon[,4])
Arcachon[,4] <- gsub("chevalier_culblanc_","chevalier_culblanc",Arcachon[,4])
Arcachon[,4] <- gsub("courlis_corlieu_","courlis_corlieu",Arcachon[,4])
Arcachon[,4] <- gsub("tournepierre_a_collier_","tournepierre_a_collier",Arcachon[,4])
Arcachon[,4] <- gsub("barge_a_queue_noire_","barge_a_queue_noire",Arcachon[,4])
Arcachon[,4] <- gsub("becasseau_cocorli_","becasseau_cocorli",Arcachon[,4])
Arcachon[,4] <- gsub("becasseau_minute_","becasseau_minute",Arcachon[,4])
Arcachon[,4] <- gsub("becasseau_sanderling_","becasseau_sanderling",Arcachon[,4])
Arcachon[,4] <- gsub("chevalier_arlequin_","chevalier_arlequin",Arcachon[,4])
Arcachon[,4] <- gsub("courlis_cendre_","courlis_cendre",Arcachon[,4])
Arcachon[,4] <- gsub("grand_gravelot_","grand_gravelot",Arcachon[,4])

# ATTENTION Pas que des limicoles dans le jeu de données : des ardéidés aussi ! 
# Les retirer : 

Arcachon <- subset(Arcachon, !(Arcachon$espece=="aigrette_des_recifs"|Arcachon$espece=="heron_cendre"|Arcachon$espece=="spatule_blanche"))

# données sp + "Gravelot ou becasseau" 

Arcachon <- subset(Arcachon,!(Arcachon$espece=="gravelot_sp"|Arcachon$espece=="becasseau_sp"|Arcachon$espece=="becasseau_ou_gravelot"
                   |Arcachon$espece=="courlis_sp"|Arcachon$espece=="chevalier_sp"|Arcachon$espece=="barge_sp"))

# Format de date : 
unique(Arcachon$date)
# Enlever les dates où pas de comptage à cause des mauvaises conditions météo : 
Arcachon <- subset(Arcachon,!(Arcachon$date=="Octobre, aucun comptage tempêtes successives"|Arcachon$date=="Pas de comptage météo horrible (non représentatif)"))
Arcachon$date <- dmy(Arcachon$date)

#Ajouter le mois : en "chiffre"
Arcachon$mois <- month(Arcachon$date)
unique(Arcachon$mois)

# -> Ne conserver que les mois pour les hivernants : 

Arcachon <- subset(Arcachon,!(Arcachon$mois=="5"|Arcachon$mois=="6"|Arcachon$mois=="7"|Arcachon$mois=="8"))


#Nom des sites :
unique(Arcachon$site)
Arcachon[,7] <- tolower(Arcachon[,7])
Arcachon[,7] <- gsub(" ","_",Arcachon[,7])
Arcachon[,7] <- gsub("é","e",Arcachon[,7])
Arcachon[,7] <- gsub("-","_",Arcachon[,7])
Arcachon[,7] <- gsub("'","_",Arcachon[,7])
Arcachon[,7] <-iconv(Arcachon[,7], from = 'UTF-8', to = 'ASCII//TRANSLIT')

# PB avec les espaces en fin de mots : 
# -> 
Arcachon[,7] <- gsub("ile_aux_oiseaux_","ile_aux_oiseaux",Arcachon[,7])

#Retrier les lignes où le site n'est pas renseigné : 
Arcachon <- subset(Arcachon,!(Arcachon$site==""))

# Vérification des abondances : 
unique(Arcachon$abondance)
unique(Arcachon[c(995:1363),9])
#Case vide + Partiel + 0 + NC 
# Les 0 ne correspondent pas à de vrais absence : mais à des non comptage : 

Arcachon <- subset(Arcachon,!(Arcachon$abondance=="0"|Arcachon$abondance=="NC"|Arcachon$abondance==""))

#Vérification des doublons + double comptage : 
duplicated(Arcachon)
duplicated(Arcachon[c(1000:2000),])
duplicated(Arcachon[c(2000:3000),])
duplicated(Arcachon[c(3000:4000),])
duplicated(Arcachon[c(4000:5000),])
duplicated(Arcachon[c(5000:6000),])
duplicated(Arcachon[c(6000:7000),])
duplicated(Arcachon[c(7000:8000),])
duplicated(Arcachon[c(8000:8551),])

# ATTENTION plusieurs comptage pour la même dans un même site => données agrégées intra-site 

#Ajouter le nom du secteur : 

Arcachon$secteur <- "arcachon"

#Ajouter le protocole : 

Arcachon$protocole <- "NA"

#Prendre en compte les remarques : 
unique(Arcachon$remarques)

Arcachon$qualite_comptage <- with(Arcachon, ifelse(Arcachon$remarques=="dérangement: engins nautiques motorisés et non motorisés","douteux",
                                            ifelse(Arcachon$remarques=="beaucoup de promeneurs","douteux",
                                            ifelse(Arcachon$remarques=="dérangement: engin nautique non motorisé","douteux",
                                            ifelse(Arcachon$remarques=="dérangement chasse","douteux",
                                             ifelse(Arcachon$remarques=="dérangements promeneurs","douteux",
                                             ifelse(Arcachon$remarques=="dérangement engin nautique motorisé","douteux",
                                            ifelse(Arcachon$remarques=="comptage depuis la dune, brume épaisse","douteux",
                                              ifelse(Arcachon$remarques=="brume épaisse","douteux",
                                              ifelse(Arcachon$remarques=="comptage partiel, 8km, trop de houle","douteux",
                                                ifelse(Arcachon$remarques=="nbx promeneurs, chiens et kite","douteux",
                                                ifelse(Arcachon$remarques=="comptage partiel, forte houle","douteux",
                                                ifelse(Arcachon$remarques=="comptage partiel, panne du bateau","douteux",
                                               ifelse(Arcachon$remarques=="dérangement bateau 278 quittent le site entre 15:20 et 15h45","douteux",
                                               ifelse(Arcachon$remarques=="Dérangement drague en limite des reposoirs + nombreuses tonnes occupés","douteux",
                                              ifelse(Arcachon$remarques=="quelques chasseurs sur place","douteux",
                                              ifelse(Arcachon$remarques=="dérangements nbx promeneurs","douteux",
                                              ifelse(Arcachon$remarques=="dérangement vélos electriques + promeneurs","douteux",
                                              ifelse(Arcachon$remarques=="très nombreux dérangements","douteux",
                                              ifelse(Arcachon$remarques=="dérangement par Kite","douteux",
                                              ifelse(Arcachon$remarques=="dérangement Kite, chiens, promeneurs","douteux",
                                              ifelse(Arcachon$remarques=="nombreux chiens","douteux",
                                              ifelse(Arcachon$remarques=="chiens + kites","douteux",
                                              ifelse(Arcachon$remarques=="Attaque de Faucon Pellerin, gros envol","douteux",
                                              ifelse(Arcachon$remarques=="Attaque de chien","douteux",
                                              ifelse(Arcachon$remarques=="Dérangement ","douteux","ok"))))))))))))))))))))))))))




            ############## 6. Reserve du Rhin #################


Rhin <- read.csv("Data/donnees_reserve_rhin.csv", header = T, fileEncoding = "UTF-8", sep = ";")
View(Rhin)
str(Rhin)

#3 sites de comptage divisé en secteurs de comptages : 
# PB -> ces secteurs (lieux_dits_actuels) peuvent être agrégés (secteur 1+2) en raison de changement de noms

colnames(Rhin) <- tolower(colnames(Rhin))
colnames(Rhin) <- gsub("\\.","_",colnames(Rhin))
colnames(Rhin) <- gsub("é","e",colnames(Rhin))
colnames(Rhin) <- iconv(colnames(Rhin), from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Nom espece : 

colnames(Rhin)[4] <- "espece" 
Rhin[,4] <- tolower(Rhin[,4])
Rhin[,4] <- gsub(" ","_",Rhin[,4])
Rhin[,4] <- gsub("'","_",Rhin[,4])
Rhin[,4] <- iconv(Rhin[,4],from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(Rhin$espece))

# -> Ne conserver que les données pour limicoles et anatidés 

unique(Rhin$famille)

Rhin <- subset(Rhin, Rhin$famille=="Anatidae"|Rhin$famille=="Scolopacidae"|Rhin$famille=="Charadriidae"
               |Rhin$famille=="Haematopodidae"|Rhin$famille=="Recurvirostridae")


sort(unique(Rhin$espece))

# -> Retirer l'oie domestique : 
Rhin <- subset(Rhin, !(Rhin$espece=="oie_domestique"))
# -> Retirer les especes indeterminées : 
Rhin <- subset(Rhin,!(Rhin$espece=="becasseau_indetermine"|Rhin$espece=="chevalier_indetermine_(tringa)"
                      |Rhin$espece=="gravelot_indetermine"|Rhin$espece=="oie_indeterminee"))
# -> Retirer les hybrides : 
Rhin <- subset(Rhin, !(Rhin$espece=="hybride_bernache_du_canada_x_oie_cendree"
                       |Rhin$espece=="hybride_fuligule_milouin_x_morillon"))

# Format de la date : 
unique(Rhin$date)
  # -> Plusieurs formats : 13-janv-19 et 13/01/2019 
  # PB : le mois est écrit en français : 
Rhin[,9] <- gsub("14-janv-18","14/01/2018",Rhin[,9])
Rhin[,9] <- gsub("15-janv-18","15/01/2018",Rhin[,9])
Rhin[,9] <- gsub("13-janv-19","13/01/2019",Rhin[,9])
Rhin[,9] <- gsub("15-janv-19","15/01/2019",Rhin[,9])
Rhin[,9] <- gsub("12-janv-20","12/01/2020",Rhin[,9])
Rhin[,9] <- gsub("17-janv-21","17/01/2021",Rhin[,9])
Rhin[,9] <- gsub("12-janv-20","12/01/2020",Rhin[,9])
Rhin[,9] <- gsub("13-janv-22","13/01/2022",Rhin[,9])
Rhin[,9] <- gsub("16-janv-22","16/01/2022",Rhin[,9])
Rhin[,9] <- gsub("20-janv-22","20/01/2022",Rhin[,9])
Rhin[,9] <- gsub("14-janv-23","14/01/2023",Rhin[,9])
Rhin[,9] <- gsub("15-janv-23","15/01/2023",Rhin[,9])
Rhin[,9] <- gsub("14-janv-24","14/01/2024",Rhin[,9])

Rhin$date <- dmy(Rhin$date)

# -> Ne retenir que les années correspondant à notre série temporelle : 

Rhin <- subset(Rhin,! (Rhin$annee<2004))

# -> Ne retenir que les mois pour les hivernants : 
Rhin <- subset(Rhin, !(Rhin$mois=="5"|Rhin$mois=="6"|Rhin$mois=="7"|Rhin$mois=="8"))

# Enlever les mois de janvier-février-mars et avril 2004 (première saison 2004-2005)

Rhin <- subset(Rhin,!(Rhin$annee=="2004"&Rhin$mois=="1"|Rhin$annee=="2004"&Rhin$mois=="2"|Rhin$annee=="2004"&Rhin$mois=="3"|Rhin$annee=="2004"&Rhin$mois=="4"))

# Noms des sites : 
    # Pour les 3 sites : 
unique(Rhin$site)
Rhin[,1] <- tolower(Rhin[,1])

  # Pour les "intra-sites"
colnames(Rhin)[20] <- "intra_site"
Rhin[,20] <- tolower(Rhin[,20])
Rhin[,20] <- iconv(Rhin[,20], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Rhin[,20] <- gsub("\\+","_",Rhin[,20])

unique(Rhin$intra_site)
  # Attention : au niveau des intra-sites : vieux rhin + grande alsace (agrégés) & vieux rhin et grande alsace (séparé)
# Retirer les intra-sites non renseignés (si on choisit celle échelle) : 
  # -> Rhin <- subset(Rhin, !(Rhin$intra_site==""))

#Vérification des abondances : 
colnames(Rhin) [31] <- "abondance"
unique(Rhin$abondance)
# -> Pas de 0 : on ne note pas les absences ? + pas de NC / NA 

#Vérification des doublons/double passage : 

duplicated(Rhin) # ok
duplicated(Rhin[c(1000:2000),]) # ok
duplicated(Rhin[c(2000:3000),]) # ok
duplicated(Rhin[c(3000:3864),]) # Erreur saisie d'entrée : 
# Deux saisies pour Secteur sud le 13/09/2022 ; Rhinland le 13/09/2022 ; Base nautique ; 13/09/2022 

help("distinct")
Rhin <- distinct(Rhin)

#Ajout du nom du secteur : 

Rhin$secteur <- "reserve_du_rhin"

# Ajout protocole : 
unique(Rhin$protocole)
colnames(Rhin) [40] <- "suivi"

Rhin$protocole <- "terrestre"

#Prendre en compte les remarques : 
unique(Rhin$liste_complete__)
colnames(Rhin) [37] <- "remarques"

unique(Rhin$remarques)

Rhin$qualite_comptage <- with(Rhin, ifelse(Rhin$liste_complete__=="0","douteux",
                                    ifelse(Rhin$remarques=="Participants : Carole BIZART, Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Victor ROUAULT. Météo : 10 à 15 cm de neige au sol; redoux en cours, avec températures devenant légèrement positives en journée. Ciel couvert. Quelques faibles pluies et neige mêlées, puis quelques faibles pluies éparses.","douteux",
                                    ifelse(Rhin$remarques=="Sous évalué en raison du vent, de nombreux individus sont à l'abri du vent derrière la digue tiroir ou ailleurs","douteux",
                                  ifelse(Rhin$remarques=="Mauvaises conditions d'observation. Total peut-être sous-évalué.","douteux","ok")))))






              ################### FUSION TABLEAU DONNEES ##############
help("rbind")
help("bind_rows")

Camargue$abondance <- as.character(Camargue$abondance)
Cotentin$abondance <- as.character(Cotentin$abondance)
Baie$abondance <- as.character(Baie$abondance)
Rhin$abondance <- as.character(Rhin$abondance)
data$coef_de_marree <- as.character(data$coef_de_marree)
data_f <- bind_rows(data,Camargue,Cotentin,Baie,Arcachon,Rhin)



        ############### Ajouter les véritables absences au jeu de données #####


# 1. Création d'un identifiant pour compiler les tables : 
id <- paste(data_f$site,data_f$date)

# 2. Création de la table "site" : 
site <- data.frame(id, data_f$site,data_f$secteur,data_f$protocole)
site <- unique(site) 

# 3. Création de la table inventaire : 

inv <- data.frame(id,data_f$date,data_f$obs)
  # -> ajouter les mois + années +jours juliens
inv$date_jj <- yday(inv$data_f.date)
inv$mois <- month(inv$data_f.date)
inv$annee <- year(inv$data_f.date)
inv <- unique(inv)

  # Compilation des deux tables : 
data_inv <- merge(site,inv,by.x = "id", by.y = "id")

# 4. Création de la table comptage (table d'observation) : 
  # -> création d'un id pour fusionner les tables (avec les espèces)
data_f$id <- paste(data_f$espece,data_f$site,data_f$date)

  #Création du tableau inventaire qu'on va croiser avec le jeu de données : 

data_f$id <- paste(data_f$site,data_f$date)
id <- unique(data_f$id)
sp <- unique(data_f$espece)

inventaire <- expand.grid(id, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste(inventaire$Var2,inventaire$Var1)

# Combinaison des deux tableaux : 

data_f <- merge(inventaire, data_f, by.x = "id_sp", by.y = "id", all.x = T)
View(data)

# Remplacement des NA par des 0
data_f$abondace[is.na(data_f$abondance)] = 0
View(data)

# On peut maintenant retirer les "anciennes" colonnes pour date, secteur et espece et ID 
  # pour obtenir la table observation : 

# Var 2 -> espece :
colnames(data)[names(data)== "Var2"] <- "espece"
colnames(data)[names(data)== "Var1"] <- "id"

# On remet les noms latins + famille + ordre

data <- merge(data,espece, by.x = "espece", by.y = "french_name")
data <- data[,-c(4,6,7:15)]

# Combinaison de tous les table : 

data_fin <- merge(data_f,data_inv,by.x="id",by.y="id")
unique(data_fin$espece)


## Voir pour les doubles comptages

                                        # [RL] conseils :
                                        # [RL] 1- créer une table site (id, nom, caratéristique,...)
                                        # [RL] 2- créer une table inventaire (id, id_site,date, annee, mois,jour_julien,observateur,...)
                                        # [RL] 3- tu as ta table observation (id_inventaire, espece, abondance)




 
















# Ajout des tableaux : 

data <- rbind.fill(data,Camargue,Cotentin,Baie)
unique(data$secteur)


