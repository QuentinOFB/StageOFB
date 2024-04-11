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

data$obs[data$obs==" pilvin; cabelguen; chery"] <- "pilvin, cabelguen, chery"
data$obs[data$obs=="allain j.p; bodineau p; cabelguen j; drouyer l"] <- "allain, bodineau, cabelguen, drouyer"
data$obs[data$obs=="lacourpaille; potiron"] <- "lacourpaille, potiron"
data$obs[data$obs=="allain jp, cabelguen j, chery g, drouyer l, pilvin d"] <- "allain, cabelguen, chery, drouyer, pilvin"
data$obs[data$obs=="cabelguen j, pilvin d."] <- "cabelguen, pilvin"
data$obs[data$obs=="gaigeard c, pilvin d."] <- "gaigeard, pilvin"
data$obs[data$obs=="bazus, cabelguen, drouyer"] <- "bazus, cabelguen, drouyer"
data$obs[data$obs==" pilvin, potiron"] <- "pilvin, potiron"
data$obs[data$obs==" bazus, cabelguen, leray, potiron"] <- "bazus, cabelguen, leray, potiron"
data$obs[data$obs==" allain jp, drouyer l, gaigeard c, pilvin d, cabelguen j"] <- "allain, drouyer, gaigeard, pilvin, cabelguen"
data$obs[data$obs=="potiron jl, lacourpaille d"] <- "potiron, lacourpaille"
data$obs[data$obs=="pilvin, drouyer et potiron"] <- "pilvin, drouyer, potiron"
data$obs[data$obs=="jp allain, l drouyer, d pilvin, j cabelguen, blandine"] <- "allain, drouyer, pilvin, cabelguen, blandine"
data$obs[data$obs=="pilvin&cabelguen"] <- "pilvin, cabelguen"
data$obs[data$obs=="drouyer, lacourpaille potiron"] <- "drouyer, lacourpaille, potiron"
data$obs[data$obs==" drouyer, potiron, vergereau"] <- "drouyer, potiron, vergereau"
data$obs[data$obs==" allain, cabelguen, becot, latraube, gueguen,vergereau, potiron, lefebvre, pilvin"] <- "allain, cabelguen, becot, latraube, gueguen,vergereau, potiron, lefebvre, pilvin"
data$obs[data$obs=="cabelguen, drouyer, becot, gueguen, lefebvre, latraube, gautier, gaigeard, chery."] <- "cabelguen, drouyer, becot, gueguen, lefebvre, latraube, gautier, gaigeard, chery"
data$obs[data$obs==" drouyer, latraube, becot, yesou, lefebvre, vergereau, pilvin , vinet"] <- "drouyer, latraube, becot, yesou, lefebvre, vergereau, pilvin ,vinet"
data$obs[data$obs==" chil + cochard + chery"] <- "chil, cochard, chery"
data$obs[data$obs=="cochard + chery"] <- "cochard, chery"
data$obs[data$obs=="friconneau + lacourpaille"] <- "friconneau, lacourpaille"
data$obs[data$obs=="drouyer + pilvin + latraube + charbonnier"] <- "drouyer, pilvin, latraube, charbonnier"
data$obs[data$obs=="latraube+charbonnier+cochard"] <- "latraube, charbonnier, cochard"
data$obs[data$obs=="jf maillard"] <- "maillard"
data$obs[data$obs=="g. cochard"] <- "cochard"
data$obs[data$obs=="frelon marc"] <- "frelon"
data$obs[data$obs=="maillard; latraube"] <- "maillard, latraube"
data$obs[data$obs=="cochard,maillard"] <- "cochard, maillard"
data$obs[data$obs=="chil,potiron"] <- "chil, potiron"
data$obs[data$obs=="cochard,drouyer,pilvin"] <- "cochard, drouyer, pilvin"
data$obs[data$obs=="drouyer, becot, pilvin, dupont christian"] <- "drouyer, becot, pilvin, dupont, christian"
data$obs[data$obs=="chil (comptage pas dissocie de st nicolas)"] <- "chil"
data$obs[data$obs=="cochard, becot, pilvin drouyer"] <- "cochard, becot, pilvin, drouyer"
data$obs[data$obs=="drouyer, pilvin becot"] <- "drouyer, pilvin, becot"
data$obs[data$obs=="cochard; chil"] <- "cochard, chil"
data$obs[data$obs=="becot; pilvin; drouyer"] <- "becot, pilvin, drouyer"
data$obs[data$obs=="drouyer, becot pilvin"] <- "drouyer, becot, pilvinr"
data$obs[data$obs=="cochard g, leduc, a."] <- "cochard, leduc"
data$obs[data$obs=="yesou p."] <- "yesou"
data$obs[data$obs=="cochard g., leduc a."] <- "cochard, leduc"
data$obs[data$obs=="cochard g, yesou p., leduc a."] <- "cochard, yesou, leduc"
data$obs[data$obs=="becot m., drouyer l., pilvin d."] <- "becot, drouyer, pilvin"
data$obs[data$obs=="cochard g."] <- "cochard"
data$obs[data$obs=="chery g."] <- "chery"
data$obs[data$obs=="cochard g.; lacourpaille d."] <- "cochard, lacourpaille"
data$obs[data$obs=="maillard j-f"] <- "maillard"
data$obs[data$obs=="yesou p"] <- "yesou"
data$obs[data$obs=="cochard g"] <- "cochard"
data$obs[data$obs=="cochard g, yesou p. "] <- "cochard, yesou"
data$obs[data$obs=="becot m, pilvin d, drouyer l"] <- "becot, pilvin, drouyer"
data$obs[data$obs=="cochard g., maillard j-f."] <- "cochard g., maillard j-f."
data$obs[data$obs=="yesou p, cochard g"] <- "yesou, cochard"
data$obs[data$obs=="cochard g, maillard j-f"] <- "cochard, maillard"
data$obs[data$obs=="monin p, cochard g"] <- "monin, cochard"
data$obs[data$obs=="yesou p, monin p, cochard g"] <- "yesou, monin, cochard"
data$obs[data$obs=="monin p, maillard j-f"] <- "monin, maillard"
data$obs[data$obs=="chil j-l"] <- "chil"
data$obs[data$obs=="monin p"] <- "monin"
data$obs[data$obs=="yesou p, monin p"] <- "yesou, monin"
data$obs[data$obs=="monin p, clero n, raymond b"] <- "monin, clero, raymond"
data$obs[data$obs=="becot m, pilvin d, chil j-l"] <- "becot, pilvin, chil"
data$obs[data$obs=="gaigeard c."] <- "gaigeard"
data$obs[data$obs=="chil j-l,"] <- "chil"
data$obs[data$obs=="guenezan m., monin p"] <- "guenezan, monin"
data$obs[data$obs=="yesou p, guenezan m, monin p"] <- "yesou, guenezan, monin"
data$obs[data$obs=="becot m, drouyer l, pilvin d"] <- "becot, drouyer, pilvin"
data$obs[data$obs=="maillard j.f"] <- "maillard"
data$obs[data$obs=="gaigeard c"] <- "gaigeard"
data$obs[data$obs==" guenezan m"] <- "guenezan"
data$obs[data$obs=="guenezan m., maillard j.f"] <- "guenezan, maillard"
data$obs[data$obs=="chil j.l"] <- "chil"
data$obs[data$obs==" guenezan m "] <- "guenezan"
data$obs[data$obs==" guenezan m - chil j.l"] <- "guenezan, chil"
data$obs[data$obs=="guenezan m."] <- "guenezan"
data$obs[data$obs=="guenezan m, maillard j.f, gallais r"] <- "guenezan, maillard, gallais"
data$obs[data$obs=="potiron j-l"] <- "potiron"
data$obs[data$obs==" guenezan m - maillard j-f"] <- "guenezan, maillard"
data$obs[data$obs=="guenezan m, maillard j.f"] <- "guenezan, maillard"
data$obs[data$obs=="yesou pierre"] <- "yesou"
data$obs[data$obs==" guenezan m - yesou p"] <- "guenezan, yesou"
data$obs[data$obs=="becot m, pilvin d, maillard j-f"] <- "becot, pilvin, maillard"
data$obs[data$obs=="bodin r"] <- "bodin"
data$obs[data$obs=="lacourpaille d"] <- "lacourpaille"
data$obs[data$obs==" guenezan m - lacourpaille d"] <- "guenezan, lacourpaille"
data$obs[data$obs=="becot m, pilvin d, le baut e"] <- "becot, pilvin, le baut"
data$obs[data$obs==" guenezan m - bodin r"] <- "guenezan, bodin"
data$obs[data$obs=="becot m, pilvin d, cabelguen j, gelinaud g"] <- "becot, pilvin, cabelguen, gelinaud"
data$obs[data$obs==" guenezan m - maillard j.f"] <- "guenezan, maillard"
data$obs[data$obs=="becot m, pilvin d, r bodin"] <- "becot, pilvin, bodin"
data$obs[data$obs=="guenezan m"] <- "guenezan"
data$obs[data$obs==" guenezan m - chil j-l"] <- "guenezan, chil"
data$obs[data$obs=="guenezan m, maillard j-f"] <- "guenezan, maillard"
data$obs[data$obs=="potiron j-l lacourpaille d"] <- "potiron, lacourpaille"
data$obs[data$obs==" guenezan m - "] <- "guenezan"
data$obs[data$obs=="becot m, pilvin d, mussier f"] <- "becot, pilvin, mussier"
data$obs[data$obs=="guenezan m, le rest k"] <- "guenezan, le rest"
data$obs[data$obs==" guenezan m, nebbache m"] <- "guenezan, nebbache"
data$obs[data$obs==" guenezan m - chil j-l, nebbache m"] <- "guenezan, chil, nebbache"
data$obs[data$obs=="guenezan m, nebbache m"] <- "guenezan, nebbache"
data$obs[data$obs=="guenezan m, le rest k, gossmann f, nebbache m"] <- "guenezan, le rest, gossmann, nebbache"
data$obs[data$obs=="bodin r, drouyer l"] <- "bodin, drouyer"
data$obs[data$obs==" guenezan m, "] <- "guenezan"
data$obs[data$obs=="becot m, pilvin d, "] <- "becot, pilvin"
data$obs[data$obs=="becot m, pilvin d, bodin r"] <- "becot, pilvin, bodin"
data$obs[data$obs=="potiron j.l"] <- "potiron"
data$obs[data$obs==" guenezan m - lacourpaille denis"] <- "guenezan, lacourpaille"
data$obs[data$obs=="chil j.l et le rest k"] <- "chil, le rest"
data$obs[data$obs=="le baut e et gossmann f"] <- "le baut, gossmann"
data$obs[data$obs=="le rest kevin barbotin aurelie"] <- "le rest, barbotin"
data$obs[data$obs=="guenezan m le rest kevin barbotin aurelie"] <- "guenezan, le rest, barbotin"
data$obs[data$obs=="becot m, gossmann f, pilvin d"] <- "becot, gossmann, pilvin"
data$obs[data$obs=="guenezan m dudouet cyr"] <- "guenezan, dudouet"
data$obs[data$obs=="le rest k"] <- "le rest"
data$obs[data$obs=="guenezan m, chil j-l"] <- "guenezan, chil"
data$obs[data$obs=="gossmann f"] <- "gossmann"
data$obs[data$obs=="gossmann f, le rest k"] <- "gossmann, le rest"
data$obs[data$obs=="becot m, bodin r, pilvin d"] <- "becot, bodin, pilvin"
data$obs[data$obs=="guenezan m, potiron j.l"] <- "guenezan, potiron"
data$obs[data$obs=="becot m, chil j.l, pilvin d"] <- "becot, chil, pilvin"
data$obs[data$obs=="guenezan m, potiron j.l, pilvin d"] <- "guenezan, potiron, pilvin"
data$obs[data$obs=="guenezan m, lacourpaille d"] <- "guenezan, lacourpaille"
data$obs[data$obs=="becot m, bodin r.l, pilvin d"] <- "becot, bodin, pilvin"
data$obs[data$obs=="guenezan m, mussier f"] <- "guenezan, mussier"
data$obs[data$obs=="le rest k - boussac l"] <- "le rest, boussac"
data$obs[data$obs=="guenezan m, souchay g"] <- "guenezan, souchay"
data$obs[data$obs=="becot m, le baut e, chil j-l"] <- "becot, le baut, chil"
data$obs[data$obs=="guenezan m, le rest k, lacourpaille d"] <- "guenezan, le rest, lacourpaille"
data$obs[data$obs=="guenezan m, le rest k, mussier f"] <- "guenezan, le rest, mussier"
data$obs[data$obs=="le baut e - bourdeau d"] <- "le baut, bourdeau"
data$obs[data$obs=="guenezan m, citoleux j"] <- "guenezan, citoleux"
data$obs[data$obs=="guenezan m,"] <- "guenezan"
data$obs[data$obs=="becot m, le baut eric, pilvin d"] <- "becot, le baut, pilvin"
data$obs[data$obs=="potiron j-l, joly j"] <- "potiron, joly"
data$obs[data$obs=="gaigeard c, boniface v"] <- "gaigeard, boniface"
data$obs[data$obs=="becot m, chil j-l, le rest k"] <- "becot, chil, le rest"
data$obs[data$obs=="becot m, pilvin d, hole g"] <- "becot, pilvin, hole"
data$obs[data$obs=="passerault m"] <- "passerault"
data$obs[data$obs=="le rest kevin"] <- "le rest"
data$obs[data$obs=="becot m, hole g, chil j-l"] <- "becot, holle, chil"
data$obs[data$obs=="lecomte m"] <- "lecomte"
data$obs[data$obs=="becot m, chil j-l, holle g"] <- "becot, chil, holle"
data$obs[data$obs=="guenezan m, raymond b"] <- "guenezan, raymond"
data$obs[data$obs=="becot m, chil j-l, bodin r"] <- "becot, chil, bodin"
data$obs[data$obs=="joly jules"] <- "joly"
data$obs[data$obs=="holle g, passereault m"] <- "holle, passereault"
data$obs[data$obs=="guenezan m, petiteau f"] <- "guenezan, petiteau"
data$obs[data$obs=="becot m, chil j-l, gaetano b"] <- "becot, chil, gaetano"
data$obs[data$obs=="lecomte muriel"] <- "lecomte"
data$obs[data$obs=="becot m, chil j-l, bodin r, passerault m"] <- "becot, chil, bodin, passerault"
data$obs[data$obs=="guenezan m fourage c"] <- "guenezan, fourage"
data$obs[data$obs=="potiron jean-luc potier justin"] <- "potiron, potier"
data$obs[data$obs=="becot m, chil j-l, bodin r, sacier b"] <- "becot, chil, bodin, sacier"
data$obs[data$obs=="guenezan m "] <- "guenezan"
data$obs[data$obs=="guenezan michel"] <- "guenezan"
data$obs[data$obs=="becot m, chil j-l, brunel bruno"] <- "becot, chil, brunel"
data$obs[data$obs=="becot m, chil j-l, brunel bruno"] <- "becot, chil, brunel"


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

Camargue$espece[Camargue$espece=="duck_ind."] <- "canard_sp"

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

Camargue$obs[Camargue$obs=="at"] <- "alain_tamisier"
Camargue$obs[Camargue$obs=="jbm"] <- "jean_baptiste_mouronval"
Camargue$obs[Camargue$obs=="mgc"] <- "michel_gauthier_clerc"

# -> Avoir les noms vernaculaires des espèces : 
espece <- read.csv("Data/espece.csv")
espece[,3] <- tolower(espece[,3])
espece[,3] <- gsub(" ","_",espece[,3])
# Prendre "scientific name 2 car il y a d'anciens noms latins dans le jeu de données Camargue : 
#  Mareca penelope et mareca strepera 
# Attention au nom latin du cygne de Bewick : 
Camargue[,8] <- gsub("cygnus_columbianus_bewickii","cygnus_columbianus",Camargue[,8])
Camargue <- merge(Camargue,espece, by.x = "espece", by.y = "scientific_name_2", all.x = T)
unique(Camargue$espece)
# -> Retirer la foulque macroule : 
Camargue <- subset(Camargue,!(Camargue$espece=="fulica_atra"))

Camargue$order_tax[Camargue$espece=="canard_sp"] <- "Ansériformes"
Camargue$family_tax[Camargue$espece=="canard_sp"] <- "Anatidés"
Camargue$grp_fonctionnel <- "anatidae"

#Retirer les colonnes dont on a pas besoin : 
Camargue <- Camargue[,-c(1,2,4,11,12,13,14,16,18:25)]

Camargue[,10] <- tolower(Camargue[,10])
Camargue[,10] <- iconv(Camargue[,10], from = 'UTF-8', to = 'ASCII//TRANSLIT')

Camargue[,11] <- tolower(Camargue[,11])
Camargue[,11] <- iconv(Camargue[,11], from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Remettre les noms des obs et des colonnes au propre 
colnames(Camargue)[9] <- "espece"
Camargue[,9] <- tolower(Camargue[,9])
Camargue[,9] <- gsub(" ","_",Camargue[,9])

Camargue[,8] <- tolower(Camargue[,8])
Camargue[,8] <- gsub(" ","_",Camargue[,8])

#Création d'une colonne "secteur" pour la Camargue : 
Camargue$secteur <- "camargue"

#Sélection des années qui correspondent avec celle de l'estuaire de la Loire 
# à partir de 2004-2005 
# Sélectionner saison 2004 : 
unique(Camargue$annee)
# Enlever les dix premières années : problème lié à une méthodologie changeante 
Camargue <- subset(Camargue, !(Camargue$saison<1986))
unique(Camargue$date)

#Création d'une colonne site retenu :

nb_suivi_site <-  
  Camargue %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

Camargue <- merge(Camargue,nb_suivi_site, by.x = "site", by.y = "site")

colnames(Camargue)[14] <- "occurence_site"

Camargue$site_retenu <- with(Camargue, ifelse(Camargue$saison < 2004,"non","oui"))

# NA Dans les abondances, pour le coups il s'agit de vrais NA, dans la mesure ou quand une espèce n'est pas comptée elle est notée
unique(Camargue$abondance)

#Faut-il retirer ces données NA ? 
Camargue <- subset(Camargue, !(Camargue$abondance=="NA"))

#Colonne sur les observations des espèces : 
Camargue$abondance <- as.numeric(Camargue$abondance)

median_ab <- Camargue %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Camargue$id_ab <- paste(Camargue$espece,Camargue$site,Camargue$mois,Camargue$annee)

Camargue <- merge(Camargue,median_ab,by.x = "id_ab",by.y="id")

#Ajout colonne protocole : 
Camargue$protocole <- "avion"

#Ajout de la voie de migration :

Camargue$voie_migr <- "est_atlantique/mediterranee"

Camargue <- Camargue[,-c(1,17,18,19,20)]
colnames(Camargue) [1] <- "site"
colnames(Camargue) [3] <- "mois"
colnames(Camargue) [4] <- "annee"
colnames(Camargue) [9] <- "espece"

#Nombre d'observation des espèces 
nb_observation <- Camargue %>%
  count(espece)

Camargue <- merge(Camargue,nb_observation, by.x = "espece",by.y = "espece")
colnames(Camargue)[22] <- "nb_observations"

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

Baie[,3] <- gsub("barge_sp","barges_sp",Baie[,3])

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

Baie$site[Baie$site=="la_mare_a_2000"] <- "mare_a_2000"

# -> Ne sélectionner que les sites avec plus de 3 comptages sur plusieurs années : 

nb_suivi_site <-  
  Baie %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

Baie <- merge(Baie,nb_suivi_site, by.x = "site", by.y = "site")

colnames(Baie)[22] <- "occurence_site"

# Création d'une colonne site retenu 

Baie$site_retenu <- with(Baie, ifelse(Baie$annee < 2004,"non",
                                ifelse(Baie$occurence_site<3,"non","oui")))

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

#Voir les abondance : 
unique(Baie$abondance)
unique(Baie$abondance[1000:1595])

#Présence de NA : les supprimer ? (correspond à une "non prospection")
Baie <- subset(Baie,!(Baie$abondance=="NA"))

#Création d'une colonne observations pour les espèces : 
Baie$abondance <- as.numeric(Baie$abondance)

median_ab <- Baie %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Baie$id_ab <- paste(Baie$espece,Baie$site,Baie$mois,Baie$annee)

Baie <- merge(Baie,median_ab,by.x = "id_ab",by.y="id")
#Remise les noms au propre :
colnames(Baie) [2] <- "site"
colnames(Baie) [5] <- "espece"
colnames(Baie) [19] <- "mois"
colnames(Baie) [20] <- "annee"


#Prendre en compte les remarques : 
unique(Baie$remarques)
Baie[,15]<- iconv(Baie[,15], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Baie[,15] <- gsub(" ","_",Baie[,15])
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
Baie <- Baie[,-c(1,3,4,6,8,10,11,12,16,17,21,22,25,26,27)]

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

#Création colonne groupe fonctionnel :
Baie$grp_fonctionnel <- with(Baie, ifelse(Baie$family_tax=="Anatidae","anatidae","limicole"))

#Création colonne voie de migration : 
Baie$voie_migr <- "est_atlantique"

#Nombre d'observation des espèces 
nb_observation <- Baie %>%
  count(espece)

Baie <- merge(Baie,nb_observation, by.x = "espece",by.y = "espece")
colnames(Baie)[22] <- "nb_observations"     


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

Cotentin <- subset(Cotentin,!(Cotentin$espece=="")) 

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

#Cotentin <- subset(Cotentin, !(Cotentin$site=="polder_ste_marie_cel"))
unique(Cotentin$site)

#Supprimer les données agrégées 2004 et 2008 : 
#Cotentin <- subset(Cotentin, !(Cotentin$site=="rnn_beauguillot"))

Cotentin$annee <- year(Cotentin$date)
Cotentin$mois <- month(Cotentin$date)

unique(Cotentin$annee) #De 2004 à 2023
unique(Cotentin$mois)

nb_suivi_site <-  
  Cotentin %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

Cotentin <- merge(Cotentin,nb_suivi_site, by.x = "site", by.y = "site")

colnames(Cotentin)[15] <- "occurence_site"

# Création d'une colonne site retenu 


Cotentin$site_retenu <- with(Cotentin, ifelse(Cotentin$site=="polder_ste_marie_cel","non",
                                    ifelse(Cotentin$site=="rnn_beauguillot","non",      
                                    ifelse(Cotentin$occurence_site < 3,"non","oui"))))
  
#Vérification abondance : 
unique(Cotentin$abondance)
# Attention à certains moments des fourchettes sont données ex : 420-440 
# Que faire ? Prendre la moyenne, le min, le max, ou supprimer ? 

Cotentin$abondance[Cotentin$abondance=="420-440"] <- "440"
Cotentin$abondance[Cotentin$abondance=="380-420"] <- "420"
Cotentin$abondance[Cotentin$abondance=="15-20"] <- "20"


Cotentin$abondance <- as.numeric(Cotentin$abondance)

median_ab <- Cotentin %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Cotentin$id_ab <- paste(Cotentin$espece,Cotentin$site,Cotentin$mois,Cotentin$annee)

Cotentin <- merge(Cotentin,median_ab,by.x = "id_ab",by.y="id")

#Nombre d'observation des espèces : 


#Remettre en forme les noms des colonnes :
colnames(Cotentin) [2] <- "site"
colnames(Cotentin) [5] <- "espece"
colnames(Cotentin) [14] <- "mois"
colnames(Cotentin) [15] <- "annee"

#####Prendre en compte les remarques :
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

#Ajout colonne voie migration : 
Cotentin$voie_migr <- "est_atlantique"

#Ajout colonne groupe fonctionnel :
Cotentin$grp_fonctionnel <- with(Cotentin, ifelse(Cotentin$family_tax=="anatidae","anatidae","limicole"))
unique(Cotentin$family_tax)

#Tri final des colonnes : 
Cotentin <- Cotentin[,-c(1,11,18,19,20,21)]

#Nombre d'observation des espèces 
nb_observation <- Cotentin %>%
  count(espece)

Cotentin <- merge(Cotentin,nb_observation, by.x = "espece",by.y = "espece")
colnames(Cotentin)[25] <- "nb_observations"



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

Arcachon <- subset(Arcachon, !(Arcachon$espece=="aigrette_des_recifs"|Arcachon$espece=="heron_cendre"|Arcachon$espece=="spatule_blanche"|Arcachon$espece==""))

# données sp + "Gravelot ou becasseau" 

Arcachon$espece[Arcachon$espece=="barge_sp"] <- "barges_sp"
Arcachon$espece[Arcachon$espece=="becasseau_ou_gravelot"]<- "limicole_sp"

# Format de date : 
unique(Arcachon$date)
# Enlever les dates où pas de comptage à cause des mauvaises conditions météo : 
Arcachon <- subset(Arcachon,!(Arcachon$date=="Octobre, aucun comptage tempêtes successives"|Arcachon$date=="Pas de comptage météo horrible (non représentatif)"))
Arcachon$date <- dmy(Arcachon$date)

#Ajouter le mois : en "chiffre"
Arcachon$mois <- month(Arcachon$date)
unique(Arcachon$mois)

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

Arcachon <- subset(Arcachon,!(Arcachon$abondance=="NC"|Arcachon$abondance==""))

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
unique(Arcachon$derangement)
Arcachon$qualite_comptage <- with(Arcachon, ifelse(Arcachon$remarques=="dérangements promeneurs, vélos, chiens, plagistes","douteux",
                                            ifelse(Arcachon$remarques=="que des chiens","douteux",
                                            ifelse(Arcachon$remarques=="dérangement: engins nautiques motorisés et non motorisés","douteux",
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
                                              ifelse(Arcachon$remarques=="Dérangement ","douteux",
                                              ifelse(Arcachon$derangement=="Attaque de chien","douteux",
                                               ifelse(Arcachon$derangement=="kite surf + chasse","douteux",
                                              ifelse(Arcachon$derangement=="Dérangement bateau chasseur","douteux",
                                              ifelse(Arcachon$derangement=="Surcote","douteux",
                                              ifelse(Arcachon$derangement=="Attaque de Faucon Pellerin, gros envol","douteux",
                                              ifelse(Arcachon$derangement=="nbx promeneurs","douteux",
                                              ifelse(Arcachon$derangement=="Engin nautique motorisé et non mototrisé","douteux","ok")))))))))))))))))))))))))))))))))))

#Ajout colonne suivi site : 

nb_suivi_site <-  
  Arcachon %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

Arcachon <- merge(Arcachon,nb_suivi_site, by.x = "site", by.y = "site")

colnames(Arcachon)[24] <- "occurence_site"

#Colonne site retenu: 
Arcachon$site_retenu <- with(Arcachon, ifelse(Arcachon$occurence_site < 3, "non","oui"))

#Colonne nombre d'observation espèces : 
nb_observation <- Arcachon %>%
  count(espece)

Arcachon <- merge(Arcachon,nb_observation, by.x = "espece",by.y = "espece")
colnames(Arcachon)[26] <- "nb_observations"

#Colonne valeur médiane, moy ... 
Arcachon$abondance <- as.numeric(Arcachon$abondance)

median_ab <- Arcachon %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Arcachon$id_ab <- paste(Arcachon$espece,Arcachon$site,Arcachon$mois,Arcachon$annee)

Arcachon <- merge(Arcachon,median_ab,by.x = "id_ab",by.y="id")

#On renomme bien les colonnes : 
colnames(Arcachon)[2] <- "espece"
colnames(Arcachon)[3] <- "site"
colnames(Arcachon)[5] <- "annee"
colnames(Arcachon)[21] <- "mois"

#Tri final des colonnes : 
Arcachon <- Arcachon[,-c(1,6,28,29,30,31)]

# Voie migration 
Arcachon$voie_migr <- "est_atlantique"

#Groupe fonctionnel 
Arcachon$grp_fonctionnel <- "limicoles"

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
# 
Rhin$espece[Rhin$espece=="becasseau_indetermine"] <- "becasseau_sp"
Rhin$espece[Rhin$espece=="chevalier_indetermine_(tringa)"] <- "chevalier_sp"
Rhin$espece[Rhin$espece=="gravelot_indetermine"] <- "gravelot_sp"
Rhin$espece[Rhin$espece=="oie_indeterminee"] <- "oie_sp"

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

# Noms des sites : 
    # Pour les 3 sites : 
unique(Rhin$site)
colnames(Rhin) [1] <- "grand_site"
Rhin[,1] <- tolower(Rhin[,1])

  # Pour les "intra-sites"
colnames(Rhin)[20] <- "site"
Rhin[,20] <- tolower(Rhin[,20])
Rhin[,20] <- iconv(Rhin[,20], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Rhin[,20] <- gsub("\\+","_",Rhin[,20])

unique(Rhin$site)
  # Attention : au niveau des intra-sites : vieux rhin + grande alsace (agrégés) & vieux rhin et grande alsace (séparé)

  # -> Si on choisit de ne pas aggréger les "intra_sites" on perd 800 données syr les 4733 concernant anatidés/limicoles
  # Uniquement des données au mois de janvier entre 2001 et 2013  


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

#Ajouter une colonne observateurs : 

Rhin$obs <- ""
Rhin$obs[Rhin$commentaire_de_la_liste=="Frederic Deck, Marc Brignon, Daniel Kirmser"] <- "Frederic Deck, Marc Brignon, Daniel Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel"] <- "Eric Buchel"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, A. Chabrolle, JP. Hiss, N. Hoffmann, JP. Risse + J. Rupp, B. Disch"] <-  "Jean-Marc Bronner, A. Chabrolle, Jean-Pierre Hiss,  Nicolas Hoffmann, JP. Risse, Jürgen Rupp, Bernhardt Disch"
Rhin$obs[Rhin$commentaire_de_la_liste=="Sébastien Didier, Raynald Moratin, Daniel Kirmser"] <- "Sébastien Didier, Raynald Moratin, Daniel Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Jean-Pierre Hiss, Jean-Marie Risse, Jürgen Rupp"] <- "Jean-Marc Bronner, Jean-Pierre Hiss, Jean-Marie Risse, Jürgen Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel, Pascale David"] <- "Eric Buchel, Pascale David"
Rhin$obs[Rhin$commentaire_de_la_liste=="Alexandre Goncalvès, Christophe Rahier, Daniel Kirmser"] <- "Alexandre Goncalvès, Christophe Rahier, Daniel Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Marion Bailleul, Eric Brunissen, Alexanfre Goncalves, Stéphane Uhmang, Daniel Kirmser"] <- "Marion Bailleul, Eric Brunissen, Alexanfre Goncalves, Stéphane Uhmang, Daniel Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Risse, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand "] <- "Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Riss, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Risse, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand, Jean-Luc Wilhelm (ONCFS), Françoise Sieffert"] <- "Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Riss, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand, Jean-Luc Wilhelm (ONCFS), Françoise Sieffert"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel, Sandrine Kech"] <- "Eric Buchel, Sandrine Kech"
Rhin$obs[Rhin$commentaire_de_la_liste=="Antoine Aït-Saîdi, Eric Brunissen, Stéphane Uhmang, Daniel Kirmser, Frédéric Deck"] <- "Antoine Aït-Saîdi, Eric Brunissen, Stéphane Uhmang, Daniel Kirmser, Frédéric Deck"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Riss, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand"] <- "Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Riss, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand"
Rhin$obs[Rhin$commentaire_de_la_liste=="Marion Bailleul, Eric Brunissen, Frédéric Deck, Daniel Kirmser, Christophe Rahier, Julie Roux, Stéphane Umhang"] <- "Marion Bailleul, Eric Brunissen, Frédéric Deck, Daniel Kirmser, Christophe Rahier, Julie Roux, Stéphane Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, JM Risse, JP Hiss, Nicolas Hoffmann + compteurs allemands (Jürgen Rupp, Bernhard Disch, Jochem Wiegand) + 2 agents ONCFS (Dominique Cronimus, Jean-Dominique Veaux)"] <- "Jean-Marc Bronner, JM Risse, Jean-Pierre Hiss, Nicolas Hoffmann, Jürgen Rupp, Bernhard Disch, Jochem Wiegand, Dominique Cronimus, Jean-Dominique Veaux"
Rhin$obs[Rhin$commentaire_de_la_liste=="Laurent Waeffler, Sébastien Glas, Sandrine Kech"] <- "Laurent Waeffler, Sébastien Glas, Sandrine Kech"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser, Eric Brunissen, Christophe Rahier"] <- "Daniel Kirmser, Eric Brunissen, Christophe Rahier"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, JP Hiss, Nicolas Hoffmann + compteurs allemands (Jürgen Rupp, Bernhard Disch, F. Rau, L. Leib et JY Follet) + Gilles Nonnenmacher et Vivien Siat (ONCFS)"] <- "Jean-Marc Bronner, Jean-Pierre Hiss, Nicolas Hoffmann, Jürgen Rupp, Bernhard Disch, F. Rau, L. Leib et JY Follet, Gilles Nonnenmacher et Vivien Siat (ONCFS)"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\nde l'entrée du plan d'eau au pK 280.5"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\ndu pK 280.5 au 178.5"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\nDu pK179.5 à la ligne de bouées sud"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Vieux-Rhin à Gerstheim : entre ouvrage de répartition et seuil 1\nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Vieux-Rhin à Gerstheim : entre seuil 1 et seuil 2\nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Vieux-Rhin à Gerstheim : entre seuil 2 et confluence\nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal aval à Gerstheim \nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "contre-canal à Gerstheim\nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal amont à Gerstheim \nE. Hornier et JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "canal amont écluse et centrale + musoir\nE. Hornier JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "canal amont vannes de décharge + polder VNF\nE.Hornier JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Aval barrage Gambsheim\nE.Hornier JL Wilhelm"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "E. Hornier Jean-Luc Wilhelm\nBeau temps - vent nul 1030 hpa\n8-15° Niveau plan d'eau très bas"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre Barrage agricole et seuil 1, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre seuil 1 et seuil 2, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre seuil 2 et confluence, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal de fuite Aval barrage hydro et écluse , Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal amenée amont barrage hydro et écluse, bonnes conditions d'observation, 29°, léger vent, beau (Erwan Hornier)"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Contre-canal à hauteur de l'île de Gerstheim, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"] <- "Erwan Hornier"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
Rhin$obs[Rhin$commentaire_de_la_liste=="comptage du musoir amont de l'usine hydroélectrique de Gerstheim et berges voisines, E. Hornier, bonne visibilité, temps orageux, pluie"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.Hornier, JL Wilhelm, comptage exhaustif amont barrage, berges+ plans d'eau+ musoirs, temps variable, vent moyen, temp=20°environ, bonnes conditions d'observation"]<- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, observation à partir de la pointe du terre plein du barrage du canal de décharge, beau temps, 29°, bonne visibilité, comptage jusqu'à l'anse de la Thumenau avec longue vue swaro zoom 20-60"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Estève - Beau temps/nuageux, vent faible, bonne visibilité, temp 21°C."]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Estève - Beau temps/Nuageux, vent faible, bonne visibilité, temp 21°C."]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Beau temps/Nuageux - Vent faible - Bonne visibilité - Temp 21°C - Courant fort"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier/Valerie Esteve Bonne condition de visibilité nuageux pluie vent moyen t°17°"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Soleil/Ciel bleu - Bonne visibilité - 18°C"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan hornier, Valérie Esteve - Pas de vent - Brouillard léger - Visibilité moyenne - Temp 10°C"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Bonne visibilité - Pas de vent - Temp 15 °C - Présence de grande sauterelle chanteuses"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Bonne visibilité - Pas de vent - Temp 21 °C"]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Pas de vent / Ensoleillé - Bonne visibilité - Temp 20 °C - Qualité du comptage : 2/5 car présence de travaux sur le musoir - Au point d'observation : Rhin PK 309 (amont du barrage) : En 10/15 min l'eau est descendue et on a pu observer le banc de sable à gauche puis l'eau est ensuite remontée."]<- "Erwan Hornier, Valérie Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc Wilhelm - Pas de vent / Brouillard - Mauvaise visibilité - Temp 1°C - Qualité du comptage : 2/5"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Un peu de vent - Bonne visibilité - Temp 1°C"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc WILHELM - Bonne visibilité - Pas de vent - Temp 2°C - Qualité du comptage : 3/5"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc WILHELM - Pas de vent - Bonne visibilité - Temp 3°C - Qualité du comptage : 4/5 \n(Travaux sur le pont)"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp 2°C - Pluie/Gris - Bonne visibilité - Qualité de comptage : 4/5"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp. 3°C - pas de pluie/ gris - bonne visibilité - Qualité de comptage : 5/5"]<- "Valérie Estève, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp. 3°C - gris - bonne visibilité - qualité du comptage: 5/5"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve , Jean-Luc Wilhelm- Temp. 3°C - Bonne visibilité - qualité de comptage : 5/5"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Visibilité moyenne - Temps couvert - Vent moyen - Débit Rhin : 1740 m^3"] <- "Erwan Hornier, Valérie Estève, Jean-Luc Wilhelm, Gérard Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Bonne visibilité - Temps couvert - Qualité du comptage : 4,5/5"] <- "Erwan Hornier, Valérie Estève, Jean-Luc Wilhelm, Gérard Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Visibilité moyenne à bonne - Vent - Temps couvert - Qualité du comptage : 3,5/5 (le comptage n'est pas mauvais mais nous avons du changer de rive pour pouvoir compter les oiseaux du côté de la digue tiroir)"] <- "Erwan Hornier, Valérie Estève, Jean-Luc Wilhelm, Gérard Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Bonne Visibilité - Vent - Temps couvert - Qualité du comptage : 4/5 (à partir de la base nautique puis 1h après de la digue tiroir) - Présence de 5 barques de pêche : dérangement observé faible"] <- "Erwan Hornier, Valérie Estève, Jean-Luc Wilhelm, Gérard Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4°C - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - Vent moyen - Temps variable - Qualité du comptage : 4,5/5"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - temp. 6 °C - pluie - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - vent moyen - qualité du comptage : 5/5"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4 °C - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - Temps variable - Qualité du comptage : 5/5"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4 °C puis 3°C  - Débit du Rhin 2000 m^3 (Vieux Rhin en cru)- Forte pluie + grêle - Qualité du comptage : 4/5 (présence de 6 chiens sans laisse qui ont fait fuir les oiseaux)"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps couvert, 5°, vent 15km/h, SO, 1037hpa, Q Rhin Kelh : 1240 m3/s, comptage 4.5/5, quelques oiseaux masqués par une digue."] <- "Erwan Hornier, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps couvert, 5°C, vent 15km/h SO, , 1037hpa, Q Rhin Kelh : 1240m3/s, qualité comptage : 4/5"] <- "Erwan Hornier, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; "] <- "Eric Buchel" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe : Eric Brunissen, Justine Delcambre et Stéphane Umhang."] <- "Daniel Kirmser, Eric Brunissen, Justine Delcambre, Stéphane Umhang." 
Rhin$obs[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; Valérie Esteve, Jean-Luc Wilhelm - Température : 0°C"] <- "Valérie Estève, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Comptage non possible lors de la date officielle du dimanche 14/1/18 en raison du brouillard. Reporté au lendemain lundi 15/1/18. Participants : Pour la FOSOR : Bernhard DISCH; pour la LPO-Alsace : Jean-Marc BRONNER + Yann CARASCO; pour l'ONCFS : Erwan HORNIER. Vent faible à modéré en matinée dans la partie nord du plan d'eau (un peu gênant par moments), devenant faible l'après-midi dans la partie sud."] <- "Jean-Marc Bronner, Bernhard Disch, Yann Carasco, Erwan Hornier" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 1°C - Bonne visibilité - Vent faible - Qualité du comptage : 5/5"] <- "Valérie Esteve, Jean-Luc Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 1°C - Bonne visibilité - Qualité du comptage : 5/5"] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 3°C - Bonne visibilité - Qualité du comptage : 5/5"] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 6°C - Visibilité moyenne (reflet du soleil) - Qualité du comptage : 4/5"] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps gris / pluie - Temp : °C - Qualité du comptage : 5/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps gris / pluvieux - Temp : 0°C - Qualité du comptage : 5/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - 1116 hPa - Temp : 0°C - Qualité du comptage : 3/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Temp : 0°C - Mauvaise visibilité avec brume de chaleur, obligeant à compter de chaque côté. Qualité du comptage : 4/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temp : Gris/ Vent à 15/20 km.h -Mauvaise visibilité à cause de brume de chaleur (comptage des deux côtés) - Qualité du comptage : 4/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Mauvaise visibilité à cause de brume de chaleur (donc comptage des deux côtés) - Temp : 1°C - Qualité du comptage : 4/5"] <- "Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps : Nuageux / un peu de vent - Temp : 9°C - Qualité du comptage : 5/5"] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temps : Nuageux / Un peu de vent - Temp : 10°C - Qualité du comptage : 5/5"] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temps : Nuageux / Un peu de vent - Temp : 10°C - Qualité du comptage : 3/5 (Présence d'une péniche qui a perturbé tous les oiseaux. Il se sont posés du côté de la digue au lieu du musoir donc à chaque fois que l'on avançait avec la voiture, ils s'envolaient pour se reposer plus loin.) Dans ce formulaire, on intègre les observations du contre canal."] <- "Valérie Esteve, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 4/5, changement de berge 20 mn (nord à sud)"] <- "Erwan Hornier, Jean-Luc Wilhelm, T Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 3/5,"] <- "Erwan Hornier, Jean-Luc Wilhelm, T Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T LE sergent, t°=9°, vent faible, 1016 hpa, couvert avec eclaircies, note comptage 5/5, de la digue sud est"] <- "Erwan Hornier, Jean-Luc Wilhelm, T Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=9°, vent faible, couvert avec éclairicies, 1016 hpa, note 5/5, de digue Sud est"] <- "Erwan Hornier, Jean-Luc Wilhelm, T Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 1005 hpa, vent moyen 10km/h, 21°, niveau de l'eau assez bas, vasières découvertes, comptage 4/5"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 1005 hpa, 21°, vent moyen 10km/h, 4/5, débit important, pas de haut fonds découverts"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 4/5, beau temps, 22-25°, vent faible, canal + écluse + contre-canal"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E-Hornier, 3/5 (embouteillage de péniche à l'entrée des écluses + mouvement d'oiseaux), beau temps légèrement nuageux, 22 à 26°, vent faible, canal + côté allemand."] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E-Hornier Beau temps - 22 à 26°-Vent nul- 5/5- péniches arrêtées à proximité des musoirs amont et aval"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 11° à 18 °, vent nul, conditions d'observation 5/5"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, léger vent, couvert, comptage 5/5"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, vent nul, couvert, pluie fine, 12°, comptage 5/5"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 13°, vent moyen, visibilité bonne , comptage 5/5"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 13°, vent faible, visibilité bonne, 5/5, y compris contre-canal"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage des digues côté UNAP"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage à partir de la digue côté UNAP + comprend aussi les effectifs du secteur étranglement"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5 (1 personne, groupes denses, visibilité limite le long de la digue tiroir. Comptage à partir de la banquette ouest et du barrage du canal de décharge"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Secteur Sud du plan d'eau (Pointe sud+ Thumenau+étranglement)à partir de la rive ouest, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 3/5 (nombreux mouvements d'oiseaux)"] <- "Erwan Hornier, Jean-Luc Wilhelm, Gerard Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="à partir du kiosque et des berges est, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 2/5 (beaucoup d'oiseaux, nombreux mouvements, visibilité moyenne)."] <- "Erwan Hornier, Jean-Luc Wilhelm, Gerard Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="Secteur nord des bouée à partir de la rive ouest et de la digue tiroir), E. Hornier/JL Wilhelm/Gerard Hog, 1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage difficile 2/5 (nombreux mouvements d'oiseaux)"] <- "Erwan Hornier, Jean-Luc Wilhelm, Gerard Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 4 °, vent nul, 1018 hpa, comptage 4/5 (dérangement barque de pêcheur au seuil 2)"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 5 °, temps couvert, pluie, 1018 hpa, comptage 5/5 (+contre-canal)"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, pluie, vent fort, houle sur le Rhin, 1012hpa, comptage à partir des digues, du barrage et un point en digue allemande en raison de la mauvaise visibilité. Comptage 3/5 (visibilité moyenne et vent fort)"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, vent fort, pluie, comptage 3/5 (mouvement d'oiseaux et visibilité)"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; "] <- "Daniel Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; E. Hornier, dérangement par embarcation de pêcheurs sous le seuil 1 entraînant un déplacement assez important de l'effectif habituel"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Mauvaises conditions météo en matinée pour le comptage de la partie Nord, avec de la pluie continue et du vent, et donc des conditions d'observation médiocres (condensation sur les optiques, vagues...). Meilleures conditions l'après-midi pour la partie sud, avec un temps plus calme, sans pluie et avec du vent devenu faible.\n4 participants de la LPO Alsace (Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse) + 3 participants allemands du FOSOR (Finn Brunssen, John Ryding, Jürgen Rupp)."] <- "Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Finn Brunssen, John Ryding, Jürgen Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Remplacement de l'observateur titulaire (Eric Buchel), non disponible pour le comptage Wetlands 2019."] <- "Christian Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 10°, 1037 hpa, vent léger, très bonne visibilité, comptage 3/5 (gros envol d'un groupe de F milouin, morillon et C chipeau), comptage à partir de la digue tiroir."] <- "Erwan Hornier, Jean-luc Wilhelm, E. Coq, M. Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 5°, 1037 hpa, vent léger, très bonne visibilité, comptage 4/5 (bonne visibilité mais groupes denses), comptage à partir de la digue tiroir."] <- "Erwan Hornier, Jean-luc Wilhelm, E. Coq, M. Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M Esnard, 11°, 1037 hpa, vent léger, très bonne visibilité, comptage 5/5, à partir de la digue SE"] <- "Erwan Hornier, Jean-luc Wilhelm, E. Coq, M. Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 11°, 1037 hpa, vent nul, très bonne visibilité, comptage 5/5 à partir de la pointe du barrage"] <- "Erwan Hornier, Jean-luc Wilhelm, E. Coq, M. Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. COQ, vent moyen (Sud), température: 7°C, couvert mais bonne visibilité\nSecteurs barrage jusque aval seuil 2"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.HORNIER, E.COQ, vent moyen (Sud), température 10°C, couvert mais bonne visibilité\nCanal + contre canal"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. Coq, 9°C, vent nul, temps couvert,"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. Coq, 10°C, vent nul, temps couvert"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq"] <- "Erwan Hornier, E. Coq, V. Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq\nCanal de décharge de l'Ill en crue"] <- "Erwan Hornier, E. Coq, V. Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq\nCanal de décharge en crue, 150 à 200m^3/s"] <- "Erwan Hornier, E. Coq, V. Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\nsoleil, vent faible\nbonne visibilité"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps couvert\nvent faible"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps ensoleillé\nenviron 13°C, vent faible"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\nsoleil, 12°C, vent faible"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps couvert / pluie le matin\nvent faible mais de plus en plus présent"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq\nTemps couvert, vent moyen, niveaux d'eau élevés\nT° 20°C, P 1005hPa"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq\nPluie et orage, visibilité réduite (arbres), vent moyen\nT° 20°C, P 1005hPa"] <- "Erwan Hornier, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nSoleil, vent faible\nT° 25°C"] <- "Jean-Luc Wilhelm, V. Klethi, E Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nBeau temps, vent faible\nT° 22-30°C, P 1118hPa"] <- "Jean-Luc Wilhelm, V. Klethi, E Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq"] <- "Jean-Luc Wilhelm, V Klethi, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Coq\nSoleil, vent faible\nT° 19-24°C"] <- "E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi\nSoleil, vent faible\nT° 22°C"] <- "Jean-Luc Wilhelm, V. Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi\nSoleil, vent faible\nT° 29°C"] <- "Jean-Luc Wilhelm, V. Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 25°C\nPerturbations : 28 voiliers, 14 bateaux à moteur"] <- "Jean-Luc Wilhelm, V. Klethi, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 27°C\nPerturbations : 1 float tube, 1 bateau de pêche"] <- "Jean-Luc Wilhelm, V. Klethi, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 25°C"] <- "Jean-Luc Wilhelm, V. Klethi, E. Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 1024 hpa, 18 ° vent nul, débit entre 1000 et 1500 m3/s en hausse, surface importante de végétation aquatique émergente sous le barrage de répartition des eaux (amont)"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau tempsd, vent nul, 25°, débit 1000 à 1500 m3/s, comprenant contre-canal de drainage et musoirs, comptage 5/5"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, pas de nuages, vent nul, 1024 hpa, 15 à 26 °, niveau d'eau normal, haut, pas de zones émergées"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27 °, beau temps, pas de nuages, vent nul, niveau normal, haut, pas de zone émergée, comptage à partir de rive est"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27°, zero nuages, vent nul, comptage à partir des digues est et tiroir"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27°, zero nuages, vent nul, comptage à partir des digues tiroir"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, 25 à 29 °, vent nul, ciel sans nuage, 1017 hpa, Rhin à 1300 m3/s,"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan.hornier, beau temps, 29/30 °, vent nul, pas de nuages, 1300m3/S environ, conditions dévarables au stationnement d'oiseaux, bancs de sables et graviers inexistants, courant assez fort, eaux turbides"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 17°c, temps variable, pression 1019 hpa, vent moyen, visibilité bonne, qualité comptage 3/5, 6 bateaux de pêche, 1 planche à voile"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps variable, vent moyen à fort, temp 17°, pression 1019, visibilité moyenne, qualité comptage 3/5, 3 bateaux de pêche, 1 voilier, comptage depuuis Rhinland"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, Temps variable, 17°, vent moyen, pression 1019, visibilité moyenne, comptage à partir de banquette ouest, estimation qualité comptage 4/5, 3 pêcheurs à pied Thumenau"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps nuageux, temp 20°, vent faible, niveau eau élevé, visibilité bonne, estimation qualité comptage '/5, comptage depuis banquette sud-est"] <- "Erwan Hornier, Jean-Luc Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, , beau temps, frais, vent moyen, bonne visibilité, temp 15°c, 1027 hpa, comptage à partir du barrage de répartition amont et des berges françaises, qualité 4/5, 8 à 10 pêcheurs à pied"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps frais, temp 18°C, 1026 hpa, vent moyen, visibilité bonne, qualité de comptage 5/5 travaux de fauche sur la digue ouest, comptage à partir de la digue ouest"] <- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, visibilité bonne, temp 19°, couvert, légère pluie, pas de vent, 1011 hpa, comptage à partir de la digue ouest et du barrage, présence de tracteur d'entretien sur la digue. qualité comptage 5/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier,  à partir de l'îlôt nord, temp 19°, couvert, légère pluie, vent nul, bonne visibilité, 1011hpa, qualité comptage 4/5 (mouvements d'oiseaux lié à la présence de 5 pêcheurs (à pied, float-tube, bateau)"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, couvert, bonne visibilité, vent faible, pas de pluie, dérangement (1 bateau de pêche, 9 pêcheurs à pieds, 3 chiens en divah=gation côté allemand,), débit normal, comptage à partir du côté francais et du barrage de réaprtition, comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, vent faible, pas de pluie, bonne visibilité, temps couvert, comptage à partir des digues ouest et est (aval), 6 pêcheurs à pied sur digues est aval, comptage 5/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 13°, 1015 hpa, temps couvert, visibilité assez bonne, brouillard léger, bruine, pas de vent, comptage à partir de la pointe sud et de la digue est, 1 pécheur pointe sud, 2 ornithos musoir du barrage, comptage 3/5, niveau d'eau moyen, pas de haut fond élergés, groupe de 30 à 50 limicoles en vol, ne se posent pas"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, Thumenau + Etrangement, comptage à partir de la berge Est, 13 °, 1015 hpa, temps couvert, vent faible, pas de pluie, pas de brouillard, comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°, temps couvert, vent nul, léger brouillard, pas de pluie, 1012 hpa, comptage à partir des digues Est, tiroir et Ouest, 4 bateaux de pêche, 1 float-tube, comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°, 1012 hpa, temps couvert, léger brouillard, pas de pluie, 18 bateaux de ^pêche, comptage à partir de la digue tiroir et de la digue ouest, comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 12°, 1023 hpa, temps demi-couvert, bonne visibilité, vent faible, pas de brume, pas de pluie, 1 pêcheur à pied, comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Hornier E, 14°, temps demi-couvert, 1023 hpa, bonne visibilité, vent faible à nul, pas de pluie, pas de brume, 1 barque de pêche, 2 pecheurs à pied, niveau d'eau un peu plus haut qu'habituellement, pas de hauts fonds émergés. Comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir des digues ouest et est (aval) ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 2 pêcheurs, 1 chien non tenu en laisse"]<- "Jean-Luc Wilhelm, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir du côté français et du barrage de répartition ; qualité du comptage estimée à 3/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : aucune"]<- "Jean-Luc Wilhelm, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, G. Laurent, temps demi-couvert, 3°, 1008hpa, léger brouillard, visibilité moyenne à bonne, à partir de la pointe sud et du barrage mobile, niveaux d'eau normaux, qualité de comptage estimé à 5/5"] <- "Erwan Hornier, Jean-Luc Wilhelm, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, G. Laurent, 5°, 1008 hpa, temps demi-couvert, visibiité moyenne à bonne, 7 barques de pêche, 1 voilier, 1 chien non tenu en laisse, qualité de comptage 4/5"] <- "Erwan Hornier, Jean-Luc Wilhelm, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir des banquettes est (comprend le secteur Thumenau et l’étranglement) ; qualité du comptage estimée à 5/5 ; niveaux d'eau normaux\nT° 8°C   P 1008hPa   Temps demi-couvert\nVisibilité bonne   Vent absent   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : aucune"] <- "Erwan Hornier, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue tiroir et des digues est et ouest ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 5°C   P 1008hPa   Temps brouillard\nVisibilité mauvaise   Vent absent   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 1 kayakiste-pêcheur, 7 barques de pêche"] <- "Erwan Hornier, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue ouest et du barrage ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux (débit 800m3/s)\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen (10km/s)   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 ornithologue-photographe, 1 voiture, au moins 3 péniches"] <- "Erwan Hornier, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de l'îlot et du musoir ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 vedette, 1 bateau, 3 promeneurs, 1 chien"] <- "Erwan Hornier, G. Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Compteurs : Valérie-Anne Clément-Demange, Daniel Kirmser, Stéphane Umhang (+ amie), Charlotte Wagner."] <- "Daniel Kirmser, Valérie-Anne Clément-Demange, Stéphane Umhang, Charlotte Wagner"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Comptage franco-allemand (LPO Alsace + FOSOR). Bonnes conditions météo. Mais comptage perturbé en matinée par des barques de pêche provoquant des envols d'oiseaux, concernant surtout les Fuligules morillons, comptés approximativement en vol.\nAu total : 7607 oiseaux (sans compter les laridés, ni passereaux et rapaces).\nParticipants :\n- LPO Alsace : Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Jean-Marie RISSE\n- FOSOR (Fachschaft für Ornithologie im Südlichen OberRhein) : Kira DONDERER, Sophia-Marie JACK, Mathias MUELLER, Jürgen RUPP, Victor WEMBER, Anton & Gustav WILD"] <- "Jean-Marc Bronner, Yann Carasco, Luca Fetique, Jean-Pierre Hiss, Jean-Marie Risse, Kira Donderer, Sophia-Marie Jack, Mathias Mueller, Jürgen Rupp, Victor Wember, Anton Wild, Gustav Wild"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe : Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Daniel Kirmser, Delphine Lacuisse, Stéphane Umhang."] <- "Daniel Kirmser, Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Delphine Lacuisse, Stéphane Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Archives Wetlands Alsace ; Observateur = Stéphane Umhang.\nDonnées saisies par Christian Frauli à la demande de Christian Dronneau, pour obtenir dans le module WI le nombre réel d'oies cendrées recensées sur le site de Gerstheim ce jour-là."] <- "Stéphane Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Participants : Carole BIZART, Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Victor ROUAULT.\nMétéo : 10 à 15 cm de neige au sol; redoux en cours, avec températures devenant légèrement positives en journée. Ciel couvert. Quelques faibles pluies et neige mêlées, puis quelques faibles pluies éparses."] <- "Jean-Marc Bronner, Yann Carasco, Luca Fetique, Jean-Pierre Hiss, Victor Rouault"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Observateur : Stéphane Goubert ; donnée saisie par Christian Frauli.\nN.B. : effectif réel = 14 ind. ; effectif saisi = 6 ind., soit le complément à l'effectif déjà recensé (8 ind.) lors du comptage officiel du site le 16/01/2022."] <- "Christian Frauli, Stéphane Goubert"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; En remplacement du compteur traditionnel (Eric BUCHEL), absent."] <- "Christian Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe de comptage : Eric Brunissen, Sébastien Didier, Daniel Kirmser, Delphine Lacuisse, Stéphane Umhang."] <- "Daniel Kirmser, Eric Brunissen, Sébastien Didier, Delphine Lacuisse, Stéphane Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 10 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Luca Fetique, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Max Kurzmann, Matthias Müller, Jürgen Rupp). Bonnes conditions météo (bonnes visibilités, vent calme, températures proches de 0° en matinée, puis très faiblement positives l'après-midi). Nombre total d'oiseaux observés : 7.836."] <- "Jean-Marc Bronner, Carole Bizart, Yann Carasco, Luca Fetique, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault, Max Kurzmann, Matthias Müller, Jürgen Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Observateur : Christian Dronneau ; donnée saisie par Christian Frauli.\nN.B. : effectif réel = 68 ind. ; effectif saisi : 9 ind. (= effectif complémentaire à l'effectif WI déjà saisi lors du comptage officiel du site le 16/01/2022)."] <- "Christian Dronneau"
Rhin$obs[Rhin$commentaire_de_la_liste=="Archives Wetlands Alsace ; Observateur : Marc HELFTER ; donnée saisie par Christian FRAULI."] <- "Marc Helfter, Christian Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; Avec Anne Tritz (prise de notes)"] <- "Eric Buchel, Anne Tritz"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; avec Anne Tritz (prise de notes)"] <- "Eric Buchel, Anne Tritz"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Observateurs : Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Jérôme Isambert, Daniel Kirmser, Delphine Lacuisse, Nathan Roser-Robert, Nicolas Roser, Stéphane Umhang."] <- "Daniel Kirmser, Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Jérôme Isambert, Delphine Lacuisse, Nathan Roser-Robert, Nicolas Roser, Stéphane Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 7 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Jürgen Rupp). Conditions météo médiocres le matin (pluvieux et venteux) pour la partie Nord du plan d’eau, puis devenant bonnes à partir de midi (temps sec, sans vent significatif) pour la partie Sud. Conformément au protocole, les oiseaux du canal de décharge de l’Ill ont été intégrés aux présentes données du secteur [17]. Nombre total d'oiseaux observés sur l’ensemble des secteurs [17], [17A] (canal d'alimentation de l'Ill) et [18] (cours du Rhin): 3918 oiseaux d’eau (chiffre le plus faible relevé sur ces sites depuis de très nombreuses années, voire depuis le début des comptages –hors périodes de gel total-, et de loin !)."] <- "Jean-Marc Bronner, Carole Bizart, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault, Jürgen Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 7 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Jürgen Rupp). Conditions météo médiocres le matin (pluvieux et venteux) pour la partie Nord du plan d’eau, puis devenant bonnes à partir de midi (temps sec, sans vent significatif) pour la partie Sud. Conformément au protocole, les oiseaux du canal de décharge de l’Ill ont été intégrés aux présentes données du secteur [17]. Nombre total d'oiseaux observés sur l’ensemble des secteurs [17], [17A] (canal d'alimentation de l'Ill) et [18] (cours du Rhin): 3918 oiseaux d’eau (chiffre le plus faible relevé sur ces sites depuis de très nombreuses années, voire depuis le début des comptages –hors périodes de gel total-, et de loin !)."] <- "Jean-Marc Bronner, Carole Bizart, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault, Jürgen Rupp"


#Prendre en compte les remarques : 
unique(Rhin$liste_complete__)
colnames(Rhin) [37] <- "remarques"

unique(Rhin$remarques)
unique(Rhin$commentaire_de_la_liste)


Rhin$qualite_comptage <- with(Rhin, ifelse(Rhin$liste_complete__=="0","douteux",
                                    ifelse(Rhin$remarques=="Participants : Carole BIZART, Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Victor ROUAULT. Météo : 10 à 15 cm de neige au sol; redoux en cours, avec températures devenant légèrement positives en journée. Ciel couvert. Quelques faibles pluies et neige mêlées, puis quelques faibles pluies éparses.","douteux",
                                    ifelse(Rhin$remarques=="Sous évalué en raison du vent, de nombreux individus sont à l'abri du vent derrière la digue tiroir ou ailleurs","douteux",
                                  ifelse(Rhin$remarques=="Mauvaises conditions d'observation. Total peut-être sous-évalué.","douteux","ok")))))
                                  
                                         
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan hornier, Valérie Esteve - Pas de vent - Brouillard léger - Visibilité moyenne - Temp 10°C"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Pas de vent / Ensoleillé - Bonne visibilité - Temp 20 °C - Qualité du comptage : 2/5 car présence de travaux sur le musoir - Au point d'observation : Rhin PK 309 (amont du barrage) : En 10/15 min l'eau est descendue et on a pu observer le banc de sable à gauche puis l'eau est ensuite remontée."] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc Wilhelm - Pas de vent / Brouillard - Mauvaise visibilité - Temp 1°C - Qualité du comptage : 2/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Visibilité moyenne à bonne - Vent - Temps couvert - Qualité du comptage : 3,5/5 (le comptage n'est pas mauvais mais nous avons du changer de rive pour pouvoir compter les oiseaux du côté de la digue tiroir)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Bonne Visibilité - Vent - Temps couvert - Qualité du comptage : 4/5 (à partir de la base nautique puis 1h après de la digue tiroir) - Présence de 5 barques de pêche : dérangement observé faible"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4 °C puis 3°C  - Débit du Rhin 2000 m^3 (Vieux Rhin en cru)- Forte pluie + grêle - Qualité du comptage : 4/5 (présence de 6 chiens sans laisse qui ont fait fuir les oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps couvert, 5°, vent 15km/h, SO, 1037hpa, Q Rhin Kelh : 1240 m3/s, comptage 4.5/5, quelques oiseaux masqués par une digue."] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 6°C - Visibilité moyenne (reflet du soleil) - Qualité du comptage : 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Temp : 0°C - Mauvaise visibilité avec brume de chaleur, obligeant à compter de chaque côté. Qualité du comptage : 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temp : Gris/ Vent à 15/20 km.h -Mauvaise visibilité à cause de brume de chaleur (comptage des deux côtés) - Qualité du comptage : 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Mauvaise visibilité à cause de brume de chaleur (donc comptage des deux côtés) - Temp : 1°C - Qualité du comptage : 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temps : Nuageux / Un peu de vent - Temp : 10°C - Qualité du comptage : 3/5 (Présence d'une péniche qui a perturbé tous les oiseaux. Il se sont posés du côté de la digue au lieu du musoir donc à chaque fois que l'on avançait avec la voiture, ils s'envolaient pour se reposer plus loin.) Dans ce formulaire, on intègre les observations du contre canal."]<- "douteux" 
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 3/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 4/5, changement de berge 20 mn (nord à sud)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E-Hornier, 3/5 (embouteillage de péniche à l'entrée des écluses + mouvement d'oiseaux), beau temps légèrement nuageux, 22 à 26°, vent faible, canal + côté allemand."] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage des digues côté UNAP"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage à partir de la digue côté UNAP + comprend aussi les effectifs du secteur étranglement"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5 (1 personne, groupes denses, visibilité limite le long de la digue tiroir. Comptage à partir de la banquette ouest et du barrage du canal de décharge"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Sud du plan d'eau (Pointe sud+ Thumenau+étranglement)à partir de la rive ouest, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 3/5 (nombreux mouvements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur nord des bouée à partir de la rive ouest et de la digue tiroir), E. Hornier/JL Wilhelm/Gerard Hog, 1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage difficile 2/5 (nombreux mouvements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="à partir du kiosque et des berges est, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 2/5 (beaucoup d'oiseaux, nombreux mouvements, visibilité moyenne)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 4 °, vent nul, 1018 hpa, comptage 4/5 (dérangement barque de pêcheur au seuil 2)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, pluie, vent fort, houle sur le Rhin, 1012hpa, comptage à partir des digues, du barrage et un point en digue allemande en raison de la mauvaise visibilité. Comptage 3/5 (visibilité moyenne et vent fort)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, vent fort, pluie, comptage 3/5 (mouvement d'oiseaux et visibilité)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; E. Hornier, dérangement par embarcation de pêcheurs sous le seuil 1 entraînant un déplacement assez important de l'effectif habituel"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Mauvaises conditions météo en matinée pour le comptage de la partie Nord, avec de la pluie continue et du vent, et donc des conditions d'observation médiocres (condensation sur les optiques, vagues...). Meilleures conditions l'après-midi pour la partie sud, avec un temps plus calme, sans pluie et avec du vent devenu faible.\n4 participants de la LPO Alsace (Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse) + 3 participants allemands du FOSOR (Finn Brunssen, John Ryding, Jürgen Rupp)."] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E Hornier, E Coq\nPluie et orage, visibilité réduite (arbres), vent moyen\nT° 20°C, P 1005hPa"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 25°C\nPerturbations : 28 voiliers, 14 bateaux à moteur"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 27°C\nPerturbations : 1 float tube, 1 bateau de pêche"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Erwan.hornier, beau temps, 29/30 °, vent nul, pas de nuages, 1300m3/S environ, conditions dévarables au stationnement d'oiseaux, bancs de sables et graviers inexistants, courant assez fort, eaux turbide"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 17°c, temps variable, pression 1019 hpa, vent moyen, visibilité bonne, qualité comptage 3/5, 6 bateaux de pêche, 1 planche à voile"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps variable, vent moyen à fort, temp 17°, pression 1019, visibilité moyenne, qualité comptage 3/5, 3 bateaux de pêche, 1 voilier, comptage depuuis Rhinland"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, Temps variable, 17°, vent moyen, pression 1019, visibilité moyenne, comptage à partir de banquette ouest, estimation qualité comptage 4/5, 3 pêcheurs à pied Thumenau"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, , beau temps, frais, vent moyen, bonne visibilité, temp 15°c, 1027 hpa, comptage à partir du barrage de répartition amont et des berges françaises, qualité 4/5, 8 à 10 pêcheurs à pied"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps frais, temp 18°C, 1026 hpa, vent moyen, visibilité bonne, qualité de comptage 5/5 travaux de fauche sur la digue ouest, comptage à partir de la digue ouest"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, visibilité bonne, temp 19°, couvert, légère pluie, pas de vent, 1011 hpa, comptage à partir de la digue ouest et du barrage, présence de tracteur d'entretien sur la digue. qualité comptage 5/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier,  à partir de l'îlôt nord, temp 19°, couvert, légère pluie, vent nul, bonne visibilité, 1011hpa, qualité comptage 4/5 (mouvements d'oiseaux lié à la présence de 5 pêcheurs (à pied, float-tube, bateau)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, couvert, bonne visibilité, vent faible, pas de pluie, dérangement (1 bateau de pêche, 9 pêcheurs à pieds, 3 chiens en divah=gation côté allemand,), débit normal, comptage à partir du côté francais et du barrage de réaprtition, comptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, vent faible, pas de pluie, bonne visibilité, temps couvert, comptage à partir des digues ouest et est (aval), 6 pêcheurs à pied sur digues est aval, comptage 5/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 13°, 1015 hpa, temps couvert, visibilité assez bonne, brouillard léger, bruine, pas de vent, comptage à partir de la pointe sud et de la digue est, 1 pécheur pointe sud, 2 ornithos musoir du barrage, comptage 3/5, niveau d'eau moyen, pas de haut fond élergés, groupe de 30 à 50 limicoles en vol, ne se posent pas"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 14°, 1012 hpa, temps couvert, léger brouillard, pas de pluie, 18 bateaux de ^pêche, comptage à partir de la digue tiroir et de la digue ouest, comptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, 12°, 1023 hpa, temps demi-couvert, bonne visibilité, vent faible, pas de brume, pas de pluie, 1 pêcheur à pied, comptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Hornier E, 14°, temps demi-couvert, 1023 hpa, bonne visibilité, vent faible à nul, pas de pluie, pas de brume, 1 barque de pêche, 2 pecheurs à pied, niveau d'eau un peu plus haut qu'habituellement, pas de hauts fonds émergés. Comptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir des digues ouest et est (aval) ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 2 pêcheurs, 1 chien non tenu en laisse"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir du côté français et du barrage de répartition ; qualité du comptage estimée à 3/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : aucune"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, G. Laurent, 5°, 1008 hpa, temps demi-couvert, visibiité moyenne à bonne, 7 barques de pêche, 1 voilier, 1 chien non tenu en laisse, qualité de comptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue tiroir et des digues est et ouest ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 5°C   P 1008hPa   Temps brouillard\nVisibilité mauvaise   Vent absent   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 1 kayakiste-pêcheur, 7 barques de pêche"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue ouest et du barrage ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux (débit 800m3/s)\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen (10km/s)   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 ornithologue-photographe, 1 voiture, au moins 3 péniches"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de l'îlot et du musoir ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 vedette, 1 bateau, 3 promeneurs, 1 chien"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Base nautique\nPostes d'observation : Digue tiroir / Digue est\nConditions météorologiques : T° 9-11 °C   P 1018 hPa\n   Nuages 10-50%   Vent : aucun  Brouillard : aucun  Pluie : aucune  Brumes de chaleur : aucune\nQualité estimée : 3/5 (très nombreux mouvements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Postes d'observation : Digue ouest / Barrage\nConditions météorologiques : T° 3-5 °C   P 992-995 hPa\n   Nuages >90%   Vent : faible  Brouillard : aucun  Pluie : abondante  Brumes de chaleur : aucune\nQualité estimée : 4/5 (luminosité mauvaise)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Postes d'observation : Îlot / Musoir\nConditions météorologiques : T° 4-5 °C   P 992-995 hPa\n   Nuages >90%   Vent : faible  Brouillard : aucun  Pluie : bruine  Brumes de chaleur : aucune\nQualité estimée : 4/5 (quelques mouvements, luminosité faible)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; Postes d'observation : Rive française / Barrage de répartition\nConditions météorologiques : T° 0-2 °C\n   Nuages 10-50%   Vent : aucun  Brouillard : léger  Pluie : aucune  Brumes de chaleur : aucune\nQualité estimée : 4/5 (nombreux dérangements : chasse, pêche,...)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Comptage franco-allemand (LPO Alsace + FOSOR). Bonnes conditions météo. Mais comptage perturbé en matinée par des barques de pêche provoquant des envols d'oiseaux, concernant surtout les Fuligules morillons, comptés approximativement en vol.\nAu total : 7607 oiseaux (sans compter les laridés, ni passereaux et rapaces).\nParticipants :\n- LPO Alsace : Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Jean-Marie RISSE\n- FOSOR (Fachschaft für Ornithologie im Südlichen OberRhein) : Kira DONDERER, Sophia-Marie JACK, Mathias MUELLER, Jürgen RUPP, Victor WEMBER, Anton & Gustav WILD"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Rhinland\nPostes d'observation : Digue tiroir / Digue est / Digue ouest\nQualité du comptage estimée : 3/5 (vent important, vagues masquant les canards)\nConditions météorologiques : T° 7-9°C   P 1020-1021hPa   D 1350m^3/s\n   Couverture nuageuse : 50-90%   Brouillard Aucun   Pluie Aucune   Brumes de chaleur Aucune"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="comptage à partir des musoirs et îlôt, 22°, 1021 hpa, 657 m3/s Rhin strsb, beau temps sans nuages, pas de brumes de chaleurs, hauts fonds découverts, comptage 4/5 (mouvement d'oiseaux), 5 pêcheurs,1 bateau pêche 2 photographes animalier, travaux musoir"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="T° 8°C, débit Rhin strsb 951 m3/s, 1025 hpa, vent moyen, couvert 50-90%, averses, pas de brouillard, Estimation qualité comptage 3/5 (oiseaux nerveux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland\nTemps couvert, 50-90% nuages, 8°, 1018 hpa, Q Strsb 984 m3/s, comptage à partir de la digue est et tiroir, 3/5 en raiso n des mouvements d'oiseaux (dérangements), pas de vent"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Sud\nTemps couvert avec éclaircies, pluie faible, vent nul, 50-90% nuages, 5 à 7 °, 1018hpa, Q strsb 984 m3/s, à partir de la pointe sud, du barrage et de la digue ouest, 3/5 (mouvements d'oiseaux en provenance de la partie nord)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps frais, 20% couverture nuageuse, 5°c, 1021hpa, Q strsb 963 m3/s, pas de vent, pas de brouillard, pas de pluies, à partir du barrage amont et de la rive française, 3/5 (envol d'oiseaux à cause d'une barque de pêche)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Sud\n11°c, 1009hpa, temps à giboulée, averses, vent moyen et rafales, \nQ Rhin Strasbourg = 1560 m3/s, comptage 2/5 (mouvement d'oiseaux avzerses) à partir de la pointe sud et de la digue est"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland\n12°c, 1012hpa, temps à giboulése, averses, vent moyen et rafales, \nQ Rhin Strasbourg = 1565 m3/s, comptage 2/5 (mouvement d'oiseaux averses) à partir de la digue est et de la digue ouest."] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base Nautique\nbeau temps, 24°c, 1015 hpa, 1370 m3/s Rhin Strasbourg, pas de vent, brumes de chaleur gênantes, comptage 2/5 à partir de la digue est et de la digue ouest"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="beau temps, 23°c, 1008 hpa, 1524 m3/s Rhin Strasbourg, pas de vent, comptage 1/5 à partir des îlo^ts et musoirs (débit trop importants, pb d'accès aux points de comptage, trop de courant"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps variable, vent faible, >90% nébolusité, averses, pas de brumes, 17°c; 1020  hpa, 1931  m3/s Strasbourg, à partir de la pointe sud et de la rive est, qualité 4/5 (quelques dérangements et déplacements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="secteur Rhinland\nTemps variable, vent faible, >90% nébolusité, averses, pas de brumes, 19°c; 1020  hpa, 1897  m3/s Strasbourg, à partir des digues est et ouest, qualité 4/5 (quelques dérangements et déplacements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="secteur Base nautique\nTemps variable, vent faible, >90% nébolusité, pas de pluies, pas de brumes, 19°c; 1020  hpa, 1897  m3/s Strasbourg, à partir des digues est et ouest, qualité 4/5 (quelques dérangements et déplacements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Beau temps, vent faible, 50-90% nébolusité, pas de pluie, pas de brumes, 21 °c; 1015  hpa, 1965  m3/s Strasbourg, à partir des digues est et ouest, qualité 4/5 (quelques dérangements et déplacements d'oiseaux"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Beau temps, vent faible, 10-50% nébolusité, pas de pluie, pas de brumes, 21 °c; 1013  hpa, 1965  m3/s Strasbourg, à partir du barrage amont et de la rive française, qualité 2/5 (incomplet, chemins coupés mais peu d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Beau temps, -2°c, 1025 hpa en baisse, 582m3/S Rhin Strasbourg, pas de vent, pas de pluie, comptage à partir de la digue ouest et du barrage, 3/5 (dérangements / travaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; Beau temps froid, -1°c, léger brouillard givrant, 1026hpa en hausse, vent faible, 582 m3/S débit du Rhin à Strasbourg (en baisse), qualité du comptage : 3/5 (dérangement, vols d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert et pluvieux, 5°c, vent moyen à fort NO, 1008 hpa en baisse, 909m3/s Q Rhin Strasbourg stable, comptage à partir de la digue ouest et du barrage, 3/5 (déplacement d'oiseauax)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert et pluvieux, 7°c, vent moyen à fort NO, 1008 hpa en baisse, 909m3/s Q Rhin Strasbourg stable, comptage à partir de la l'îlot et du musoir, 3/5 (déplacement d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Sud:\nTemps couvert et brumeux, 7°C, 1024hpa (en baisse), vent faible de nord, Débit Rhin Strasbourg: 646 m3/S, comptage 2/5 (visibilité limite)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Nord:\nTemps couvert et brumeux, 10°C, 1023hpa (en baisse), vent faible de nord, Débit Rhin Strasbourg: 646 m3/S, comptage 2/5 (visibilité limite)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base Nautique:\nTemps couvert et brumeux, 10°C, 1022hpa (en baisse), vent faible de nord, Débit Rhin Strasbourg: 646 m3/S, comptage 4/5 (visibilité limite)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Pointe Sud\nBeau temps, pas de nuages, pas de vent, 20 à 28°c, 1017hpa en baisse, 669m3/s Rhin Strasbourg, niveau du plan d'eau normal haut, comptage 3/5 (dérangement bateau surveillance qualité eau (FDAAPPMA))"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Beau temps, pas de nuages, pas de vent, 29°c, 1016hpa en baisse, 618m3/s Rhin Strasbourg, comptage 3/5 (dérangement bateau police Rhin)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Beau temps, 20°c, vent nul, pas de pluie, 1023 hpa en baisse, Débit Rhin Strasbourg 766 m3/s, comptage 2/5 fort mouvements d'oiseaux, y compris avec le secteur amont)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert (90%), pas de pluie, vent moyen nord, 12°c, 1025hpa, Débit Rhin Strasbourg = 868 m3/S, comptage 3/5 (pb longue-vue, comptage aux jumelles )"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert (90%), vent moyen nord, 13°c, 1023hpa, Débit Rhin Strasbourg = 868 m3/S, comptage 3/5 (pb longue-vue, à la jumelle)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Pointe Sud\nBeau temps, 12°C, légèrement voilé, vent nul, 1023 hpa en hausse, Débit Rhin Strasbourg = 730m3/S; comptage 3/5 (mouvement d'oiseaux, dérangement barques de pêche)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Pointe Sud : \nTemps couvert 90%, 9°c, vent faible à nul, 1012 hpa en baisse, 619m3/s Q Rhin Strasbourg, en hausse, comptage à partir de pointes sud, barrage Krafft et berges est, 3//5 (mouvement d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland : \nTemps couvert 90%, pluie faible, 10°c, vent faible à nul, 1010 hpa en baisse, 900m3/s Q Rhin Strasbourg, en hausse, comptage à partir des digues est et ouest, 3//5 (mouvement d'oiseaux"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base nautique : \nTemps couvert 90%, 13°c, vent faible à nul, 1007 hpa en baisse, 900m3/s Q Rhin Strasbourg, en hausse, comptage à partir de digue ouest, 4/5 (mouvement d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert (70%), éclaircies, 4°c, vent nul, 1020 hpa en baisse, 595 m3/s Rhin Strasbourg en hausse, comptage 3/5 (mouvements, contre-jour)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert (70%), éclaircies, 7°c, vent faible de sud ouest, 1018 hpa en baisse, 595 m3/s Rhin Strasbourg en hausse, comptage 3/5 (mouvements)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland:\nBeau temps, temp=15°c,p1029 hpa, 646 m3/s Rhin Strasbourg, vent faible SSO, comptage difficile avec déplacement d'oiseaux en masse (2/5)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base nautique:\nBeau temps, temp=15°c,p1027 hpa, 646 m3/s Rhin Strasbourg, vent faible SSO, comptage difficile avec groupe de milouin très éloigné collé à la digue tiroir,(3/5)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base nautique :\n19°c, couverte, vent fort SE, 1012hpa en baisse, Q Rhin Strasbourg = 974m3/s, comptage 2/5 (identification difficile)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Temps couvert, 19°c, 1011hpa stable, vent moyen (20-30 km) Sud, Débit du Rhin à Gambsheim aval 792 m3/s, Qualité comptage 3/5 (mouvement oiseaux notamment d'oies et bernaches)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur pointe sud du plan d'eau de Plobsheim\nBeau temps, partiellement couvert, lumineux, 17°c, 1017hpa en baisse, vent faible SO, nuit précédente pertubée avec pluie intense et vents forts (nombreuses branches au sol), Q Rhin Gerstheim aval 729m3/s,comptage 3/5 (mouvements d'oiseaux importants), manque certaines espèces (oies, harle bièvre)?"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland\nBeau temps, partiellement couvert, lumineux, 21°c, 1017hpa en baisse, vent faible SO, Débit Rhin Gerstheim aval 804m3/s, nuit précédente pertubée avec pluie intense et vents forts (nombreuses branches au sol), comptage 3/5 (mouvements d'oiseaux importants)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Base nautique\nBeau temps, partiellement couvert, lumineux, 17°c, 1016hpa en baisse, vent faible SO, nuit précédente pertubée avec pluie intense et vents forts (nombreuses branches au sol), Q Rhin Gerstheim aval 804 m3/scomptage 4/5"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="Secteur Rhinland\n7°c, vent faible SO, 1020hpa en hausse, 100% couvert, faible pluie, Q Rhin =3253m3/s à Gerstheim aval, à partir des points de la digue tiroir + digue ouest, comptage 3/5 (mouvements d'oiseaux)"] <- "douteux"
Rhin$qualite_comptage[Rhin$commentaire_de_la_liste=="température : 0°C ; pression : 1035 hPa en baisse ; débit : 2562 m3/s ; vent faible SO ; brouillard gênant ; pas de pluie"] <- "douteux"


#Ajout colonne suivi site : 

nb_suivi_site <-  
  Rhin %>% 
  count(site, annee)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

Rhin <- merge(Rhin,nb_suivi_site, by.x = "site", by.y = "site")

colnames(Rhin)[47] <- "occurence_site"

#Colonne site retenu: 
Rhin$site_retenu <- with(Rhin, ifelse(Rhin$occurence_site < 3, "non","oui"))

#Colonne nombre d'observation espèces : 
nb_observation <- Rhin %>%
  count(espece)

Rhin <- merge(Rhin,nb_observation, by.x = "espece",by.y = "espece")
colnames(Rhin)[49] <- "nb_observations"

#Colonne valeur médiane, moy ... 
Rhin$abondance <- as.numeric(Rhin$abondance)

median_ab <- Rhin %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Rhin$id_ab <- paste(Rhin$espece,Rhin$site,Rhin$mois,Rhin$annee)

Rhin <- merge(Rhin,median_ab,by.x = "id_ab",by.y="id")

#Ajout colonne voie migration : 

Rhin$voie_migr <- "est_atlantique/mediterranee"

#Tri des colonnes 

Rhin <- Rhin[,-c(1,5,6,8,10,14,15,20,22,24,25,28,29,30,31,33,34,35,36,37,39,40,41,42,43,51,52,53,54)]

colnames(Rhin)[1] <- "espece"
colnames(Rhin)[2] <- "site"
colnames(Rhin)[7] <- "annee"
colnames(Rhin)[8] <- "mois"

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


