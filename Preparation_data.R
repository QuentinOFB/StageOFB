setwd("C:/Users/quentin.petit/Documents/Git/StageOFB")

library(lubridate)
library(dplyr)
library(data.table)
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

data$obs[data$obs=="h vergereau"] <- "vergereau"
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
                                                                                                              ifelse(data$site=="saint_brevin" & data$date=="2013-09-16", "douteux",
                                                                                                                    ifelse(data$date=="2021-08-20" & data$site =="saint_brevin",'douteux',
                                                                                                                            ifelse(data$date=="2017-09-19" & data$site=="migron",'douteux','ok')))))))))))
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

data$date[data$site=="migron" & data$obs =="guenezan, potiron" & data$espece=="canard_pilet"] <- "2017-11-15"

# Lié au nom d'observateur (avec des espaces avant et après)
#data[,8] <- gsub("guenezan m ","guenezan m",data[,8])
#data[,8] <- gsub(" guenezan m","guenezan m",data[,8])

# Enlever les noms de sites absents : 
data <- subset(data, !(data$site==""))

# Colonne mois + années 
unique(data$mois)
data$mois <- month(data$date)

unique(data$annee)
data$annee <- year(data$date)

data$jour_julien <- yday(data$date)

#Création d'une colonne site_retenu 
help("count")

#nb_suivi_site <-  
  #data %>% 
  #count(site, annee)

#nb_suivi_site <- 
  #nb_suivi_site %>%
  #count(site)

#data <- merge(data,nb_suivi_site, by.x = "site", by.y = "site")

#colnames(data)[19] <- "nb_annee_suivie"

#data$site_retenu <- with(data, ifelse(nb_annee_suivie < 3,"non",
    #ifelse(data$site=="baracon","non","oui")))  

# Enlever le comptage du 10/11/2008 : X2 passage sur tous les sites, avec des conditions météo nulles
# + comptage partiel -> remplacé par les comptages du 12/11 et du 24/11 : 
data <- subset(data,!(data$date=="2008-11-10"))

# Double comptage Corsept 18/07/2008 -> erreur de saisi des données, c'est un doublon !
data <- distinct(data)
help("distinct")

data <- subset(data,!(data$site=="saint_brevin"& data$date=="2008-07-18"& data$obs=="potiron"))

data$espece[data$site =="saint_nicolas"&data$date=="2012-11-12"&data$espece=="sarcelle_d_ete"&data$abondance=="7"] <- "sarcelle_d_hiver"

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

nb_observation <- data %>% subset(abondance > 0) %>%
  count(espece)

data <- merge(data,nb_observation, by.x = "espece",by.y = "espece")
colnames(data)[33] <- "occurence_sp"

#Ajouter colonne protocole 

data$protocole <- with(data, ifelse(data$site=="pierre_rouge","bateau",ifelse(
  data$site=="nord_cordemais","bateau", ifelse(data$site=="carnet","bateau",
                                               ifelse(data$site=="pipy","bateau",
                                                      ifelse(data$site=="marechale","bateau",
                                                             ifelse(data$site=="donges","bateau","terrestre")))))))
#Ajouter une colonne "Voie de migration 

data$voie_migration <- "est_atlantique"  

# 1. Création d'un identifiant pour compiler les tables : 
id <- paste0(data$site,data$date)

# 2. Création de la table "site" : 
site <- data.frame(id, data$site,data$secteur,data$protocole, data$qualite_comptage,data$voie_migration)
site <- unique(site) 
table(duplicated(site$id))
# 3. Création de la table inventaire : 

inv <- data.frame(id,data$date,data$obs,data$mois,data$annee)
inv <- unique(inv)
table(duplicated(inv$id))

# Compilation des deux tables : 
data_inv <- merge(site,inv,by.x = "id", by.y = "id")
table(duplicated(data_inv$id))
data_inv <- subset(data_inv, !(data_inv$id=="migron 2017-12-15"& data_inv$data.obs=="guenezan, potiron"))

# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
data$id <- paste0(data$espece,data$site,data$date)
data$id_inv <- paste0(data$site,data$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(data$id_inv)
sp <- unique(data$espece)
unique(data$espece)
inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

data_obs <- data.frame(data$abondance,data$id)
table(duplicated(data_obs$data.id))
data_obs %>%
  group_by(data.id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()


#Erreur liée à une mauvaise saisie des données : 
data_obs <- subset(data_obs, ! (data_obs$data.id=="becasseau_variablemigron2013-02-06" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="becasseau_variablemigron2013-03-07" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2014-11-19" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2013-12-16" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-11-12" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-12-10" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-02-20" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2013-02-08" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2014-03-13" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-01-20" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-10-12" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_cravantgrand_bilho2012-03-19" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_nonnettegrand_bilho2015-02-17" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="bernache_nonnettegrand_bilho2015-01-19" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="chevalier_gambettesaint_nicolas2008-07-18" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2018-12-20" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendrepaimboeuf_corsept2008-07-18" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2017-04-11" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2016-02-17" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2017-02-15" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2018-12-20" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2013-03-07" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2013-01-09" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2017-04-11" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2016-03-21" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2012-01-19" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2020-01-31" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2013-02-06" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2017-12-15" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2017-11-15" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="courlis_cendremigron2015-03-13" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="cygne_tuberculepipy2012-06-19" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="grand_gravelotestuaire2006-01-13" & data_obs$data.abondance=="50"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="grand_gravelotsaint_brevin_mean2008-02-18" & data_obs$data.abondance=="41"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="oie_cendreelavau2005-02-08" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="oie_cendreeestuaire2006-02-13" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="oie_cendreemassereau2006-03-13" & data_obs$data.abondance=="0"))
data_obs <- subset(data_obs, ! (data_obs$data.id=="oie_cendreemassereau2006-03-13" & data_obs$data.abondance=="0"))

#Erreur lié à un double passage 
  #Double passage "unique" => Mean en 2005 - 04 - 12 

  #Plusieurs double passage => Pierre rouge 

data_obs <- aggregate(data_obs, data.abondance ~ data.id, median)

warning(data_obs)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)

# Combinaison des deux tableaux : 

data_f <- merge(inventaire, data_obs, by.x = "id_sp", by.y = "data.id", all.x = T)
View(data)
data_f[is.na(data_f)] <- 0
# Remplacement des NA par des 0

View(data)

# Var 2 -> espece :
colnames(data_f)[names(data_f)== "Var2"] <- "espece"
colnames(data_f)[names(data_f)== "Var1"] <- "id"

#
data_f <- merge(data_f, data_inv, by.x = "id", by.y = "id")
table(duplicated(data_f$id_sp))

# Voir ou sont les doublons 
data_f %>%
  group_by(id_sp) %>%
  filter(n()>1) %>%
  ungroup()
 ######

colnames(data_f) <- gsub("data.","",colnames(data_f))

# On remet les noms latins + famille + ordre

#data_f <- merge(data_f,espece[,c("scientific_name","french_name","family_tax","order_tax")], by.x = "espece", by.y = "french_name", all.x = TRUE)

#Package data.table : d
data_f$abondance <- as.numeric(data_f$abondance)
setDT(data_f)
data_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(data_f)
data_f <- subset(data_f, abondance_tot > 0)

write.csv2(data_f,"Data/estuaire_loire.csv")


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

Camargue$obs[Camargue$obs=="at"] <- "tamisier"
Camargue$obs[Camargue$obs=="jbm"] <- "mouronval"
Camargue$obs[Camargue$obs=="mgc"] <- "gauthier_clerc"

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


Camargue[,17] <- tolower(Camargue[,17])
Camargue[,17] <- iconv(Camargue[,17], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Camargue[,17] <- gsub(" ","_",Camargue[,17]) 
Camargue[,17] <- gsub("'","_",Camargue[,17]) 

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

colnames(Camargue)[29] <- "nb_annee_suivi"

Camargue$site_retenu <- with(Camargue, ifelse(Camargue$nb_annee_suivi < 3 ,"non","oui"))

# NA Dans les abondances, pour le coups il s'agit de vrais NA, dans la mesure ou quand une espèce n'est pas comptée elle est notée
unique(Camargue$abondance)

#retier les NA
Camargue <- subset(Camargue, !(Camargue$abondance=="NA"))


Camargue$abondance <- as.numeric(Camargue$abondance)
setDT(Camargue)
Camargue[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Camargue)
Camargue <- subset(Camargue, abondance_tot > 0)

Camargue$id <- paste0(Camargue$site,Camargue$date)
Camargue$id_sp <- paste0(Camargue$french_name,Camargue$site,Camargue$date)

table(duplicated(Camargue$id_sp))

#Ajout colonne protocole : 
Camargue$protocole <- "avion"

#Ajout de la voie de migration :

Camargue$voie_migration <- "est_atlantique/mediterranee"

#Transferer les canards sp -> colonnes 'french name'
Camargue$french_name[Camargue$espece=="canard_sp"] <- "canard_sp"

setDT(Camargue)
Camargue[, c('id_nico', 'id_input','gel','niveau_eau','redistribution','espece'):=NULL]
setDF(Camargue)

#Renommer la colonne "french_name"
colnames(Camargue) [11] <- "espece"

write.csv2(Camargue, "Data/camargue.csv")


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

#les espèces inderterminées : 

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

colnames(Baie)[22] <- "nb_annee_suivie"

# Création d'une colonne site retenu 

Baie$site_retenu <- with(Baie, ifelse(Baie$nb_annee_suivie < 3,"non","oui"))

#Format date 
unique(Baie$date) #de 1977 à 2024
Baie$date <- dmy(Baie$date)

#Nom des observateurs : 
unique(Baie$obs)
Baie[,6] <- tolower(Baie[,6])
Baie[,6] <- iconv(Baie[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Baie[,6] <- gsub(" lpo 17","lpo 17",Baie[,6])

Baie$obs[Baie$obs=="gonin c., gueret jean-pierre, joyeux emmanuel, lagadec e., marquis jacques"] <- "gonin, gueret, joyeux, lagadec, marquis"
Baie$obs[Baie$obs=="gueret jean-pierre, lagadec e., marquis jacques, puyo s., verdier j."] <- "gueret, lagadec, marquis, puyo, verdier"
Baie$obs[Baie$obs=="gueret jean-pierre, marquis jacques"] <- "gueret, marquis"
Baie$obs[Baie$obs=="gueret jean-pierre, marquis jacques, sudraud julien"] <- "gueret, marquis, sudraud"
Baie$obs[Baie$obs=="gueret jean-pierre, joyeux emmanuel, marquis jacques, sudraud julien"] <- "gueret, joyeux, marquis, sudraud"
Baie$obs[Baie$obs=="mercier fabien"] <- "mercier"
Baie$obs[Baie$obs=="equipe rn ba"] <- "equipe rn ba"
Baie$obs[Baie$obs=="joyeux emmanuel"] <- "joyeux"
Baie$obs[Baie$obs=="marquis jacques"] <- "marquis"
Baie$obs[Baie$obs=="gallais regis"] <- "gallais"
Baie$obs[Baie$obs=="bergere herve"] <- "bergere"
Baie$obs[Baie$obs=="chauveau herve"] <- "chauveau"
Baie$obs[Baie$obs=="gonin c., gueret jean-pierre, joyeux emmanuel, marquis jacques, mercier fabien, sudraud julien"] <- "gonin, gueret, joyeux, marquis, mercier, sudraud"
Baie$obs[Baie$obs=="lagrange pamela"] <- "lagrange"
Baie$obs[Baie$obs=="payen paul"] <- "payen"
Baie$obs[Baie$obs=="gillette christophe"] <- "gillette"
Baie$obs[Baie$obs=="bobineau maxime"] <- "bobineau"
Baie$obs[Baie$obs=="haie sylvain"] <- "haie"
Baie$obs[Baie$obs=="sudraud julien"] <- "sudraud"
Baie$obs[Baie$obs=="gueret jean-pierre"] <- "gueret"
Baie$obs[Baie$obs=="gueret jean-pierre, sudraud julien"] <- "gueret, sudraud"
Baie$obs[Baie$obs=="gueret jean-pierre, lagadec e., marquis jacques"] <- "gueret, lagadec, marquis"
Baie$obs[Baie$obs=="deplaine lucas"] <- "deplaine"
Baie$obs[Baie$obs=="houdoin c."] <- "houdoin"
Baie$obs[Baie$obs=="gonin julien"] <- "gonin"
Baie$obs[Baie$obs=="brochard m., gueret jean-pierre, marquis jacques, sudraud julien"] <- "brochard, gueret, marquis, sudraud"
Baie$obs[Baie$obs=="guegnard aurelie"] <- "guegnard"
Baie$obs[Baie$obs=="chambrelin justin"] <- "chambrelin"
Baie$obs[Baie$obs=="gueret jean-pierre, marquis jacques, thomas a."] <- "gueret, marquis, thomas"
Baie$obs[Baie$obs=="gueret jean-pierre, marquis jacques, pallier s., sudraud julien"] <- "gueret, marquis, pallier, sudraud"
Baie$obs[Baie$obs=="marquis jacques, gonin c, rochier d."] <- "marquis, gonin, rochier"
Baie$obs[Baie$obs=="adev, gonin c., gueret jean-pierre, thomas a., sudraud julien"] <- "adev, gonin, gueret, thomas, sudraud"
Baie$obs[Baie$obs=="sudraud julien, marquis jacques"] <- "sudraud, marquis"
Baie$obs[Baie$obs=="bonnin p., gueret jean-pierre, gonin c., joyeux emmanuel, marquis jacques"] <- "bonnin, gueret, gonin, joyeux, marquis"
Baie$obs[Baie$obs=="duranel thomas"] <- "duranel"
Baie$obs[Baie$obs=="moneuse steve"] <- "moneuse"
Baie$obs[Baie$obs=="gourraud lydie"] <- "gourraud"
Baie$obs[Baie$obs=="decoene delphine"] <- "decoene"
Baie$obs[Baie$obs=="marquis jacques, gueret jean-pierre, sudraud julien"] <- "marquis, gueret, sudraud"
Baie$obs[Baie$obs=="pinto laura"] <- "pinto"
Baie$obs[Baie$obs=="gueret jean-pierre, gonin c., joyeux emmanuel, marquis jacques"] <- "gueret, gonin, joyeux, marquis"
Baie$obs[Baie$obs=="adams john"] <- "adams"
Baie$obs[Baie$obs=="texier alain"] <- "texier"
Baie$obs[Baie$obs=="drion l."] <- "drion"
Baie$obs[Baie$obs=="marquis jacques, sudraud julien"] <- "marquis, sudraud"
Baie$obs[Baie$obs=="fouquet michel"] <- "fouquet"
Baie$obs[Baie$obs=="corre f., gueret jean-pierre, gonin c., joyeux emmanuel, marquis jacques"] <- "corre, gueret, gonin, joyeux, marquis"
Baie$obs[Baie$obs=="seguin marlene"] <- "seguin"
Baie$obs[Baie$obs=="welch geoff & hilary"] <- "welch"
Baie$obs[Baie$obs=="turpaud-fizzala victor"] <- "turpaud-fizzala"
Baie$obs[Baie$obs=="martineau adrien"] <- "martineau"
Baie$obs[Baie$obs=="welch geoff"] <- "welch"
Baie$obs[Baie$obs=="artaud jean-marie"] <- "artaud"
Baie$obs[Baie$obs=="bernard charlie"] <- "bernard"
Baie$obs[Baie$obs=="blanc jean-francois"] <- "blanc"
Baie$obs[Baie$obs=="trouillard daniel"] <- "trouillard"
Baie$obs[Baie$obs=="hanotel remi"] <- "hanotel"
Baie$obs[Baie$obs=="laplace melanie"] <- "laplace"
Baie$obs[Baie$obs=="gore olivier"] <- "gore"
Baie$obs[Baie$obs=="tullie laurent"] <- "tullie"
Baie$obs[Baie$obs=="dulac perrine"] <- "dulac"
Baie$obs[Baie$obs=="le bail jean-louis"] <- "le bail"
Baie$obs[Baie$obs=="thomas alain"] <- "thomas"
Baie$obs[Baie$obs=="romet nicolas"] <- "romet"
Baie$obs[Baie$obs=="lastere pierre"] <- "lastere"
Baie$obs[Baie$obs=="fernandez fabien"] <- "fernandez"
Baie$obs[Baie$obs=="barbier stephane"] <- "barbier"
Baie$obs[Baie$obs=="bouet jacques-martial"] <- "bouet"
Baie$obs[Baie$obs=="bonnin pascal"] <- "bonnin"
Baie$obs[Baie$obs=="grit anaide"] <- "grit"
Baie$obs[Baie$obs=="bariteau jean-robert"] <- "bariteau"
Baie$obs[Baie$obs=="de bouet du portal pierre"] <- "de bouet du portal"
Baie$obs[Baie$obs=="blanchet romain"] <- "blanchet"
Baie$obs[Baie$obs=="molinari francois"] <- "molinari"
Baie$obs[Baie$obs=="francesiaz charlotte"] <- "francesiaz"
Baie$obs[Baie$obs=="froud louise"] <- "froud"
Baie$obs[Baie$obs=="reynaud jean-stephane"] <- "reynaud"
Baie$obs[Baie$obs=="mercier olivia"] <- "mercier"
Baie$obs[Baie$obs=="goossens helene"] <- "goossens"
Baie$obs[Baie$obs=="marsaud louis"] <- "marsaud"
Baie$obs[Baie$obs=="marine marie"] <- "marine"
Baie$obs[Baie$obs=="chenu audran"] <- "chenu"
Baie$obs[Baie$obs=="prioul m."] <- "prioul"
Baie$obs[Baie$obs=="meuraillon yohan"] <- "meuraillon"
Baie$obs[Baie$obs=="tailpied florian"] <- "tailpied"
Baie$obs[Baie$obs=="girard olivier"] <- "girard"
Baie$obs[Baie$obs=="croise jean-claude"] <- "croise"
Baie$obs[Baie$obs=="marquis olivier"] <- "marquis_O"
Baie$obs[Baie$obs=="pirio maxime"] <- "pirio"
Baie$obs[Baie$obs=="portier frederic"] <- "portier"
Baie$obs[Baie$obs=="daviaud elisa"] <- "daviaud"
Baie$obs[Baie$obs=="dupuy jeremy & daviaud elisa"] <- "dupuy, daviaud"
Baie$obs[Baie$obs=="dupuy jeremy"] <- "dupuy"
Baie$obs[Baie$obs=="corre frederic"] <- "corre"
Baie$obs[Baie$obs=="petit loic"] <- "petit"
Baie$obs[Baie$obs=="joyeux emmanuel, marquis jacques"] <- "joyeux, marquis"
Baie$obs[Baie$obs=="jacob herve"] <- "jacob"
Baie$obs[Baie$obs=="martin guillaume"] <- "martin"
Baie$obs[Baie$obs=="corre f., joyeux emmanuel"] <- "corre, joyeux"
Baie$obs[Baie$obs=="lafond jean-pierre"] <- "lafond"
Baie$obs[Baie$obs=="le bihan audrey"] <- "le bihan"
Baie$obs[Baie$obs=="caupenne michel"] <- "caupenne"
Baie$obs[Baie$obs=="leroux cecile"] <- "leroux"
Baie$obs[Baie$obs=="duval sebastien"] <- "duval"
Baie$obs[Baie$obs=="francois andre"] <- "francois"
Baie$obs[Baie$obs=="trotignon paul"] <- "trotignon"
Baie$obs[Baie$obs=="palier sebastien"] <- "palier"
Baie$obs[Baie$obs=="boullard c."] <- "boullard"
Baie$obs[Baie$obs=="hunault sylvain"] <- "hunault"
Baie$obs[Baie$obs=="dolle pierre"] <- "dolle"
Baie$obs[Baie$obs=="des touches hugues"] <- "des touches"
Baie$obs[Baie$obs=="laurent thibault, roy alexandre"] <- "laurent, roy"
Baie$obs[Baie$obs=="arnaud"] <- "arnaud"
Baie$obs[Baie$obs=="leve f."] <- "leve"
Baie$obs[Baie$obs=="ball r., moinard g."] <- "ball, moinard"
Baie$obs[Baie$obs=="ball r."] <- "ball"
Baie$obs[Baie$obs=="thevenon g."] <- "thevenon"
Baie$obs[Baie$obs=="pillier jonathan"] <- "pillier"
Baie$obs[Baie$obs=="meunier francis"] <- "meunier"
Baie$obs[Baie$obs=="gian franco alessandria"] <- "gian franco"
Baie$obs[Baie$obs=="raiffaud alain"] <- "raiffaud"
Baie$obs[Baie$obs=="martineau rouet corinne"] <- "martineau rouet"
Baie$obs[Baie$obs=="gaugris yves"] <- "gaugris"
Baie$obs[Baie$obs=="cleva roland"] <- "cleva"
Baie$obs[Baie$obs=="equipe rn sdp"] <- "rn sdp"

#Les différents protocoles : 
unique(Baie$type_protocole)
# -> Dans comptage simultané grues : 2 observations :  bécassine des marais + colvert 
Baie <- subset(Baie, !(Baie$type_protocole=="Comptage simultané grues"))

#Voir les abondance : 
unique(Baie$abondance)
unique(Baie$abondance[1000:1595])

#Présence de NA : les supprimer ? (correspond à une "non prospection")
Baie <- subset(Baie,!(Baie$abondance=="NA"))


#Prendre en compte les remarques : 
unique(Baie$remarques)
Baie[,15]<- iconv(Baie[,15], from = 'UTF-8', to = 'ASCII//TRANSLIT')
Baie[,15] <- gsub(" ","_",Baie[,15])
#Ajouter la colonne qualité du comptage : 
sort(unique(Baie$site))
# Uniformisation des remarques (pour que ça passe avec ifelse) 
Baie$qualite_comptage <- with(Baie, ifelse(Baie$site=="arcay" & Baie$date=="2019-10-14" , "douteux", 
                                    ifelse(site=="arcay" & date =="2019-12-20","douteux", 
                                    ifelse(site=="la_bosse_(rnba)" & date=="2019-12-20", "douteux",
                                    ifelse(site=="le_cure_(rnba)" & date=="2019-12-20", "douteux",
                                    ifelse(site=="tdcl_le_cure" & date == "2019-12-20","douteux",
                                    ifelse(site=="tdcl_pree_mizottiere" & date =="2019-12-20","douteux",
                                    ifelse(site=="lagunage_de_la_tranche_sur_mer" & date=="2020-01-10", "douteux", 
                                    ifelse(site=="les_casserottes" & date=="2020-12-14","douteux", 
                                    ifelse(site=="marais_de_la_guittiere" & date =="2013-10-17","douteux",
                                    ifelse(site=="pointe_de_l_aiguillon_(apb)" & date =="2021-03-15","douteux",
                                    ifelse(site=="pointe_saint_clement_(rnba)" & date == "2020-09-16","douteux",
                                    ifelse(site=="rnr_ferme_de_choisy" & date == "2020-02-18","douteux", 
                                    ifelse(site == "tdcl_le_cure" & date=="2020-09-16","douteux",
                                    ifelse(site== "tdcl_le_cure" & date== "2021-09-20", "douteux",
                                    ifelse(site== "la_bosse_(rnba)" & date == "2022-02-17", "douteux", 
                                    ifelse(site== "le_cure_(rnba)" & date == "2022-02-17", "douteux",
                                    ifelse(site== "les_chaines_(rnba)" & date == "2022-02-17", "douteux",
                                    ifelse(site== "mirador_(rnba)" & date == "2022-02-17", "douteux",
                                    ifelse(site== "transfo_(rnba)" & date == "2022-02-17", "douteux",
                                    ifelse(site== "la_marina_(rnba)" & date == "2022-09-09", "douteux",
                                    ifelse(site== "les_chaines_(rnba)" & date == "2022-09-09","douteux",
                                    ifelse(site== "la_marina_(rnba)" & date == "2022-10-24", "douteux",
                                    ifelse(site== "les_chaines_(rnba)" & date == "2022-10-24","douteux",
                                    ifelse(site == "pointe_saint_clement_(rnba)" & date == "2022-09-09","douteux",
                                    ifelse(site == "reposoir_principal_(rnba)" & date == "2022-10-22", "douteux",
                                    ifelse(site == "mirador_(rnba)" & date == "2022-10-22", "douteux", 
                                    ifelse(site == "le_cure_(rnba)" & date == "2023-01-23", "douteux",
                                    ifelse(site == "tdcl_le_cure" & date == "2023-01-20", "douteux",
                                    ifelse(site == "tdcl_le_cure" & date == "2023-02-23", "douteux",
                                    ifelse(site == "le_cure_(rnba)" & date == "2023-02-23", "douteux",
                                    ifelse(site == "les_casserottes" & date == "2022-11-22", "douteux",
                                    ifelse(date == "2022-12-22","douteux",
                                    ifelse(date=="1987-02-13" & site =="arcay", "douteux",
                                    ifelse(date=="1987-02-13"& site =="rnn_baie_de_l_aiguillon","douteux",
                                    ifelse(date=="2020-12-14" & site == "les_casserottes","douteux", 
                                    ifelse(date=="2023-01-20" & site == "transfo_(rnba)","douteux",
                                    ifelse(date=="2023-03-10" & site == "la_marina_(rnba)","douteux",
                                    ifelse(date=="2023-03-10" & site == "transfo_(rnba)","douteux",
                                    ifelse(date=="2023-03-10" & site == "pointe_saint_clement_(rnba)","douteux",
                                    ifelse(date=="2023-02-23" & site == "reposoir_principal_(rnba)","douteux",
                                    ifelse(date=="2023-02-23" & site == "les_chaines_(rnba)", "douteux",
                                    ifelse(date=="2023-02-23" & site == "rcfs_pointe_d_arcay_(arcay)", "douteux",
                                    ifelse(date=="2023-02-23" & site == "les_casserottes", "douteux", "ok"))))))))))))))))))))))))))))))))))))))))))))
                                    
Baie$qualite_comptage[Baie$site=="rade_d_amour_(arcay)" & Baie$date=="2023-02-23"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="plans_d_eau_de_l_aiguillon" & Baie$date=="2023-02-23"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="lagunage_de_la_tranche_sur_mer" & Baie$date=="2023-02-23"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="communal_d_angles" & Baie$date=="2023-02-23"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="lagunage_de_longeville" & Baie$date=="2023-02-23"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="pointe_saint_clement_(rnba)" & Baie$date=="2023-09-15"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="rnr_ferme_de_choisy" & Baie$date=="2023-09-20"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="pointe_saint_clement_(rnba)" & Baie$date=="2023-10-16"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="rnr_poire_sur_velluire" & Baie$date=="2023-11-17"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="la_vacherie" & Baie$date=="2023-11-17"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="communal_de_lairoux_curzon" & Baie$date=="2023-11-17"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="mirador_(rnba)" & Baie$date=="2023-12-13"] <- "douteux"                                          
Baie$qualite_comptage[Baie$site=="la_vacherie" & Baie$date=="2023-12-14"] <- "douteux"                                          

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
setDT(Baie)
Baie[, c('obse_taxo_id', 'obse_id','obse_relv_id','confidentialite','validation','obse_nom','id_habitat','x','x.1'):=NULL]
setDF(Baie)



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


#Création colonne voie de migration : 
Baie$voie_migratoire <- "est_atlantique"

# 1. Création d'un identifiant pour compiler les tables : 
id <- paste0(Baie$site,Baie$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Baie$site, Baie$secteur, Baie$protocole, Baie$nb_annee_suivie, Baie$qualite_comptage, Baie$voie_migratoire, Baie$site_retenu)
site <- unique(site) 
table(duplicated(site$id))

# 3. Création de la table inventaire : 

inv <- data.frame(id,Baie$date,Baie$obs,Baie$mois,Baie$annee)
inv <- unique(inv)
table(duplicated(inv$id))
inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% print(n = 42)

# Régler le problème des observateurs (deux observateurs pour un même site/même date -> créer des doublons dans les lignes)

inv$Baie.obs[inv$id=="arcay2019-06-18"] <- "joyeux, marquis"
inv$Baie.obs[inv$id=="communal_de_chasnais2002-01-15"] <- "gueret, adams"
inv$Baie.obs[inv$id=="la_marina_(rnba)2023-11-15"] <- "goossens, blanchet"
inv$Baie.obs[inv$id=="la_marina_(rnba)2023-02-23"] <- "goossens, blanchet"
inv$Baie.obs[inv$id=="la_marina_(rnba)2023-03-10"] <- "froud, goossens"
inv$Baie.obs[inv$id=="la_vacherie2013-04-11"] <- "gueret, girard"
inv$Baie.obs[inv$id=="lagunage_de_la_tranche_sur_mer2002-06-13"] <- "fouquet, marquis"
inv$Baie.obs[inv$id=="le_cure_(rnba)2021-07-22"] <- "blanc, reynaud"
inv$Baie.obs[inv$id=="les_chaines_(rnba)2022-11-22"] <- "blanc, goossens"
inv$Baie.obs[inv$id=="les_chaines_(rnba)2019-08-14"] <- "moneuse, reynaud"
inv$Baie.obs[inv$id=="marais_de_landelene2014-07-09"] <- "petit, mercier"
inv$Baie.obs[inv$id=="mirador_(rnba)2021-12-20"] <- "lagrange, bonnin"
inv$Baie.obs[inv$id=="mirador_(rnba)2023-02-23"] <- "froud, chenu"
inv$Baie.obs[inv$id=="mirador_(rnba)2023-06-16"] <- "bariteau, goossens"
inv$Baie.obs[inv$id=="pointe_saint_clement_(rnba)2019-05-17"] <- "reynaud, gueret"
inv$Baie.obs[inv$id=="rnn_casse_de_la_belle_henriette2021-08-19"] <- "trotignon, chambrelin"
inv$Baie.obs[inv$id=="pointe_saint_clement_(rnba)2022-11-22"] <- "caupenne, chauveau"
inv$Baie.obs[inv$id=="reposoir_principal_(rnba)2023-12-13"] <- "gallais, chenu"
inv$Baie.obs[inv$id=="tdcl_pree_mizottiere2024-01-11"] <- "goossens, froud"
inv$Baie.obs[inv$id=="transfo_(rnba)2023-04-18"] <- "bariteau, goossens"
inv$Baie.obs[inv$id=="transfo_(rnba)2021-03-15"] <- "francois, bergere"

inv <- unique(inv)

# Compilation des deux tables : 
Baie_inv <- merge(site,inv,by.x = "id", by.y = "id")
duplicated(Baie_inv$id)


# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Baie$id <- paste0(Baie$espece,Baie$site,Baie$date)
Baie$id_inv <- paste0(Baie$site,Baie$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Baie$id_inv)
sp <- unique(Baie$espece)

inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)

Baie_obs <- data.frame(Baie$abondance,Baie$id)
Baie_obs <- aggregate(Baie_obs, Baie.abondance ~ Baie.id, median)

# Combinaison des deux tableaux : 

Baie_f <- merge(inventaire, Baie_obs, by.x = "id_sp", by.y = "Baie.id", all.x = T)
View(Baie)
Baie_f <- distinct(Baie_f)
# Remplacement des NA par des 0

Baie_f[is.na(Baie_f)] <- 0

# Var 2 -> espece :
colnames(Baie_f)[names(Baie_f)== "Var2"] <- "espece"
colnames(Baie_f)[names(Baie_f)== "Var1"] <- "id"

# Merge des tables : 
Baie_f <- merge(Baie_f, Baie_inv, by.x = "id", by.y = "id")

colnames(Baie_f) <- gsub("Baie.","",colnames(Baie_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Baie_f$abondance <- as.numeric(Baie_f$abondance)
setDT(Baie_f)
Baie_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Baie_f)
Baie_f <- subset(Baie_f, abondance_tot > 0)

write.csv2(Baie_f,"Data/Baie_aiguillon.csv")

#Nombre d'observation des espèces 
nb_observation <- Baie %>%
  count(espece)

Baie <- merge(Baie,nb_observation, by.x = "espece",by.y = "espece")
colnames(Baie)[22] <- "nb_observations"     
#Création d'une colonne observations pour les espèces : 
Baie$abondance <- as.numeric(Baie$abondance)

median_ab <- Baie %>%
  group_by(espece,mois,site,annee) %>%
  summarise(abondance_moy=mean(abondance), abondance_max=max(abondance), abondance_min=min(abondance), abondance_median=median(abondance))

median_ab$id <- paste(median_ab$espece,median_ab$site,median_ab$mois,median_ab$annee)

Baie$id_ab <- paste(Baie$espece,Baie$site,Baie$mois,Baie$annee)

Baie <- merge(Baie,median_ab,by.x = "id_ab",by.y="id")


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



#Nom minuscules :

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

Cotentin[,1] <- gsub("'","_",Cotentin[,1])


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

colnames(Cotentin)[15] <- "nb_annee_suivi"

# Création d'une colonne site retenu 

Cotentin$site_retenu <- with(Cotentin, ifelse(Cotentin$site=="rnn_beauguillot","non",      
                                    ifelse(Cotentin$nb_annee_suivi < 3,"non","oui")))
  
#Vérification abondance : 
unique(Cotentin$abondance)
# Attention à certains moments des fourchettes sont données ex : 420-440 
# Que faire ? Prendre la moyenne, le min, le max, ou supprimer ? 

Cotentin$abondance[Cotentin$abondance=="420-440"] <- "440"
Cotentin$abondance[Cotentin$abondance=="380-420"] <- "420"
Cotentin$abondance[Cotentin$abondance=="15-20"] <- "20"

#Les observateurs :  
unique(Cotentin$obs)

Cotentin$obs[Cotentin$obs=="blond mickael & caille marion"] <- "blond, caille"
Cotentin$obs[Cotentin$obs=="caillot emmanuel"] <- "caillot"
Cotentin$obs[Cotentin$obs=="blond mickael & galloo thierry (symel)"] <- "blond, galloo"
Cotentin$obs[Cotentin$obs=="galloo thierry"] <- "galloo"
Cotentin$obs[Cotentin$obs=="elder jean-francois"] <- "elder"
Cotentin$obs[Cotentin$obs=="observateurs du reseau limicoles cotiers de la baie de veys et du littoral est-cotentin"] <- "obs_reseau_limicoles"
Cotentin$obs[Cotentin$obs=="blond mickael"] <- "blond"
Cotentin$obs[Cotentin$obs=="galloo thierry (symel)"] <- "galloo"
Cotentin$obs[Cotentin$obs=="galloo thierry & suardi quentin"] <- "galloo, suardi"
Cotentin$obs[Cotentin$obs=="elder jean-francois & trouverie nathan"] <- "elder, trouverie"
Cotentin$obs[Cotentin$obs=="blond mickael & caillot emmanuel"] <- "blond, caillot"
Cotentin$obs[Cotentin$obs=="elder jean-francois & galloo thierry"] <- "elder, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael & davignon dimitri"] <- "blond, davignon"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, lebreton lucas & robert lili"] <- "blond, caillot, lebreton, robert"
Cotentin$obs[Cotentin$obs=="elder jean-francois & suardi quentin"] <- "elder, suardi"
Cotentin$obs[Cotentin$obs=="elder jean-francois & mauduit geraldine"] <- "elder, mauduit"
Cotentin$obs[Cotentin$obs=="bunel marie & caillot emmanuel"] <- "bunel, caillot"
Cotentin$obs[Cotentin$obs=="blond mickael, galloo thierry (symel) & leconte christian"] <- "blond, galloo, leconte"
Cotentin$obs[Cotentin$obs=="gabet ludivine"] <- "gabet"
Cotentin$obs[Cotentin$obs=="suardi quentin"] <- "suardi"
Cotentin$obs[Cotentin$obs=="caillot emmanuel & lebreton lucas"] <- "caillot, lebreton"
Cotentin$obs[Cotentin$obs=="blond mickael, caille marion & caillot emmanuel"] <- "blond, caille, caillot"
Cotentin$obs[Cotentin$obs=="fillol nicolas & galloo thierry"] <- "fillol, galloo"
Cotentin$obs[Cotentin$obs=="elder jean-francois & lesouef quentin"] <- "elder, lesouef"
Cotentin$obs[Cotentin$obs=="blond mickael, caille marion & davignon dimitri"] <- "blond, caille, davignon"
Cotentin$obs[Cotentin$obs=="galloo thierry & laurent sylvain"] <- "galloo, laurent"
Cotentin$obs[Cotentin$obs=="blond mickael, davignon dimitri & lebreton lucas"] <- "blond, davignon, lebreton"
Cotentin$obs[Cotentin$obs=="blond mickael & robert lili"] <- "blond, robert"
Cotentin$obs[Cotentin$obs=="gabet ludivine & suardi quentin"] <- "gabet, suardi"
Cotentin$obs[Cotentin$obs=="galloo thierry & lesouef quentin"] <- "galloo, lesouef"
Cotentin$obs[Cotentin$obs=="caillot emmanuel & elder jean-francois"] <- "caillot, elder"
Cotentin$obs[Cotentin$obs=="gabet ludivine, galloo thierry & laurent sylvain"] <- "gabet, galloo, laurent"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry & laurent sylvain"] <- "elder, galloo, laurent"
Cotentin$obs[Cotentin$obs=="fillol nicolas, galloo thierry & laurent sylvain"] <- "fillol, galloo, laurent"
Cotentin$obs[Cotentin$obs=="gabet ludivine & galloo thierry"] <- "gabet, galloo"
Cotentin$obs[Cotentin$obs=="elder jean-francois & fillol nicolas"] <- "elder, fillol"
Cotentin$obs[Cotentin$obs=="elder jean-francois & purenne regis"] <- "elder, purenne"
Cotentin$obs[Cotentin$obs=="fillol nicolas"] <- "fillol"
Cotentin$obs[Cotentin$obs=="fillol nicolas, laurent sylvain & suardi quentin"] <- "fillol, laurent, suardi"
Cotentin$obs[Cotentin$obs=="elder jean-francois & provost sebastien"] <- "elder, provost"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry, laurent sylvain & wodey alexis"] <- "elder, galloo, laurent, wodey"
Cotentin$obs[Cotentin$obs=="blond mickael & elder jean-francois"] <- "blond, elder"
Cotentin$obs[Cotentin$obs=="blond mickael, elder jean-francois & galloo thierry (symel)"] <- "blond, elder, galloo"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry & lesouef quentin"] <- "elder, galloo, lesouef"
Cotentin$obs[Cotentin$obs=="elder jean-francois & jean-baptiste james"] <- "elder, jean-baptiste"
Cotentin$obs[Cotentin$obs=="elder jean-francois, grof yvan & laurent sylvain"] <- "elder, grof, laurent"
Cotentin$obs[Cotentin$obs=="blond, caillot, elder, leconte"] <- "blond, caillot, elder, leconte"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry & wodey alexis"] <- "elder, galloo, wodey"
Cotentin$obs[Cotentin$obs=="bachelet anthony, blond mickael, caillot emmanuel & galloo thierry (symel)"] <- "bachelet, blond, caillot, galloo"
Cotentin$obs[Cotentin$obs=="caillot emmanuel, elder jean-francois, mordel francois & quesnel maxime"] <- "caillot, elder, mordel, quesnel"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry & suardi quentin"] <- "elder, galloo, suardi"
Cotentin$obs[Cotentin$obs=="blond mickael, elder jean-francois, galloo thierry (symel) & lebarbey nathan"] <- "blond, elder, galloo, lebarbey"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, elder jean-francois, galloo thierry (symel) & leconte christian"] <- "blond, caillot, elder, galloo, leconte"
Cotentin$obs[Cotentin$obs=="galloo thierry, laurent sylvain & suardi quentin"] <- "galloo, laurent, suardi"
Cotentin$obs[Cotentin$obs=="caillot emmanuel & galloo thierry (symel)"] <- "caillot, galloo"
Cotentin$obs[Cotentin$obs=="galloo thierry, laurent sylvain & schmitt emmanuel"] <- "galloo, laurent, schmitt"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, davignon dimitri & elder jean-francois"] <- "blond, caillot, davignon, elder"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, elder jean-francois & galloo thierry (symel)"] <- "blond, caillot, elder, galloo"
Cotentin$obs[Cotentin$obs=="elder jean-francois, gabet ludivine & galloo thierry"] <- "elder, gabet, galloo"
Cotentin$obs[Cotentin$obs=="caillot emmanuel & kuhn victoire"] <- "caillot, kuhn"
Cotentin$obs[Cotentin$obs=="elder jean-francois, galloo thierry, lesouef quentin & wodey alexis"] <- "elder, galloo, lesouef, wodey"
Cotentin$obs[Cotentin$obs=="bachelet anthony & caillot emmanuel"] <- "bachelet, caillot"
Cotentin$obs[Cotentin$obs=="galloo thierry, laurent sylvain & wodey alexis"] <- "galloo, laurent, wodey"
Cotentin$obs[Cotentin$obs=="caillot emmanuel, elder jean-francois & galloo thierry (symel)"] <- "caillot, elder, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, davignon dimitri & galloo thierry (symel)"] <- "blond, caillot, davignon, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael, davignon dimitri & elder jean-francois"] <- "blond, davignon, elder"
Cotentin$obs[Cotentin$obs=="elder jean-francois & laurent sylvain"] <- "elder, laurent"
Cotentin$obs[Cotentin$obs=="elder jean-francois & gabet ludivine"] <- "elder, gabet"
Cotentin$obs[Cotentin$obs=="blond mickael, elder jean-francois, gabet ludivine & galloo thierry (symel)"] <- "blond, elder, gabet, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel, davignon dimitri, elder jean-francois & galloo thierry (symel)"] <- "blond, caillot, davignon, elder, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael & leconte christian"] <- "blond, leconte"
Cotentin$obs[Cotentin$obs=="galloo thierry & lecaplain benoit"] <- "galloo, lecaplain"
Cotentin$obs[Cotentin$obs=="elder jean-francois & galloo thierry (symel)"] <- "elder, galloo"
Cotentin$obs[Cotentin$obs=="galloo thierry, laurent sylvain & lesouef quentin"] <- "galloo, laurent, lesouef"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel & galloo thierry (symel)"] <- "blond, caillot, galloo"
Cotentin$obs[Cotentin$obs=="blond mickael, elder jean-francois, galloo thierry (symel) & leconte christian"] <- "blond, elder, galloo, leconte"
Cotentin$obs[Cotentin$obs=="bunel marie & elder jean-francois"] <- "bunel, elder"
Cotentin$obs[Cotentin$obs=="galloo thierry & mauger remi"] <- "galloo, mauger"
Cotentin$obs[Cotentin$obs=="gabet ludivine, laurent sylvain & suardi quentin"] <- "gabet, laurent, suardi"
Cotentin$obs[Cotentin$obs=="blond mickael, davignon dimitri & galloo thierry (symel)"] <- "blond, davignon, galloo"
Cotentin$obs[Cotentin$obs=="caillot emmanuel & grossin emmanuel"] <- "caillot, grossin"
Cotentin$obs[Cotentin$obs=="caillot emmanuel, collas precillia, galloo thierry (symel) & masquerel lola"] <- "caillot, collas, galloo, masquerel"
Cotentin$obs[Cotentin$obs=="gabet ludivine, galloo thierry & suardi quentin"] <- "gabet, galloo, suardi"
Cotentin$obs[Cotentin$obs=="laurent sylvain"] <- "laurent"
Cotentin$obs[Cotentin$obs=="vimard gilbert"] <- "vimard"
Cotentin$obs[Cotentin$obs=="elder jean-francois, gabet ludivine, galloo thierry & laurent sylvain"] <- "elder, gabet, galloo, laurent"
Cotentin$obs[Cotentin$obs=="blond mickael, caillot emmanuel & elder jean-francois"] <- "blond, caillot, elder"
Cotentin$obs[Cotentin$obs=="gabet ludivine & morin remi"] <- "gabet, morin"
Cotentin$obs[Cotentin$obs=="gabet ludivine & menard helene"] <- "gabet, menard"
Cotentin$obs[Cotentin$obs=="davignon dimitri"] <- "davignon"
Cotentin$obs[Cotentin$obs=="galloo thierry & menard helene"] <- "galloo, menard"
Cotentin$obs[Cotentin$obs=="menard helene"] <- "menard"
Cotentin$obs[Cotentin$obs=="bouillon emmanuelle & gabet ludivine"] <- "bouillon, gabet"
Cotentin$obs[Cotentin$obs=="gabet ludivine, galloo thierry, laurent sylvain & suardi quentin"] <- "gabet, galloo, laurent, suardi"
Cotentin$obs[Cotentin$obs=="gabet ludivine & parmentier emmanuel"] <- "gabet, parmentier"
Cotentin$obs[Cotentin$obs=="schmitt emmanuel"] <- "schmitt"


#####Prendre en compte les remarques :
unique(Cotentin$remarques)

Cotentin[,12] <- gsub(":","_",Cotentin[,12])
Cotentin[,12] <- gsub("/","_",Cotentin[,12])
Cotentin[,12] <- gsub(" ","_",Cotentin[,12])
Cotentin[,12] <- iconv(Cotentin[,12], from = 'UTF-8', to ='ASCII//TRANSLIT')
#Uniformisation des remarques : 

Cotentin$remarques[Cotentin$remarques== "Eau-milieux_(plans_d'eau)__Normaux___Eau-milieux_(hors_plans_d'eau)___Inondes___Precision_du_denombrement___Bonne_precision___Conditions_de_denombrement___Bonnes____Facteur_1______Facteur_2______Facteur_3______Decompte___Decompte_total___Remarque___Amenagement_RNN_Travaux_terrassement_chemin_des_observatoires"] <- "derangement_travaux"
Cotentin$remarques[Cotentin$remarques=="Eau-milieux_(plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Eau-milieux_(hors_plans_d'eau)______Conditions_de_denombrement___Mauvaises___Facteur_1___Vent___Decompte___Decompte_total___Remarque___fosses,_prairies_gelees,_froid_intense,_mer_agitee"] <- "conditions_meteo_pas_fav"
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
Cotentin$voie_migration <- "est_atlantique"

#Retirer les anatidés (+vanneau et pluvier doré) du suivi limicoles côtiers 

limic <- Cotentin |> 
  filter(suivi |>  str_detect("limicoles"))

unique(limic$espece)
limic <- subset(limic, (espece=="bernache_cravant"|espece=="eider_a_duvet"|espece=="harle_huppe"|espece=="tadorne_de_belon"
                        |order_tax=="charadriiformes"))
unique(limic$espece)

#On retire également vanneau +pluvier doré et bécassines qui ne sont pas comptées de manière exhaustive :

limic <- subset(limic, !(espece=="vanneau_huppe"|espece=="pluvier_dore"|espece=="becassine_des_marais"))

#Retier les limicoles pour le suivi des remises (sauf vanneau huppe + pluvier doré + becassinne des marais)

anat <- Cotentin |> filter(suivi |> str_detect("remises"))

anat <- subset(anat, (order_tax=="anseriformes"|espece=="vanneau_huppe"|espece=="pluvier_dore"|espece=="becassine_des_marais"))

Cotentin <- rbind(anat, limic)

id <- paste0(Cotentin$site,Cotentin$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Cotentin$site, Cotentin$secteur, Cotentin$protocole, Cotentin$nb_annee_suivi, Cotentin$qualite_comptage, Cotentin$voie_migration, Cotentin$site_retenu)
site <- unique(site) 
table(duplicated(site$id))

site %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

site$Cotentin.qualite_comptage[duplicated(site$id == TRUE)] <- "douteux"
site <- unique(site)
# 3. Création de la table inventaire : 

inv <- data.frame(id,Cotentin$date,Cotentin$obs,Cotentin$mois,Cotentin$annee)
inv <- unique(inv)
table(duplicated(inv$id))

inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

#Réunir les noms d'observateurs pour un même site et une même date : 

inv$Cotentin.obs[inv$id=="dpm_nord2013-10-18"] <- "blond, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-11-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2015-07-31"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2015-09-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-07-07"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-08-02"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-09-16"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-09-30"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2016-11-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2017-01-16"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2017-02-14"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2017-08-22"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2017-09-22"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2018-04-05"] <- "elder, galloo, gabet"
inv$Cotentin.obs[inv$id=="dpm_nord2019-11-14"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2020-10-15"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_nord2021-01-18"] <- "elder, galloo, lesouef"
inv$Cotentin.obs[inv$id=="dpm_nord2022-09-14"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2014-08-29"] <- "blond, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2016-11-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2017-01-16"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2017-03-29"] <- "elder, galloo, gabet"
inv$Cotentin.obs[inv$id=="dpm_sud2017-08-22"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2019-01-07"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2019-07-02"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2021-06-24"] <- "fillol, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2022-06-15"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2022-08-30"] <- "suardi, galloo"
inv$Cotentin.obs[inv$id=="dpm_sud2022-09-30"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="fosse_du_gabion2015-04-08"] <- "elder, galloo, purenne"
inv$Cotentin.obs[inv$id=="fosse_du_gabion2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="l_ile_est2017-02-16"] <- "elder, galloo, fillol"
inv$Cotentin.obs[inv$id=="l_ile_est2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="la_dune_de_mer2018-01-09"] <- "elder, galloo, laurent"
inv$Cotentin.obs[inv$id=="la_dune_sud2006-02-06"] <- "caillot, elder, rnn domaine de beauguillot"
inv$Cotentin.obs[inv$id=="la_dune_sud2014-12-16"] <- "elder, jean-baptiste, galloo"
inv$Cotentin.obs[inv$id=="la_dune_sud2015-01-20"] <- "elder, galloo, purenne"
inv$Cotentin.obs[inv$id=="la_dune_sud2015-04-08"] <- "elder, galloo, purenne"
inv$Cotentin.obs[inv$id=="la_grande_piece_de_mer2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="le_gabion2016-11-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="le_gabion2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="le_grand_etang2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="le_milieu2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="les_grandes_iles2007-02-16"] <- "caillot, elder"
inv$Cotentin.obs[inv$id=="les_grandes_iles2008-04-25"] <- "bunel, caillot"
inv$Cotentin.obs[inv$id=="les_grandes_iles2015-03-13"] <- "elder, purenne, galloo"
inv$Cotentin.obs[inv$id=="les_grandes_iles2018-01-09"] <- "elder, galloo, laurent"
inv$Cotentin.obs[inv$id=="les_grandes_iles2021-12-22"] <- "elder, galloo, laurent, wodey"
inv$Cotentin.obs[inv$id=="partie_terrestre2010-09-26"] <- "elder, caillot"
inv$Cotentin.obs[inv$id=="partie_terrestre2013-02-20"] <- "vimard, blond"
inv$Cotentin.obs[inv$id=="partie_terrestre2017-12-12"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="partie_terrestre2017-12-19"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="partie_terrestre2018-01-18"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="partie_terrestre2018-02-19"] <- "elder, galloo, gabet"
inv$Cotentin.obs[inv$id=="partie_terrestre2020-12-17"] <- "elder, gabet, galloo, laurent"
inv$Cotentin.obs[inv$id=="partie_terrestre2021-01-15"] <- "elder, gabet, galloo, laurent"
inv$Cotentin.obs[inv$id=="partie_terrestre2022-12-22"] <- "elder, gabet, galloo, laurent"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2010-02-16"] <- "elder, davignon, galloo, caillot"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2010-09-08"] <- "davignon, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2011-04-26"] <- "blond, davignon"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2013-08-08"] <- "blond, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2013-11-26"] <- "blond, elder, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2014-01-16"] <- "blond, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2014-01-18"] <- "blond, elder"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2015-07-31"] <- "elder, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-02-23"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-03-23"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-03-24"] <- "gabet, menard"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-04-07"] <- "gabet, menard"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-04-21"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-08-18"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-09-06"] <- "gabet, menard"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-09-30"] <- "gabet, menard"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2016-10-20"] <- "gabet, menard"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2017-04-11"] <- "gabet, menard, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2017-05-09"] <- "gabet, menard, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2017-10-17"] <- "menard, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2017-12-12"] <- "gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2018-02-19"] <- "elder, gabet, galloo"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2018-08-28"] <- "galloo, laurent"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2019-11-13"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2020-04-23"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2020-07-22"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2022-01-04"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2022-02-23"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2022-09-14"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2022-11-10"] <- "galloo, gabet, suardi"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2022-12-22"] <- "galloo, gabet, laurent"
inv$Cotentin.obs[inv$id=="polder_ste_marie_cel2023-04-04"] <- "galloo, gabet"
inv$Cotentin.obs[inv$id=="dpm_nord2016-08-18"] <- "elder, galloo"

inv <- unique(inv)

# Compilation des deux tables : 
Cotentin_inv <- merge(site,inv,by.x = "id", by.y = "id")


# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Cotentin$id <- paste0(Cotentin$espece,Cotentin$site,Cotentin$date)
Cotentin$id_inv <- paste0(Cotentin$site,Cotentin$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Cotentin$id_inv)
sp <- unique(Cotentin$espece)


inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

Cotentin_obs <- data.frame(Cotentin$abondance, Cotentin$id)
Cotentin_obs <- aggregate(Cotentin_obs, Cotentin.abondance ~ Cotentin.id, median)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)

# Combinaison des deux tableaux : 

Cotentin_f <- merge(inventaire, Cotentin_obs, by.x = "id_sp", by.y = "Cotentin.id", all.x = T)

#remplacer les na par les 0 
Cotentin_f[is.na(Cotentin_f)] <- 0

# Var 2 -> espece :
colnames(Cotentin_f)[names(Cotentin_f)== "Var2"] <- "espece"
colnames(Cotentin_f)[names(Cotentin_f)== "Var1"] <- "id"

# Merge des tables : 
Cotentin_f <- merge(Cotentin_f, Cotentin_inv, by.x = "id", by.y = "id")

colnames(Cotentin_f) <- gsub("Cotentin.","",colnames(Cotentin_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Cotentin_f$abondance <- as.numeric(Cotentin_f$abondance)
setDT(Cotentin_f)
Cotentin_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Cotentin_f)
Cotentin_f <- subset(Cotentin_f, abondance_tot > 0)

write.csv2(Cotentin_f,"Data/Baie_Cotentin.csv")

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
Arcachon[,4] <- gsub("becasseau_maubeche_","becasseau_maubeche",Arcachon[,4])
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
Arcachon[,4] <- gsub("becasseau__minute","becasseau_minute",Arcachon[,4])

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


# Nom des observateurs : 
unique(Arcachon$obs)

Arcachon$obs[Arcachon$obs=="Julien Gernigon"] <- "gernigon"
Arcachon$obs[Arcachon$obs=="Philippe Nadé"] <- "nade"
Arcachon$obs[Arcachon$obs=="Damien Filloux"] <- "filloux"
Arcachon$obs[Arcachon$obs=="Bernard Laporte"] <- "laporte"
Arcachon$obs[Arcachon$obs=="Grégory Gomes"] <- "gomes"
Arcachon$obs[Arcachon$obs=="Gregory Gomes"] <- "gomes"
Arcachon$obs[Arcachon$obs=="Jean-Pierre Duval"] <- "duval"
Arcachon$obs[Arcachon$obs=="ROT"] <- "rot"
Arcachon$obs[Arcachon$obs=="Marie Lagarde"] <- "lagarde"
Arcachon$obs[Arcachon$obs=="Franck Jouandoudet"] <- "jouandoudet"
Arcachon$obs[Arcachon$obs=="Alain Fleury"] <- "fleury"
Arcachon$obs[Arcachon$obs=="Bernard Noel"] <- "noel"
Arcachon$obs[Arcachon$obs=="Christophe Le Noc"] <- "le noc"
Arcachon$obs[Arcachon$obs=="Bernard Capdeville"] <- "capdeville"
Arcachon$obs[Arcachon$obs=="Bernard Cappdeville"] <- "capdeville"
Arcachon$obs[Arcachon$obs=="Bernad Capdeville"] <- "capdeville"
Arcachon$obs[Arcachon$obs=="Bernard Caprdeville"] <- "capdeville"
Arcachon$obs[Arcachon$obs=="Jean-Jacques Boubert"] <- "boubert"
Arcachon$obs[Arcachon$obs=="Tom Perrin"] <- "perrin"
Arcachon$obs[Arcachon$obs=="Françoise Huchin"] <- "huchin"
Arcachon$obs[Arcachon$obs=="Julein Gernigon"] <- "gernigon"
Arcachon$obs[Arcachon$obs=="Péio Lambert"] <- "lambert"
Arcachon$obs[Arcachon$obs=="Peio Lambert"] <- "lambert"
Arcachon$obs[Arcachon$obs=="Jerôme Allou"] <- "allou"
Arcachon$obs[Arcachon$obs=="Jérôme Allou"] <- "allou"
Arcachon$obs[Arcachon$obs=="Tome Perrin"] <- "perrin"
Arcachon$obs[Arcachon$obs=="Jean Servant"] <- "servant"
Arcachon$obs[Arcachon$obs=="Matthias Grandpierre "] <- "grandpierre"
Arcachon$obs[Arcachon$obs=="Thierry Duprat"] <- "duprat"
Arcachon$obs[Arcachon$obs=="Nathalie Bos"] <- "bos"
Arcachon$obs[Arcachon$obs=="Christian Remy"] <- "remy"
Arcachon$obs[Arcachon$obs=="Christian Rémy"] <- "remy"
Arcachon$obs[Arcachon$obs=="Matthias Grandpierre"] <- "grandpierre"
Arcachon$obs[Arcachon$obs=="Roger Bounicaut"] <- "bounicaut"
Arcachon$obs[Arcachon$obs=="Romuald Chapelle"] <- "chapelle"
Arcachon$obs[Arcachon$obs=="Antoine Billay"] <- "billay"
Arcachon$obs[Arcachon$obs=="Julien Nezan"] <- "nezan"
Arcachon$obs[Arcachon$obs=="Dimitri Delorme"] <- "delorme"
Arcachon$obs[Arcachon$obs=="Thierry Duprat "] <- "duprat"
Arcachon$obs[Arcachon$obs=="Jean-Pierre Gans"] <- "gans"
Arcachon$obs[Arcachon$obs=="Péio  Lambert"] <- "lambert"
Arcachon$obs[Arcachon$obs=="Benjamin Viry"] <- "viry"
Arcachon$obs[Arcachon$obs=="Frédéric Revers"] <- "revers"
Arcachon$obs[Arcachon$obs=="Antoine Gergaud"] <- "gergaud"
Arcachon$obs[Arcachon$obs=="Richard Deneuvic"] <- "deneuvic"
Arcachon$obs[Arcachon$obs=="Yves Luciat Labry"] <- "luciat labry"
Arcachon$obs[Arcachon$obs=="Harold Dinclaux "] <- "dinclaux"
Arcachon$obs[Arcachon$obs=="Jeremy Dupuy"] <- "dupuy"
Arcachon$obs[Arcachon$obs=="Amandine Theillout, "] <- "theillout"
Arcachon$obs[Arcachon$obs=="Amandine Theillout"] <- "theillout"
Arcachon$obs[Arcachon$obs=="Caroline Péré"] <- "pere"
Arcachon$obs[Arcachon$obs=="Laurent Chevalier"] <- "chevalier"
Arcachon$obs[Arcachon$obs=="Magali Contrasty"] <- "contrasty"
Arcachon$obs[Arcachon$obs=="Alain Frélal"] <- "frelal"
Arcachon$obs[Arcachon$obs=="Yves Braconnier"] <- "braconnier"
Arcachon$obs[Arcachon$obs=="Alexandre Bert"] <- "bert"
Arcachon$obs[Arcachon$obs=="Claude Soubiran"] <- "soubiran"
Arcachon$obs[Arcachon$obs=="Clément Oncins"] <- "oncins"
Arcachon$obs[Arcachon$obs=="Hubert Huguenot"] <- "huguenot"
Arcachon$obs[Arcachon$obs=="S.Lafourcade "] <- "lafourcade"
Arcachon$obs[Arcachon$obs=="Laurent Couzi"] <- "couzi"
Arcachon$obs[Arcachon$obs=="Isabelle Thiberville"] <- "thiberville"
Arcachon$obs[Arcachon$obs=="Frédéric Perrier"] <- "perrier"
Arcachon$obs[Arcachon$obs=="Pierre Rigou"] <- "rigou"
Arcachon$obs[Arcachon$obs=="Nicolas Mokuenko"] <- "mokuenko"
Arcachon$obs[Arcachon$obs=="Mathieu Sannier"] <- "sannier"
Arcachon$obs[Arcachon$obs=="Chloé Mailhé"] <- "mailhe"
Arcachon$obs[Arcachon$obs=="Françoise Poinfer"] <- "poinfer"
Arcachon$obs[Arcachon$obs=="Alice Tribot "] <- "tribot"
Arcachon$obs[Arcachon$obs=="Sylvain Brun"] <- "brun"
Arcachon$obs[Arcachon$obs=="Eric Lenain"] <- "lenain"
Arcachon$obs[Arcachon$obs=="Michel Leconte"] <- "leconte"
Arcachon$obs[Arcachon$obs=="F.Christian"] <- "christian"
Arcachon$obs[Arcachon$obs=="Emilie de Blas"] <- "de blas"
Arcachon$obs[Arcachon$obs=="Caroline Péré "] <- "pere"
Arcachon$obs[Arcachon$obs=="Mathias Le Trouvé"] <- "le trouve"
Arcachon$obs[Arcachon$obs=="Jean Servan"] <- "servant"
Arcachon$obs[Arcachon$obs=="Adrien de Montaudoin"] <- "de montaudoin"
Arcachon$obs[Arcachon$obs=="Anaïs Lucas"] <- "lucas"
Arcachon$obs[Arcachon$obs=="Céline Girardeau"] <- "girardeau"
Arcachon$obs[Arcachon$obs=="Manon Cognyl"] <- "cognyl"
Arcachon$obs[Arcachon$obs=="Sébastien Labatut"] <- "labatut"
Arcachon$obs[Arcachon$obs=="Alexandre Loiseau"] <- "loiseau"
Arcachon$obs[Arcachon$obs=="Pierre Zimberlin"] <- "zimberlin"
Arcachon$obs[Arcachon$obs=="Simon Bauvineau"] <- "bauvineau"
Arcachon$obs[Arcachon$obs=="Claire Rougier"] <- "rougier"
Arcachon$obs[Arcachon$obs=="Adrien de Montaudouin"] <- "de montaudoin"
Arcachon$obs[Arcachon$obs=="Amandine Theillou"] <- "theillout"
Arcachon$obs[Arcachon$obs=="Mathieu Sanier"] <- "sanier"
Arcachon$obs[Arcachon$obs=="Sylvain BRUN"] <- "brun"
Arcachon$obs[Arcachon$obs=="Benoît Fritsch"] <- "fritsch"
Arcachon$obs[Arcachon$obs=="François Dindinaud "] <- "dindinaud"
Arcachon$obs[Arcachon$obs=="Bernard laporte"] <- "laporte"
Arcachon$obs[Arcachon$obs=="Céline Guevara"] <- "guevara"
Arcachon$obs[Arcachon$obs=="Christophe Troquereau"] <- "troquereau"
Arcachon$obs[Arcachon$obs=="Rot"] <- "rot"
Arcachon$obs[Arcachon$obs=="Fritsch Benoît"] <- "fritsch"
Arcachon$obs[Arcachon$obs=="Benoit Dumeau"] <- "dumeau"
Arcachon$obs[Arcachon$obs=="Julien Paradis"] <- "paradis"
Arcachon$obs[Arcachon$obs=="Lise Latry"] <- "latry"
Arcachon$obs[Arcachon$obs=="Benoit Fritsch"] <- "fritsch"
Arcachon$obs[Arcachon$obs=="Jean Jacques Boubert"] <- "boubert"
Arcachon$obs[Arcachon$obs=="Vaea Bujan"] <- "bujan"
Arcachon$obs[Arcachon$obs=="Robin Brouat"] <- "brouat"
Arcachon$obs[Arcachon$obs=="isabelle Thiberville"] <- "thiberville"
Arcachon$obs[Arcachon$obs=="jean pierre gans"] <- "gans"
Arcachon$obs[Arcachon$obs=="Françoise Pointfer"] <- "poinfer"

unique(Arcachon$Observateur.2)
Arcachon$Observateur.2[Arcachon$Observateur.2=="Julien Nezan"] <- "nezan"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Jean-Pierre Gans"] <- "gans"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Philippe Nadé"] <- "nade"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Jean-Jacques Boubert"] <- "boubert"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Julien Steinmetz"] <- "steinmetz"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Chritophe Le Noc"] <- "le noc"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Dimitri Delorme"] <- "delorme"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Matthias Grandpierre"] <- "grandpierre"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Fredérique Perrier"] <- "pierrier"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Yann Toutain"] <- "toutain"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Bernard Capdeville"] <- "capdeville"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Jérôme Allou"] <- "allou"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Roger Bounicaut"] <- "bounicaut"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Christian Rémy"] <- "remy"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Patrick Dutaut"] <- "dutaut"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Pascal Quadrio"] <- "quadrio"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Claude Soubiran"] <- "soubiran"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Françoise Poinfer"] <- "poinfer"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Michelle Juino"] <- "juino"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Bernard Capdeville"] <- "capdeville"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Olivier Pichon"] <- "pichon"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Clément Oncins"] <- "oncins"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Sylvain Brun"] <- "brun"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Jacques Dupas"] <- "dupas"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Françoise Poinfer"] <- "poinfer"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Angéline Amador"] <- "amador"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Eric Lenain"] <- "lenain"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Laurent Chevalier"] <- "chevalier"
Arcachon$Observateur.2[Arcachon$Observateur.2==" Alexandre Bert"] <- "bert"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Sylvain Brun"] <- "brun"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Michelle Juino"] <- "juino"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Mali Triger"] <- "triger"
Arcachon$Observateur.2[Arcachon$Observateur.2=="H.Dinclaux"] <- "dinclaux"
Arcachon$Observateur.2[Arcachon$Observateur.2=="F. Christian"] <- "christian"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Christophe Cazaux"] <- "cazaux"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Caroline Micallef"] <- "cazaux"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Alexandre Bert"] <- "bert"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Eric Lenain"] <- "lenain"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Yves Braconnier"] <- "braconnier"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Thierry Duprat"] <- "duprat"
Arcachon$Observateur.2[Arcachon$Observateur.2=="françoise Poinfer"] <- "poinfer"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Franck Jouandoudet"] <- "jouandoudet"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Adrien de Montaudoin"] <- "de montaudoin"
Arcachon$Observateur.2[Arcachon$Observateur.2=="M.Cagnato"] <- "cagnato"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Richard Deneuvic"] <- "deneuvic"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Cédric Cugny"] <- "cugny"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Adrien de Montaudouin"] <- "de montaudoin"
Arcachon$Observateur.2[Arcachon$Observateur.2=="M.Cagnato"] <- "cagnato"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Pierre Duteau"] <- "duteau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Emilie de Blas"] <- "de blas"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Bernard Laporte"] <- "laporte"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Grégory Boré"] <- "bore"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Céline Girardeau"] <- "girardeau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Cassandre Even"] <- "even"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Cédric Barbeyron"] <- "barbeyron"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Simon bauvineau"] <- "bauvineau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Claire Rougier"] <- "rougier"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Noé Metge"] <- "metge"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Yann Jacob"] <- "jacob"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Simon Bauvineau"] <- "bauvineau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Alexandre Loiseau"] <- "loiseau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Bernad Capdeville"] <- "capdeville"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Michel Juino"] <- "juino"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Manon Cognyl"] <- "cognyl"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Philippe  Birac"] <- "birac"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Sébastien Delahodde"] <- "delahodde"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Isabelle Thiberville"] <- "thiberville"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Anaïs Lucas"] <- "lucas"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Céline Guévara"] <- "guevara"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Leslie Stout"] <- "stout"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Benoît Fritsch"] <- "fritsch"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Ninon Durand"] <- "durand"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Thomas Boudou"] <- "boudou"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Océane Poursat"] <- "poursat"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Julian Lazard"] <- "lazard"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Céline Giradeau"] <- "girardeau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Darieus Helou"] <- "helou"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Françoise Pointfer"] <- "poinfer"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Deneuvic Richard"] <- "deneuvic"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Matthias Grandpierre "] <- "grandpierre"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Benoit Fritsch"] <- "fritsch"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Philippe Birac"] <- "birac"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Lisa Primi"] <- "primi"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Florian Lalièvre"] <- "lalièvre"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Marion Célik"] <- "celik"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Françoise Dindinaud"] <- "dindinaud"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Thierry Duprat "] <- "duprat"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Céline Guevara"] <- "guevara"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Françoise poinfer"] <- "poinfer"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Robien Bruant"] <- "bruant"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Robin Bruant"] <- "bruant"
Arcachon$Observateur.2[Arcachon$Observateur.2=="François Dindinaud"] <- "dindinaud"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Vaea Bujan"] <- "bujan"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Jeremy Behamou"] <- "behamou"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Florian Papon"] <- "papon"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Justine Hazera"] <- "hazera"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Benoit Dumeau"] <- "dumeau"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Sarah Dias"] <- "dias"
Arcachon$Observateur.2[Arcachon$Observateur.2=="thierry duprat"] <- "duprat"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Nicolas Mokuenko"] <- "mokuenko"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Pierre Dutaut"] <- "dutaut"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Olivier Vidal"] <- "vidal"
Arcachon$Observateur.2[Arcachon$Observateur.2=="Philippe  Birac"] <- "birac"

sort(unique(Arcachon$Observateur.2))

unique(Arcachon$Observateur.3)
Arcachon$Observateur.3[Arcachon$Observateur.3=="Daniel Baqué"] <- "baque"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Alexandre Bert"] <- "bert"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Thierry Duprat"] <- "duprat"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Marie Cazeneuve"] <- "cazeneuve"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Jean-Jacques Boubert"] <- "boubert"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Michelle Juino"] <- "juino"
Arcachon$Observateur.3[Arcachon$Observateur.3==" Clément Oncins"] <- "oncins"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Olivier Pichon"] <- "pichon"
Arcachon$Observateur.3[Arcachon$Observateur.3==" Laurent Chevalier"] <- "chevalier"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Alain Frelal"] <- "frelal"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Lucie Fuentes"] <- "fuentes"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Yves Braconnier"] <- "braconnier"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Philippe Birac"] <- "birac"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Chloé Maihle"] <- "maihle"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Alice Tribot"] <- "tribot"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Benoît Lujan"] <- "lujan"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Emilie de Blas"] <- "de blas"
Arcachon$Observateur.3[Arcachon$Observateur.3=="P.Favali"] <- "favali"
Arcachon$Observateur.3[Arcachon$Observateur.3=="A. Bat"] <- "bat"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Françoise Juino"] <- "f_juino"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Mathias Le Trouvé"] <- "le trouve"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Gregory Bore"] <- "bore"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Pascal Zeddam"] <- "zeddam"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Sandra De Mélo"] <- "de melo"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Cédric Barbeyron"] <- "barbeyron"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Bastien Campistron"] <- "campistron"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Florine Iaïch"] <- "iaich"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Romuald Gaubert"] <- "gaubert"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Sylvain Brun"] <- "brun"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Brun Sylvain"] <- "brun"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Michèle Juino"] <- "juino"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Olivia Tassin"] <- "tassin"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Yann Jacob"] <- "jacob"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Alexandre Loiseau"] <- "loiseau"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Clément Oncins"] <- "oncins"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Claire Rougier"] <- "rougier"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Matthias Grandpierre"] <- "grandpierre"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Cassandre Even"] <- "even"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Anais Lucas"] <- "lucas"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Manon Cognyl"] <- "cognyl"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Hubert Huguenot"] <- "huguenot"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Claude Feigné"] <- "feigne"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Anaïs Lucas"] <- "lucas"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Célia Maillotte"] <- "maillotte"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Sébastien Delahodde"] <- "delahodde"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Jean-Luc Castet"] <- "castet"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Bernard Laporte"] <- "laporte"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Laurie Landier"] <- "landier"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Manuel Donadieu"] <- "donadieu"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Thomas Boudou"] <- "boudou"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Mathilde Coursimault"] <- "coursimault"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Hubert Huguenau"] <- "huguenau"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Julian"] <- "julian"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Florian Lalièvre"] <- "lalievre"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Robon Brouat"] <- "brouat"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Marion Célik"] <- "celik"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Thomas Boubou"] <- "boubou"
Arcachon$Observateur.3[Arcachon$Observateur.3=="benoit Dumeau"] <- "dumeau"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Adrien de Montaudouin"] <- "de montaudoin"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Isabelle Thiberville"] <- "thiberville"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Luna Marmeuse"] <- "marmeuse"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Joris Grenon"] <- "grenon"
Arcachon$Observateur.3[Arcachon$Observateur.3=="Inge "] <- "van halder"
sort(unique(Arcachon$Observateur.3))

Arcachon[,14] <- gsub("Départ de 1 à 9:10","",Arcachon[,14])
Arcachon[,14] <- gsub("Départ de 14 à 8:47","",Arcachon[,14])
Arcachon[,14] <- gsub("Départ de 23 à 9:01","",Arcachon[,14])
Arcachon[,14] <- gsub("Départ de 28 à 8:45 vers océan","",Arcachon[,14])

unique(Arcachon$Observateur.4)
Arcachon$Observateur.4[Arcachon$Observateur.4=="F. Dissart"] <- "dissart"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Emilie de Blas"] <- "de blas"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Mathias Le Trouvé"] <- "le trouve"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Céline Girardeau"] <- "girardeau"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Cassandre Even"] <- "even"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Alexandre Bert"] <- "bert"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Christian David"] <- "david"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Yves Braconnier"] <- "braconnier"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Alexandre Loiseau"] <- "loiseau"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Claire Rougier"] <- "rougier"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Franck Jouandoudet"] <- "jouandoudet"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Sylvie Ducasse"] <- "ducasse"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Jean-Luc Castet"] <- "castet"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Céline Guévara"] <- "guevara"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Jean-Jacques Boubert"] <- "boubert"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Thierry Duprat"] <- "duprat"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Pauline Destainville"] <- "destainville"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Benoit Dumeau"] <- "dumeau"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Robien Brouat"] <- "brouat"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Charles Coup"] <- "coup"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Florian Royer"] <- "royer"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Jean Jacques Boubert"] <- "boubert"
Arcachon$Observateur.4[Arcachon$Observateur.4=="Inge Van Halder"] <- "van halder"

Arcachon[,15] <- gsub("Olivier","",Arcachon[,15])
Arcachon[,15] <- gsub("Théo","",Arcachon[,15])
Arcachon[,15] <- gsub("2 départ à 15h47 en direction de l'ile","",Arcachon[,15])
Arcachon[,15] <- gsub("Olivier, Julie","",Arcachon[,15])
Arcachon[,15] <- gsub("Mathis","",Arcachon[,15])
Arcachon[,15] <- gsub(", Julie","",Arcachon[,15])

sort(unique(Arcachon$Observateur.4))

unique(Arcachon$Observateur.5)
Arcachon[,16] <- gsub("F. Paquin","paquin",Arcachon[,16])
Arcachon[,16] <- gsub("Alexandre Bert","bert",Arcachon[,16])
Arcachon[,16] <- gsub("Yannick Beta-Maisonnave","beta maisonnave",Arcachon[,16])
Arcachon[,16] <- gsub("Morgane Le Billan","le billan",Arcachon[,16])
Arcachon[,16] <- gsub("71231","",Arcachon[,16])
Arcachon[,16] <- gsub("1600 la veille","",Arcachon[,16])
Arcachon[,16] <- gsub("Attaque de pèlerin sur Grand Banc","",Arcachon[,16])
Arcachon[,16] <- gsub("Départ de 683 entre 7:15 et 7:29","",Arcachon[,16])
Arcachon[,16] <- gsub("Départ 850 à 8h50  vers IaO","",Arcachon[,16])
Arcachon[,16] <- gsub("Attaque de pèlerin sur Grand Banc","",Arcachon[,16])
Arcachon[,16] <- gsub("Présence d'un Faucon Pelerin","",Arcachon[,16])
sort(unique(Arcachon$Observateur.5))

unique(Arcachon$Observateur.6)
Arcachon$Observateur.6[Arcachon$Observateur.6=="43 partent vers l'océan à 15h + 5 vers l'ile à 14h40"] <- ""
Arcachon[,17] <- gsub("17 départ vers l'ile à 14h55","",Arcachon[,17])
Arcachon[,17] <- gsub("120 partent vers l'ile à 14h40","",Arcachon[,17])
Arcachon[,17] <- gsub("3 partent vers l'ile à 6h31","",Arcachon[,17])
Arcachon[,17] <- gsub("127 partent entre 15:00 et 15:10 ","",Arcachon[,17])
Arcachon[,17] <- gsub("Mathys Salaud","salaud",Arcachon[,17])

#Arcachon$obs <- paste(Arcachon$obs,Arcachon$Observateur.2,Arcachon$Observateur.3,Arcachon$Observateur.4,Arcachon$Observateur.5,Arcachon$Observateur.6, sep = ", ", recycle0 = T)
help("paste")

# Vérification des abondances : 
unique(Arcachon$abondance)
unique(Arcachon[c(995:1363),9])
#Case vide + Partiel + 0 + NC 

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

Arcachon$protocole <- "terrestre ?"

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

colnames(Arcachon)[24] <- "nb_annee_suivie"

#Colonne site retenu: 
Arcachon$site_retenu <- with(Arcachon, ifelse(Arcachon$nb_annee_suivie < 3, "non", "oui"))

# Voie migration 
Arcachon$voie_migration <- "est_atlantique"

#Bassin d'arcachon : 

id <- paste0(Arcachon$site,Arcachon$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Arcachon$site, Arcachon$secteur, Arcachon$protocole, Arcachon$nb_annee_suivie, Arcachon$qualite_comptage, Arcachon$voie_migration, Arcachon$site_retenu)
site <- unique(site) 
table(duplicated(site$id))
site %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

# Dérangement appliqué que à  la première ligne donc création qualité comptage "ok" et "douteux" alors que c'est que "douteux"
site$Arcachon.qualite_comptage[site$id=="andernos2017-11-15"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="andernos2020-12-14"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="ile_aux_oiseaux2023-12-11"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="ile_aux_oiseaux2023-04-17"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2023-05-17"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2020-02-07"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2023-09-27"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2017-11-15"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2020-10-14"] <- "douteux"
site$Arcachon.qualite_comptage[site$id=="rnnpsa2018-07-18"] <- "douteux"

site <- unique(site)

# 3. Création de la table inventaire : 

inv <- data.frame(id,Arcachon$date,Arcachon$obs,Arcachon$mois,Arcachon$annee)
inv <- unique(inv)
table(duplicated(inv$id))

inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

# réunir les observateurs pour un même secteur de comptage : 
inv$Arcachon.obs[inv$id=="andernos2017-01-09"] <- "mokuenko, viry"
inv$Arcachon.obs[inv$id=="andernos2017-01-09"] <- "mokuenko, viry"
inv$Arcachon.obs[inv$id=="andernos2015-10-09"] <- ""
inv$Arcachon.obs[inv$id=="andernos2019-01-18"] <- "jouandoudet"
inv$Arcachon.obs[inv$id=="andernos2020-06-09"] <- "jouandoudet, de blas"
inv$Arcachon.obs[inv$id=="arcachon2016-01-08"] <- "revers, gergaud"
inv$Arcachon.obs[inv$id=="arcachon2018-01-15"] <- "PNRLG, de montaudoin"
inv$Arcachon.obs[inv$id=="certes2010-08-16"] <- "fleury, nade"
inv$Arcachon.obs[inv$id=="certes2011-01-17"] <- "fleury, nade"
inv$Arcachon.obs[inv$id=="certes2016-01-08"] <- "luciat labry, nade, dinclaux"
inv$Arcachon.obs[inv$id=="certes2017-01-09"] <- "lafourcade, nade, mokuenko"
inv$Arcachon.obs[inv$id=="certes2018-01-15"] <- "christian, revers"
inv$Arcachon.obs[inv$id=="delta_de_l_eyre2009-12-14"] <- "fleury, lambert"
inv$Arcachon.obs[inv$id=="delta_de_l_eyre2016-01-08"] <- "PNRLG, pere"
inv$Arcachon.obs[inv$id=="delta_de_l_eyre2017-01-09"] <- "rigou, pere, perrier"
inv$Arcachon.obs[inv$id=="grand_banc2011-12-21"] <- "duprat, grandpierre"
inv$Arcachon.obs[inv$id=="grand_banc2020-08-24"] <- "grandpierre, boubert"
inv$Arcachon.obs[inv$id=="gujan_mestras2009-12-14"] <- "laporte"
inv$Arcachon.obs[inv$id=="gujan_mestras2017-06-15"] <- "laporte, poinfer"
inv$Arcachon.obs[inv$id=="ile_aux_oiseaux2019-04-16"] <- "boubert, grandpierre"
inv$Arcachon.obs[inv$id=="ile_aux_oiseaux2019-05-16"] <- "deneuvic, capdeville"
inv$Arcachon.obs[inv$id=="la_hume2009-05-08"] <- "perrin, laporte"
inv$Arcachon.obs[inv$id=="la_hume2016-01-08"] <- "gergaud, laporte"
inv$Arcachon.obs[inv$id=="la_hume2016-03-21"] <- "gergaud, laporte"
inv$Arcachon.obs[inv$id=="la_hume2016-10-13"] <- "gergaud, laporte"
inv$Arcachon.obs[inv$id=="la_hume2017-01-09"] <- "couzi, laporte"
inv$Arcachon.obs[inv$id=="la_hume2017-06-15"] <- "poinfer, laporte"
inv$Arcachon.obs[inv$id=="la_hume2018-05-28"] <- "de montaudoin, laporte"
inv$Arcachon.obs[inv$id=="la_hume2019-12-10"] <- "de montaudoin, laporte"
inv$Arcachon.obs[inv$id=="la_hume2020-01-08"] <- "thiberville, laporte"
inv$Arcachon.obs[inv$id=="la_hume2022-09-15"] <- "poinfer, de montaudoin"
inv$Arcachon.obs[inv$id=="la_hume2022-11-07"] <- "poinfer, de montaudoin"
inv$Arcachon.obs[inv$id=="la_hume2023-09-27"] <- "poinfer, latry"
inv$Arcachon.obs[inv$id=="mimbeau2010-04-14"] <- "lagarde, billay"
inv$Arcachon.obs[inv$id=="plage_oceane_la_teste2017-01-09"] <- "revers, sannier"
inv$Arcachon.obs[inv$id=="plages_oceanes_du_ferret2011-04-15"] <- "gans"
inv$Arcachon.obs[inv$id=="rnnba2012-12-11"] <- "grandpierre"
inv$Arcachon.obs[inv$id=="rnnba2019-07-19"] <- "rougier, deneuvic"
inv$Arcachon.obs[inv$id=="rnnba2020-11-12"] <- "grandpierre, deneuvic"
inv$Arcachon.obs[inv$id=="ro_du_teich2023-05-17"] <- "rot"

inv <- unique(inv)

# Compilation des deux tables : 
Arcachon_inv <- merge(site,inv,by.x = "id", by.y = "id")


# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Arcachon$id <- paste0(Arcachon$espece,Arcachon$site,Arcachon$date)
Arcachon$id_inv <- paste0(Arcachon$site,Arcachon$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Arcachon$id_inv)
sp <- unique(Arcachon$espece)

inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)

Arcachon_obs <- data.frame(Arcachon$abondance, Arcachon$id)
Arcachon_obs <- aggregate(Arcachon_obs, Arcachon.abondance ~ Arcachon.id, median)

# Combinaison des deux tableaux : 

Arcachon_f <- merge(inventaire, Arcachon_obs, by.x = "id_sp", by.y = "Arcachon.id", all.x = T)

#remplacer les na par les 0 
Arcachon_f[is.na(Arcachon_f)] <- 0

# Var 2 -> espece :
colnames(Arcachon_f)[names(Arcachon_f)== "Var2"] <- "espece"
colnames(Arcachon_f)[names(Arcachon_f)== "Var1"] <- "id"

# Merge des tables : 
Arcachon_f <- merge(Arcachon_f, Arcachon_inv, by.x = "id", by.y = "id")

colnames(Arcachon_f) <- gsub("Arcachon.","",colnames(Arcachon_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Arcachon_f$abondance <- as.numeric(Arcachon_f$abondance)
setDT(Arcachon_f)
Arcachon_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Arcachon_f)
Arcachon_f <- subset(Arcachon_f, abondance_tot > 0)

write.csv2(Arcachon_f,"Data/Bassin_arcachon_limicoles.csv")


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
Rhin$espece[Rhin$espece=="canard_siffleur_du_chili"] <- "canard_de_chiloe"
Rhin$espece[Rhin$espece=="eismature_rousse"] <- "erismature_rousse"
Rhin$espece[Rhin$espece=="fuligule_indetermine"] <- "fuligule_sp"

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
Rhin$obs[Rhin$commentaire_de_la_liste=="Frederic Deck, Marc Brignon, Daniel Kirmser"] <- "deck, brignon, kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel"] <- "buchel"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, A. Chabrolle, JP. Hiss, N. Hoffmann, JP. Risse + J. Rupp, B. Disch"] <-  "bronner, chabrolle, hiss, hoffmann, risse, rupp, disch"
Rhin$obs[Rhin$commentaire_de_la_liste=="Sébastien Didier, Raynald Moratin, Daniel Kirmser"] <- "didier, moratin, kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Jean-Pierre Hiss, Jean-Marie Risse, Jürgen Rupp"] <- "bronner, hiss, risse, rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel, Pascale David"] <- "buchel, david"
Rhin$obs[Rhin$commentaire_de_la_liste=="Alexandre Goncalvès, Christophe Rahier, Daniel Kirmser"] <- "goncalvès, rahier, kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Marion Bailleul, Eric Brunissen, Alexanfre Goncalves, Stéphane Uhmang, Daniel Kirmser"] <- "bailleul, brunissen, goncalves, uhmang, kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Risse, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand "] <- "bronner, hoffmann, riss, Hiss, Rupp, Disch, Wiegand"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Risse, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand, Jean-Luc Wilhelm (ONCFS), Françoise Sieffert"] <- "Bronner, Hoffmann, Riss, Hiss, Rupp, Disch, Wiegand, Wilhelm, Sieffert"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel, Sandrine Kech"] <- "Buchel, Kech"
Rhin$obs[Rhin$commentaire_de_la_liste=="Antoine Aït-Saîdi, Eric Brunissen, Stéphane Uhmang, Daniel Kirmser, Frédéric Deck"] <- "Aït-Saîdi, Brunissen, Uhmang, Kirmser, Deck"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner, Nicolas Hoffmann, Jean-Marie Riss, Jean-Pierre Hiss, Jürgen Rupp, Bernhardt Disch, Jochem Wiegand"] <- "Bronner, Hoffmann, Riss, Hiss, Rupp, Disch, Wiegand"
Rhin$obs[Rhin$commentaire_de_la_liste=="Marion Bailleul, Eric Brunissen, Frédéric Deck, Daniel Kirmser, Christophe Rahier, Julie Roux, Stéphane Umhang"] <- "Bailleul, Brunissen, Deck, Kirmser, Rahier, Roux, Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, JM Risse, JP Hiss, Nicolas Hoffmann + compteurs allemands (Jürgen Rupp, Bernhard Disch, Jochem Wiegand) + 2 agents ONCFS (Dominique Cronimus, Jean-Dominique Veaux)"] <- "Bronner, Risse, Hiss, Hoffmann, Rupp, Disch, Wiegand, Cronimus, Veaux"
Rhin$obs[Rhin$commentaire_de_la_liste=="Laurent Waeffler, Sébastien Glas, Sandrine Kech"] <- "Waeffler, Glas, Kech"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser, Eric Brunissen, Christophe Rahier"] <- "Kirmser, Brunissen, Rahier"
Rhin$obs[Rhin$commentaire_de_la_liste=="JM Bronner, JP Hiss, Nicolas Hoffmann + compteurs allemands (Jürgen Rupp, Bernhard Disch, F. Rau, L. Leib et JY Follet) + Gilles Nonnenmacher et Vivien Siat (ONCFS)"] <- "Bronner, Hiss, Hoffmann, Rupp, Disch, Rau, Leib, Follet, Nonnenmacher, Siat"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\nde l'entrée du plan d'eau au pK 280.5"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\ndu pK 280.5 au 178.5"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°\nDu pK179.5 à la ligne de bouées sud"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier/JL Wilhelm/2 longues-vues/ Vent 15-40 km/h SO/1020hpa/couvert/11°"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Vieux-Rhin à Gerstheim : entre ouvrage de répartition et seuil 1\nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Vieux-Rhin à Gerstheim : entre seuil 1 et seuil 2\nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Vieux-Rhin à Gerstheim : entre seuil 2 et confluence\nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal aval à Gerstheim \nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "contre-canal à Gerstheim\nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal amont à Gerstheim \nE. Hornier et JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "canal amont écluse et centrale + musoir\nE. Hornier JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "canal amont vannes de décharge + polder VNF\nE.Hornier JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Aval barrage Gambsheim\nE.Hornier JL Wilhelm"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "E. Hornier Jean-Luc Wilhelm\nBeau temps - vent nul 1030 hpa\n8-15° Niveau plan d'eau très bas"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre Barrage agricole et seuil 1, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre seuil 1 et seuil 2, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Entre seuil 2 et confluence, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal de fuite Aval barrage hydro et écluse , Hornier, bonnes conditions d'observation, 29°, léger vent, beau"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Canal amenée amont barrage hydro et écluse, bonnes conditions d'observation, 29°, léger vent, beau (Erwan Hornier)"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste== "Contre-canal à hauteur de l'île de Gerstheim, Hornier, bonnes conditions d'observation, 29°, léger vent, beau"] <- "Hornier"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
Rhin$obs[Rhin$commentaire_de_la_liste=="comptage du musoir amont de l'usine hydroélectrique de Gerstheim et berges voisines, E. Hornier, bonne visibilité, temps orageux, pluie"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.Hornier, JL Wilhelm, comptage exhaustif amont barrage, berges+ plans d'eau+ musoirs, temps variable, vent moyen, temp=20°environ, bonnes conditions d'observation"]<- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, observation à partir de la pointe du terre plein du barrage du canal de décharge, beau temps, 29°, bonne visibilité, comptage jusqu'à l'anse de la Thumenau avec longue vue swaro zoom 20-60"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Estève - Beau temps/nuageux, vent faible, bonne visibilité, temp 21°C."]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Estève - Beau temps/Nuageux, vent faible, bonne visibilité, temp 21°C."]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Beau temps/Nuageux - Vent faible - Bonne visibilité - Temp 21°C - Courant fort"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier/Valerie Esteve Bonne condition de visibilité nuageux pluie vent moyen t°17°"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Soleil/Ciel bleu - Bonne visibilité - 18°C"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan hornier, Valérie Esteve - Pas de vent - Brouillard léger - Visibilité moyenne - Temp 10°C"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Bonne visibilité - Pas de vent - Temp 15 °C - Présence de grande sauterelle chanteuses"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Bonne visibilité - Pas de vent - Temp 21 °C"]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve - Pas de vent / Ensoleillé - Bonne visibilité - Temp 20 °C - Qualité du comptage : 2/5 car présence de travaux sur le musoir - Au point d'observation : Rhin PK 309 (amont du barrage) : En 10/15 min l'eau est descendue et on a pu observer le banc de sable à gauche puis l'eau est ensuite remontée."]<- "Hornier, Estève"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc Wilhelm - Pas de vent / Brouillard - Mauvaise visibilité - Temp 1°C - Qualité du comptage : 2/5"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Un peu de vent - Bonne visibilité - Temp 1°C"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc WILHELM - Bonne visibilité - Pas de vent - Temp 2°C - Qualité du comptage : 3/5"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie ESTEVE, Jean-Luc WILHELM - Pas de vent - Bonne visibilité - Temp 3°C - Qualité du comptage : 4/5 \n(Travaux sur le pont)"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp 2°C - Pluie/Gris - Bonne visibilité - Qualité de comptage : 4/5"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp. 3°C - pas de pluie/ gris - bonne visibilité - Qualité de comptage : 5/5"]<- "Estève, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temp. 3°C - gris - bonne visibilité - qualité du comptage: 5/5"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve , Jean-Luc Wilhelm- Temp. 3°C - Bonne visibilité - qualité de comptage : 5/5"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Visibilité moyenne - Temps couvert - Vent moyen - Débit Rhin : 1740 m^3"] <- "Hornier, Estève, Wilhelm, Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Bonne visibilité - Temps couvert - Qualité du comptage : 4,5/5"] <- "Hornier, Estève, Wilhelm, Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Visibilité moyenne à bonne - Vent - Temps couvert - Qualité du comptage : 3,5/5 (le comptage n'est pas mauvais mais nous avons du changer de rive pour pouvoir compter les oiseaux du côté de la digue tiroir)"] <- "Hornier, Estève, Wilhelm, Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm, Gérard Hog - Temp 3°C - Bonne Visibilité - Vent - Temps couvert - Qualité du comptage : 4/5 (à partir de la base nautique puis 1h après de la digue tiroir) - Présence de 5 barques de pêche : dérangement observé faible"] <- "Hornier, Estève, Wilhelm, Hog" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4°C - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - Vent moyen - Temps variable - Qualité du comptage : 4,5/5"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - temp. 6 °C - pluie - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - vent moyen - qualité du comptage : 5/5"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4 °C - débit du Rhin 2000 m^3 (Vieux Rhin en cru) - Temps variable - Qualité du comptage : 5/5"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temp. 4 °C puis 3°C  - Débit du Rhin 2000 m^3 (Vieux Rhin en cru)- Forte pluie + grêle - Qualité du comptage : 4/5 (présence de 6 chiens sans laisse qui ont fait fuir les oiseaux)"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps couvert, 5°, vent 15km/h, SO, 1037hpa, Q Rhin Kelh : 1240 m3/s, comptage 4.5/5, quelques oiseaux masqués par une digue."] <- "Hornier, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps couvert, 5°C, vent 15km/h SO, , 1037hpa, Q Rhin Kelh : 1240m3/s, qualité comptage : 4/5"] <- "Hornier, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; "] <- "Buchel" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe : Eric Brunissen, Justine Delcambre et Stéphane Umhang."] <- "Kirmser, Brunissen, Delcambre, Umhang." 
Rhin$obs[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; Valérie Esteve, Jean-Luc Wilhelm - Température : 0°C"] <- "Estève, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Comptage non possible lors de la date officielle du dimanche 14/1/18 en raison du brouillard. Reporté au lendemain lundi 15/1/18. Participants : Pour la FOSOR : Bernhard DISCH; pour la LPO-Alsace : Jean-Marc BRONNER + Yann CARASCO; pour l'ONCFS : Erwan HORNIER. Vent faible à modéré en matinée dans la partie nord du plan d'eau (un peu gênant par moments), devenant faible l'après-midi dans la partie sud."] <- "Bronner, Disch, Carasco, Hornier" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 1°C - Bonne visibilité - Vent faible - Qualité du comptage : 5/5"] <- "Esteve, Wilhelm" 
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 1°C - Bonne visibilité - Qualité du comptage : 5/5"] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 3°C - Bonne visibilité - Qualité du comptage : 5/5"] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps ensoleillé - Temp : 6°C - Visibilité moyenne (reflet du soleil) - Qualité du comptage : 4/5"] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps gris / pluie - Temp : °C - Qualité du comptage : 5/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps gris / pluvieux - Temp : 0°C - Qualité du comptage : 5/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - 1116 hPa - Temp : 0°C - Qualité du comptage : 3/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Temp : 0°C - Mauvaise visibilité avec brume de chaleur, obligeant à compter de chaque côté. Qualité du comptage : 4/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temp : Gris/ Vent à 15/20 km.h -Mauvaise visibilité à cause de brume de chaleur (comptage des deux côtés) - Qualité du comptage : 4/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, Valérie Esteve, Jean-Luc Wilhelm - Temps : Gris/ Vent à 15 km/h - Mauvaise visibilité à cause de brume de chaleur (donc comptage des deux côtés) - Temp : 1°C - Qualité du comptage : 4/5"] <- "Hornier, Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Esteve, Jean-Luc Wilhelm - Temps : Nuageux / un peu de vent - Temp : 9°C - Qualité du comptage : 5/5"] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temps : Nuageux / Un peu de vent - Temp : 10°C - Qualité du comptage : 5/5"] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Valérie Estève, Jean-Luc Wilhelm - Temps : Nuageux / Un peu de vent - Temp : 10°C - Qualité du comptage : 3/5 (Présence d'une péniche qui a perturbé tous les oiseaux. Il se sont posés du côté de la digue au lieu du musoir donc à chaque fois que l'on avançait avec la voiture, ils s'envolaient pour se reposer plus loin.) Dans ce formulaire, on intègre les observations du contre canal."] <- "Esteve, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 4/5, changement de berge 20 mn (nord à sud)"] <- "Hornier, Wilhelm, Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=7°, couvert, 1016hpa, , vent 15à 20km/s du s, visibilité assez mauvaise à longue distance, note de comptage 3/5,"] <- "Hornier, Wilhelm, Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T LE sergent, t°=9°, vent faible, 1016 hpa, couvert avec eclaircies, note comptage 5/5, de la digue sud est"] <- "Hornier, Wilhelm, Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, T. Le Sergent, t°=9°, vent faible, couvert avec éclairicies, 1016 hpa, note 5/5, de digue Sud est"] <- "Hornier, Wilhelm, Le Sergent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 1005 hpa, vent moyen 10km/h, 21°, niveau de l'eau assez bas, vasières découvertes, comptage 4/5"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 1005 hpa, 21°, vent moyen 10km/h, 4/5, débit important, pas de haut fonds découverts"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 4/5, beau temps, 22-25°, vent faible, canal + écluse + contre-canal"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E-Hornier, 3/5 (embouteillage de péniche à l'entrée des écluses + mouvement d'oiseaux), beau temps légèrement nuageux, 22 à 26°, vent faible, canal + côté allemand."] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E-Hornier Beau temps - 22 à 26°-Vent nul- 5/5- péniches arrêtées à proximité des musoirs amont et aval"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps, 11° à 18 °, vent nul, conditions d'observation 5/5"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, léger vent, couvert, comptage 5/5"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, vent nul, couvert, pluie fine, 12°, comptage 5/5"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 13°, vent moyen, visibilité bonne , comptage 5/5"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 13°, vent faible, visibilité bonne, 5/5, y compris contre-canal"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage des digues côté UNAP"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5(1 personne, groupes denses, visibilité limite le long de la digue tiroir. comptage à partir de la digue côté UNAP + comprend aussi les effectifs du secteur étranglement"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert à 100%, pluie fine/bruine, 6 ° vent faible à nul, visibilité assez bonne, comptage 3/5 (1 personne, groupes denses, visibilité limite le long de la digue tiroir. Comptage à partir de la banquette ouest et du barrage du canal de décharge"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Secteur Sud du plan d'eau (Pointe sud+ Thumenau+étranglement)à partir de la rive ouest, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 3/5 (nombreux mouvements d'oiseaux)"] <- "Hornier, Wilhelm, Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="à partir du kiosque et des berges est, E. Hornier/JL Wilhelm/Gerard Hog, -1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage 2/5 (beaucoup d'oiseaux, nombreux mouvements, visibilité moyenne)."] <- "Hornier, Wilhelm, Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="Secteur nord des bouée à partir de la rive ouest et de la digue tiroir), E. Hornier/JL Wilhelm/Gerard Hog, 1°, temps couvert, vent 5-10 km/h, 1019 hpa. Comptage difficile 2/5 (nombreux mouvements d'oiseaux)"] <- "Hornier, Wilhelm, Hog"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, temps couvert, 4 °, vent nul, 1018 hpa, comptage 4/5 (dérangement barque de pêcheur au seuil 2)"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 5 °, temps couvert, pluie, 1018 hpa, comptage 5/5 (+contre-canal)"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, pluie, vent fort, houle sur le Rhin, 1012hpa, comptage à partir des digues, du barrage et un point en digue allemande en raison de la mauvaise visibilité. Comptage 3/5 (visibilité moyenne et vent fort)"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 7°, vent fort, pluie, comptage 3/5 (mouvement d'oiseaux et visibilité)"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; "] <- "Kirmser"
Rhin$obs[Rhin$commentaire_de_la_liste=="RNCFS du Rhin ; E. Hornier, dérangement par embarcation de pêcheurs sous le seuil 1 entraînant un déplacement assez important de l'effectif habituel"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Mauvaises conditions météo en matinée pour le comptage de la partie Nord, avec de la pluie continue et du vent, et donc des conditions d'observation médiocres (condensation sur les optiques, vagues...). Meilleures conditions l'après-midi pour la partie sud, avec un temps plus calme, sans pluie et avec du vent devenu faible.\n4 participants de la LPO Alsace (Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse) + 3 participants allemands du FOSOR (Finn Brunssen, John Ryding, Jürgen Rupp)."] <- "Bronner, Carasco, Hiss, Risse, Brunssen, Ryding, Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Remplacement de l'observateur titulaire (Eric Buchel), non disponible pour le comptage Wetlands 2019."] <- "Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 10°, 1037 hpa, vent léger, très bonne visibilité, comptage 3/5 (gros envol d'un groupe de F milouin, morillon et C chipeau), comptage à partir de la digue tiroir."] <- "Hornier, Wilhelm, Coq, Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 5°, 1037 hpa, vent léger, très bonne visibilité, comptage 4/5 (bonne visibilité mais groupes denses), comptage à partir de la digue tiroir."] <- "Hornier, Wilhelm, Coq, Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M Esnard, 11°, 1037 hpa, vent léger, très bonne visibilité, comptage 5/5, à partir de la digue SE"] <- "Hornier, Wilhelm, Coq, Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, E. Coq, M. Esnard, 11°, 1037 hpa, vent nul, très bonne visibilité, comptage 5/5 à partir de la pointe du barrage"] <- "Hornier, Wilhelm, Coq, Esnard"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. COQ, vent moyen (Sud), température: 7°C, couvert mais bonne visibilité\nSecteurs barrage jusque aval seuil 2"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.HORNIER, E.COQ, vent moyen (Sud), température 10°C, couvert mais bonne visibilité\nCanal + contre canal"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. Coq, 9°C, vent nul, temps couvert,"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, E. Coq, 10°C, vent nul, temps couvert"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq"] <- "Hornier, Coq, Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq\nCanal de décharge de l'Ill en crue"] <- "Hornier, Coq, Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, V Klethi, E Coq\nCanal de décharge en crue, 150 à 200m^3/s"] <- "Hornier, Coq, Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\nsoleil, vent faible\nbonne visibilité"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps couvert\nvent faible"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps ensoleillé\nenviron 13°C, vent faible"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\nsoleil, 12°C, vent faible"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E.COQ\ntemps couvert / pluie le matin\nvent faible mais de plus en plus présent"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq\nTemps couvert, vent moyen, niveaux d'eau élevés\nT° 20°C, P 1005hPa"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, E Coq\nPluie et orage, visibilité réduite (arbres), vent moyen\nT° 20°C, P 1005hPa"] <- "Hornier, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nSoleil, vent faible\nT° 25°C"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nBeau temps, vent faible\nT° 22-30°C, P 1118hPa"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Coq\nSoleil, vent faible\nT° 19-24°C"] <- "Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi\nSoleil, vent faible\nT° 22°C"] <- "Wilhelm, Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi\nSoleil, vent faible\nT° 29°C"] <- "Wilhelm, Klethi"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 25°C\nPerturbations : 28 voiliers, 14 bateaux à moteur"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 27°C\nPerturbations : 1 float tube, 1 bateau de pêche"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, V Klethi, E Coq\nTemps couvert, vent moyen\nT° 25°C"] <- "Wilhelm, Klethi, Coq"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 1024 hpa, 18 ° vent nul, débit entre 1000 et 1500 m3/s en hausse, surface importante de végétation aquatique émergente sous le barrage de répartition des eaux (amont)"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau tempsd, vent nul, 25°, débit 1000 à 1500 m3/s, comprenant contre-canal de drainage et musoirs, comptage 5/5"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, pas de nuages, vent nul, 1024 hpa, 15 à 26 °, niveau d'eau normal, haut, pas de zones émergées"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27 °, beau temps, pas de nuages, vent nul, niveau normal, haut, pas de zone émergée, comptage à partir de rive est"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27°, zero nuages, vent nul, comptage à partir des digues est et tiroir"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, beau temps, 27°, zero nuages, vent nul, comptage à partir des digues tiroir"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan Hornier, 25 à 29 °, vent nul, ciel sans nuage, 1017 hpa, Rhin à 1300 m3/s,"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Erwan.hornier, beau temps, 29/30 °, vent nul, pas de nuages, 1300m3/S environ, conditions dévarables au stationnement d'oiseaux, bancs de sables et graviers inexistants, courant assez fort, eaux turbides"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, 17°c, temps variable, pression 1019 hpa, vent moyen, visibilité bonne, qualité comptage 3/5, 6 bateaux de pêche, 1 planche à voile"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps variable, vent moyen à fort, temp 17°, pression 1019, visibilité moyenne, qualité comptage 3/5, 3 bateaux de pêche, 1 voilier, comptage depuuis Rhinland"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, Temps variable, 17°, vent moyen, pression 1019, visibilité moyenne, comptage à partir de banquette ouest, estimation qualité comptage 4/5, 3 pêcheurs à pied Thumenau"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, temps nuageux, temp 20°, vent faible, niveau eau élevé, visibilité bonne, estimation qualité comptage '/5, comptage depuis banquette sud-est"] <- "Hornier, Wilhelm"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, , beau temps, frais, vent moyen, bonne visibilité, temp 15°c, 1027 hpa, comptage à partir du barrage de répartition amont et des berges françaises, qualité 4/5, 8 à 10 pêcheurs à pied"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, beau temps frais, temp 18°C, 1026 hpa, vent moyen, visibilité bonne, qualité de comptage 5/5 travaux de fauche sur la digue ouest, comptage à partir de la digue ouest"] <- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, visibilité bonne, temp 19°, couvert, légère pluie, pas de vent, 1011 hpa, comptage à partir de la digue ouest et du barrage, présence de tracteur d'entretien sur la digue. qualité comptage 5/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier,  à partir de l'îlôt nord, temp 19°, couvert, légère pluie, vent nul, bonne visibilité, 1011hpa, qualité comptage 4/5 (mouvements d'oiseaux lié à la présence de 5 pêcheurs (à pied, float-tube, bateau)"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, couvert, bonne visibilité, vent faible, pas de pluie, dérangement (1 bateau de pêche, 9 pêcheurs à pieds, 3 chiens en divah=gation côté allemand,), débit normal, comptage à partir du côté francais et du barrage de réaprtition, comptage 4/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°c, 1021 hpa, vent faible, pas de pluie, bonne visibilité, temps couvert, comptage à partir des digues ouest et est (aval), 6 pêcheurs à pied sur digues est aval, comptage 5/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 13°, 1015 hpa, temps couvert, visibilité assez bonne, brouillard léger, bruine, pas de vent, comptage à partir de la pointe sud et de la digue est, 1 pécheur pointe sud, 2 ornithos musoir du barrage, comptage 3/5, niveau d'eau moyen, pas de haut fond élergés, groupe de 30 à 50 limicoles en vol, ne se posent pas"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, Thumenau + Etrangement, comptage à partir de la berge Est, 13 °, 1015 hpa, temps couvert, vent faible, pas de pluie, pas de brouillard, comptage 4/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°, temps couvert, vent nul, léger brouillard, pas de pluie, 1012 hpa, comptage à partir des digues Est, tiroir et Ouest, 4 bateaux de pêche, 1 float-tube, comptage 4/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 14°, 1012 hpa, temps couvert, léger brouillard, pas de pluie, 18 bateaux de ^pêche, comptage à partir de la digue tiroir et de la digue ouest, comptage 4/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, 12°, 1023 hpa, temps demi-couvert, bonne visibilité, vent faible, pas de brume, pas de pluie, 1 pêcheur à pied, comptage 4/5"]<- "Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="Hornier E, 14°, temps demi-couvert, 1023 hpa, bonne visibilité, vent faible à nul, pas de pluie, pas de brume, 1 barque de pêche, 2 pecheurs à pied, niveau d'eau un peu plus haut qu'habituellement, pas de hauts fonds émergés. Comptage 4/5"]<- "Erwan Hornier"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir des digues ouest et est (aval) ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 2 pêcheurs, 1 chien non tenu en laisse"]<- "Wilhelm, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="JL Wilhelm, G Laurent ; comptage à partir du côté français et du barrage de répartition ; qualité du comptage estimée à 3/5 ; niveaux d'eau normaux\nT° 1°C   Temps couvert avec nappes de brouillard\nVisibilité moyenne à mauvaise   Vent faible   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : aucune"]<- "Wilhelm, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, G. Laurent, temps demi-couvert, 3°, 1008hpa, léger brouillard, visibilité moyenne à bonne, à partir de la pointe sud et du barrage mobile, niveaux d'eau normaux, qualité de comptage estimé à 5/5"] <- "Hornier, Wilhelm, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E. Hornier, JL Wilhelm, G. Laurent, 5°, 1008 hpa, temps demi-couvert, visibiité moyenne à bonne, 7 barques de pêche, 1 voilier, 1 chien non tenu en laisse, qualité de comptage 4/5"] <- "Hornier, Wilhelm, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir des banquettes est (comprend le secteur Thumenau et l’étranglement) ; qualité du comptage estimée à 5/5 ; niveaux d'eau normaux\nT° 8°C   P 1008hPa   Temps demi-couvert\nVisibilité bonne   Vent absent   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : aucune"] <- "Hornier, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue tiroir et des digues est et ouest ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 5°C   P 1008hPa   Temps brouillard\nVisibilité mauvaise   Vent absent   Brouillard gênant   Brume de chaleur absente   Pluie absente\nPerturbations : 1 kayakiste-pêcheur, 7 barques de pêche"] <- "Hornier, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de la digue ouest et du barrage ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux (débit 800m3/s)\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen (10km/s)   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 ornithologue-photographe, 1 voiture, au moins 3 péniches"] <- "Hornier, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="E Hornier, G Laurent ; comptage à partir de l'îlot et du musoir ; qualité du comptage estimée à 4/5 ; niveaux d'eau normaux\nT° 9 à 11°C   P 1008hPa   Temps quart à demi-couvert\nVisibilité bonne   Vent moyen   Brouillard absent   Brume de chaleur absente   Pluie absente\nPerturbations : 1 vedette, 1 bateau, 3 promeneurs, 1 chien"] <- "Hornier, Laurent"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Compteurs : Valérie-Anne Clément-Demange, Daniel Kirmser, Stéphane Umhang (+ amie), Charlotte Wagner."] <- "Kirmser, Clément-Demange, Umhang, Wagner"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Comptage franco-allemand (LPO Alsace + FOSOR). Bonnes conditions météo. Mais comptage perturbé en matinée par des barques de pêche provoquant des envols d'oiseaux, concernant surtout les Fuligules morillons, comptés approximativement en vol.\nAu total : 7607 oiseaux (sans compter les laridés, ni passereaux et rapaces).\nParticipants :\n- LPO Alsace : Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Jean-Marie RISSE\n- FOSOR (Fachschaft für Ornithologie im Südlichen OberRhein) : Kira DONDERER, Sophia-Marie JACK, Mathias MUELLER, Jürgen RUPP, Victor WEMBER, Anton & Gustav WILD"] <- "Bronner, Carasco, Fetique, Hiss, Risse, Donderer, Jack, Mueller, Rupp, Wember, Wild, Wild"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe : Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Daniel Kirmser, Delphine Lacuisse, Stéphane Umhang."] <- "Kirmser, Brunissen, Clément-Demange, Didier, Lacuisse, Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Archives Wetlands Alsace ; Observateur = Stéphane Umhang.\nDonnées saisies par Christian Frauli à la demande de Christian Dronneau, pour obtenir dans le module WI le nombre réel d'oies cendrées recensées sur le site de Gerstheim ce jour-là."] <- "Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Participants : Carole BIZART, Jean-Marc BRONNER, Yann CARASCO, Luca FETIQUE, Jean-Pierre HISS, Victor ROUAULT.\nMétéo : 10 à 15 cm de neige au sol; redoux en cours, avec températures devenant légèrement positives en journée. Ciel couvert. Quelques faibles pluies et neige mêlées, puis quelques faibles pluies éparses."] <- "Bronner, Carasco, Fetique, Hiss, Rouault"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Observateur : Stéphane Goubert ; donnée saisie par Christian Frauli.\nN.B. : effectif réel = 14 ind. ; effectif saisi = 6 ind., soit le complément à l'effectif déjà recensé (8 ind.) lors du comptage officiel du site le 16/01/2022."] <- "Frauli, Goubert"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; En remplacement du compteur traditionnel (Eric BUCHEL), absent."] <- "Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Equipe de comptage : Eric Brunissen, Sébastien Didier, Daniel Kirmser, Delphine Lacuisse, Stéphane Umhang."] <- "Kirmser, Brunissen, Didier, Lacuisse, Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 10 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Luca Fetique, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Max Kurzmann, Matthias Müller, Jürgen Rupp). Bonnes conditions météo (bonnes visibilités, vent calme, températures proches de 0° en matinée, puis très faiblement positives l'après-midi). Nombre total d'oiseaux observés : 7.836."] <- "Bronner, Bizart, Carasco, Fetique, Hiss, Risse, Rouault, Kurzmann, Müller, Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Christian Frauli ; Observateur : Christian Dronneau ; donnée saisie par Christian Frauli.\nN.B. : effectif réel = 68 ind. ; effectif saisi : 9 ind. (= effectif complémentaire à l'effectif WI déjà saisi lors du comptage officiel du site le 16/01/2022)."] <- "Dronneau"
Rhin$obs[Rhin$commentaire_de_la_liste=="Archives Wetlands Alsace ; Observateur : Marc HELFTER ; donnée saisie par Christian FRAULI."] <- "Helfter, Frauli"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; Avec Anne Tritz (prise de notes)"] <- "Buchel, Tritz"
Rhin$obs[Rhin$commentaire_de_la_liste=="Eric Buchel ; avec Anne Tritz (prise de notes)"] <- "Buchel, Tritz"
Rhin$obs[Rhin$commentaire_de_la_liste=="Daniel Kirmser ; Observateurs : Eric Brunissen, Valérie-Anne Clément-Demange, Sébastien Didier, Jérôme Isambert, Daniel Kirmser, Delphine Lacuisse, Nathan Roser-Robert, Nicolas Roser, Stéphane Umhang."] <- "Kirmser, Brunissen, Clément-Demange, Didier, Isambert, Lacuisse, Roser-Robert, Roser, Umhang"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 7 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Jürgen Rupp). Conditions météo médiocres le matin (pluvieux et venteux) pour la partie Nord du plan d’eau, puis devenant bonnes à partir de midi (temps sec, sans vent significatif) pour la partie Sud. Conformément au protocole, les oiseaux du canal de décharge de l’Ill ont été intégrés aux présentes données du secteur [17]. Nombre total d'oiseaux observés sur l’ensemble des secteurs [17], [17A] (canal d'alimentation de l'Ill) et [18] (cours du Rhin): 3918 oiseaux d’eau (chiffre le plus faible relevé sur ces sites depuis de très nombreuses années, voire depuis le début des comptages –hors périodes de gel total-, et de loin !)."] <- "Bronner, Bizart, Carasco, Hiss, Risse, Rouault, Rupp"
Rhin$obs[Rhin$commentaire_de_la_liste=="Jean-Marc Bronner ; Recensement par une équipe franco-allemande de 7 personnes : LPO-Alsace (Carole Bizart, Jean-Marc Bronner, Yann Carasco, Jean-Pierre Hiss, Jean-Marie Risse, Victor Rouault) + FOSOR (Jürgen Rupp). Conditions météo médiocres le matin (pluvieux et venteux) pour la partie Nord du plan d’eau, puis devenant bonnes à partir de midi (temps sec, sans vent significatif) pour la partie Sud. Conformément au protocole, les oiseaux du canal de décharge de l’Ill ont été intégrés aux présentes données du secteur [17]. Nombre total d'oiseaux observés sur l’ensemble des secteurs [17], [17A] (canal d'alimentation de l'Ill) et [18] (cours du Rhin): 3918 oiseaux d’eau (chiffre le plus faible relevé sur ces sites depuis de très nombreuses années, voire depuis le début des comptages –hors périodes de gel total-, et de loin !)."] <- "Bronner, Bizart, Carasco, Hiss, Risse, Rouault, Rupp"

Rhin[,45] <- tolower(Rhin[,45])
Rhin[,45] <- iconv(Rhin[,45], from = "UTF-8", to = "ASCII//TRANSLIT")
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

colnames(Rhin)[47] <- "nb_annee_suivie"

#Colonne site retenu: 
Rhin$site_retenu <- with(Rhin, ifelse(Rhin$nb_annee_suivie < 3, "non", 
                                      ifelse(Rhin$site=="amontbarrage_avalbarrage","non",
                                       ifelse(Rhin$site=="secteursud_rhinland_basenautique","non",
                                        ifelse(Rhin$site=="vieuxrhin_grandcanalalsace","non","oui")))))

Rhin$voie_migration <- "est_atlantique/mediterranee"


id <- paste0(Rhin$site,Rhin$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Rhin$site,Rhin$secteur, Rhin$protocole, Rhin$nb_annee_suivie, Rhin$qualite_comptage, Rhin$voie_migration, Rhin$site_retenu)
site <- unique(site) 
table(duplicated(site$id))
site %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

site$Rhin.qualite_comptage[site$id=="2023-01-15"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="avalbarrage2017-02-28"] <- "ok"
site$Rhin.qualite_comptage[site$id=="grandcanalalsace2016-11-22"] <- "ok"
site$Rhin.qualite_comptage[site$id=="grandcanalalsace2018-03-12"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="rhinland2019-07-19"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="rhinland2017-12-12"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="rhinland2018-03-27"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="rhinland2018-12-14"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="rhinland2019-11-22"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="secteursud2018-02-20"] <- "douteux"
site$Rhin.qualite_comptage[site$id=="vieuxrhin2016-11-22"] <- "ok"
site$Rhin.qualite_comptage[site$id=="vieuxrhin2017-02-23"] <- "ok"
site$Rhin.qualite_comptage[site$id=="vieuxrhin2019-04-16"] <- "ok"

site <- unique(site)

# 3. Création de la table inventaire : 

inv <- data.frame(id,Rhin$date,Rhin$obs,Rhin$mois,Rhin$annee)
inv <- unique(inv)
table(duplicated(inv$id))

inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

inv$Rhin.obs[inv$id=="2017-10-17"] <- "hornier, esteve"
inv$Rhin.obs[inv$id=="avalbarrage2017-02-28"] <- "hornier, wilhelm"
inv$Rhin.obs[inv$id=="grandcanalalsace2021-01-17"] <- "kirmser, brunissen, clément-demange, didier, lacuisse, umhang"
inv$Rhin.obs[inv$id=="rhinland2017-09-21"] <- "hornier, esteve"
inv$Rhin.obs[inv$id=="vieuxrhin2019-04-16"] <- "coq"
inv$Rhin.obs[inv$id=="vieuxrhin2017-02-23"] <- "hornier, wilhelm"

inv <- unique(inv)

# Compilation des deux tables : 
Rhin_inv <- merge(site,inv,by.x = "id", by.y = "id")


# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Rhin$id <- paste0(Rhin$espece,Rhin$site,Rhin$date)
Rhin$id_inv <- paste0(Rhin$site,Rhin$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Rhin$id_inv)
sp <- unique(Rhin$espece)

inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)
Rhin_obs <- data.frame(Rhin$abondance, Rhin$id)
Rhin_obs <- aggregate(Rhin_obs, Rhin.abondance ~ Rhin.id, median)

# Combinaison des deux tableaux : 

Rhin_f <- merge(inventaire, Rhin_obs, by.x = "id_sp", by.y = "Rhin.id", all.x = T)

#remplacer les na par les 0 
Rhin_f[is.na(Rhin_f)] <- 0

# Var 2 -> espece :
colnames(Rhin_f)[names(Rhin_f)== "Var2"] <- "espece"
colnames(Rhin_f)[names(Rhin_f)== "Var1"] <- "id"


# Merge des tables : 
Rhin_f <- merge(Rhin_f, Rhin_inv, by.x = "id", by.y = "id")
colnames(Rhin_f) <- gsub("Rhin.","",colnames(Rhin_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Rhin_f$abondance <- as.numeric(Rhin_f$abondance)
setDT(Rhin_f)
Rhin_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Rhin_f)
Rhin_f <- subset(Rhin_f, abondance_tot > 0)

write.csv2(Rhin_f,"Data/Reserve_du_Rhin.csv")

      ########## 7 Baie de Saint-Brieuc ############


Brieuc <- read.csv("Data/data_saint_brieuc.csv", header = F, sep = ";")
View(Brieuc)

colnames(Brieuc) <- gsub("V4","espece",colnames(Brieuc))
colnames(Brieuc) <- gsub("V5","protocole_observation",colnames(Brieuc))
colnames(Brieuc) <- gsub("V6","type_de_comptage",colnames(Brieuc))
colnames(Brieuc) <- gsub("V7","date",colnames(Brieuc))
colnames(Brieuc) <- gsub("V8","obs",colnames(Brieuc))
colnames(Brieuc) <- gsub("V12","site",colnames(Brieuc))
colnames(Brieuc) <- gsub("V13","lattitude",colnames(Brieuc))
colnames(Brieuc) <- gsub("V14","longitude",colnames(Brieuc))
colnames(Brieuc) <- gsub("V15","lieu_observation",colnames(Brieuc))
colnames(Brieuc) <- gsub("V16","abondance",colnames(Brieuc))
colnames(Brieuc) <- gsub("V18","precision",colnames(Brieuc))
colnames(Brieuc) <- gsub("V19","remarques",colnames(Brieuc))

Brieuc <- Brieuc[-c(1,2),]

#Comptage bi-mensuel depuis 2012 ; mensuel depuis 1996 et annuel au-delà :

#Rajout de la colonne date : 

#La date : 
Brieuc$date <- ymd(Brieuc$date)

#Ajouter le mois et l'année 
Brieuc$mois <- month(Brieuc$date)
Brieuc$annee <- year(Brieuc$date)

#Les espèces : 
Brieuc [,4] <- tolower(Brieuc[,4])
Brieuc[,4] <- iconv(Brieuc[,4], from = 'UTF-8',to = 'ASCII//TRANSLIT')
Brieuc [,4] <- gsub(" ","_",Brieuc[,4])
Brieuc [,4] <- gsub("'","_",Brieuc[,4])
Brieuc [,4] <- gsub("-","_",Brieuc[,4])
sort(unique(Brieuc$espece))

Brieuc [,4] <- gsub("chevalier_combattant","combattant_varie",Brieuc[,4])
Brieuc [,4] <- gsub("bernache_cravant_a_ventre_pale","bernache_cravant_occidentale",Brieuc[,4])


#Sélection des anatidés et limicoles : 
espece <- read.csv("Data/espece.csv", header = T)
espece <- espece[-c(98,301,427),]

espece[,5] <- tolower(espece[,5])
espece[,5] <- gsub(" ","_",espece[,5])
espece[,5] <- gsub("'","_",espece[,5])
espece[,5] <- gsub("-","_",espece[,5])
espece[,5] <- iconv(espece[,5],from = 'UTF-8',to = 'ASCII//TRANSLIT')
sort(unique(espece$french_name))
Brieuc <- merge(Brieuc, espece, by.x = "espece", by.y = "french_name", all.x = T) 

#Voir les doublons 
tabledupli <- Brieuc %>%
  group_by(V1) %>%
  filter(n()>1) %>%
  ungroup() 

Brieuc$order_tax[Brieuc$espece=="bernache_cravant_du_pacifique"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="canard_de_chiloe"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="dendrocygne_fauve"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="eider_a_tete_grise"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="ouette_de_magellan"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="nette_demi_deuil"] <- "Ansériformes"
Brieuc$order_tax[Brieuc$espece=="oie_a_tete_barree"] <- "Ansériformes"

Brieuc$family_tax[Brieuc$espece=="bernache_cravant_du_pacifique"] <- "Anatidés"
Brieuc$family_tax[Brieuc$espece=="dendrocygne_fauve"] <- "Anatidés"
Brieuc$family_tax[Brieuc$espece=="eider_a_tete_grise"] <- "Anatidés"
Brieuc$family_tax[Brieuc$espece=="ouette_de_magellan"] <- "Anatidés"
Brieuc$family_tax[Brieuc$espece=="oie_a_tete_barree"] <- "Anatidés"

unique(Brieuc$order_tax)
Brieuc <- subset(Brieuc,(Brieuc$order_tax=="Charadriiformes"|Brieuc$order_tax=="Ansériformes"))

unique(Brieuc$family_tax)
Brieuc <- subset(Brieuc, (Brieuc$family_tax=="Recurvirostridés"|Brieuc$family_tax=="Scolopacidés"
                             |Brieuc$family_tax=="Anatidés"|Brieuc$family_tax=="Anatidae"
                            |Brieuc$family_tax=="Charadriidés"|Brieuc$family_tax=="Haematropodidés"
                            |Brieuc$family_tax=="Burhinidés"))

sort(unique(Brieuc$espece))

#Les sites 
sort(unique(Brieuc$site))
Brieuc[,12] <- tolower(Brieuc[,12])
Brieuc[,12] <- gsub(" ","_",Brieuc[,12])
Brieuc[,12]<- gsub("'","_",Brieuc[,12])
Brieuc[,12] <- gsub("-","_",Brieuc[,12])
Brieuc[,12] <- iconv(Brieuc[,12],from = 'UTF-8',to = 'ASCII//TRANSLIT')

Brieuc [,12] <- gsub("dpm_anse_d_yffiniac___zpr","dpm_anse_d_yffiniac_zpr",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_d_yffiniac___zpr_est","dpm_anse_d_yffiniac_zpr_est",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_d_yffiniac___zpr_ouest","dpm_anse_d_yffiniac_zpr_ouest",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_d_yffiniac___zpr_sud","dpm_anse_d_yffiniac_zpr_sud",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_d_yffiniac__estran","dpm_anse_d_yffiniac_estran",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_de_morieux___est","dpm_anse_de_morieux_est",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_de_morieux___ouest","dpm_anse_de_morieux_ouest",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_de_morieux___ouest___zh_littorale","dpm_anse_de_morieux_ouest_zh_littorale",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_de_morieux___roc_verd","dpm_anse_de_morieux_roc_verd",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_zone_marine_hors_zps_(ouest_baie)","dpm_zone_marine_hors_zps_ouest_baie",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_zps_estran_hors_reserve_(zone_est_baie)","dpm_zps_estran_hors_reserve_zone_est_baie",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_zps_greve_des_courses","dpm_zps_greve_des_courses",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_zps_zone_marine_(zone_est_baie)","dpm_zps_zone_marine_zone_est_baie",Brieuc[,12])
Brieuc [,12] <- gsub("dpm_anse_de_morieux_ouest___zh_littorale","dpm_anse_de_morieux_ouest_zh_littorale",Brieuc[,12])

#Les points d'observation : 
sort(unique(Brieuc$lieu_observation))

#Ajouter le nom du secteur 

Brieuc$secteur <- "baie_de_saint_brieuc"

#Ajouter le protocole de comptage 

Brieuc$protocole <- "terrestre"

#Les observateurs : 
unique(Brieuc$obs)

Brieuc$obs <- "reserve_naturelle"

#Prendre en compte les remarques : 
sort(unique(Brieuc$remarques))

#Ajouter la colonne qualité du comptage : 
Brieuc$qualite_comptage <- "ok"

Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 19"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 20"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 21"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 22"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 23"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 24"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 25"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 26"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 28"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 30"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 32"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 33"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 34"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 35"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 36"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 37"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 38"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 39"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 40"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 41"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 42"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 47"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 48"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 49"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 54"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 55"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 56"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 57"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 58"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 59"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 60"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 64"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 65"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 70"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 73"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 74"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 75"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="attention dérangement kite anse yffiniac, comptage certains anatidés le 76"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="BROUILLARD ET CHIEN SUR YFFINIAC"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="Chiens sans laisse"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="comptage avancé d'un jour cuase météo le lendemain, donc marée plus haute que d'habitude"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="comptage non complet"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="dérangement avant comptage Yffiniac et pendant Morieux et bateau mytili macreuse"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="derangement collecte algues vertes du Bon abri"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="dérangement sur morieux et évapotranspiration sur Yffiniac"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="Dérangement ULM avant comptage"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="environ, comptage incomplet"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="KITE SURF A BELIARD\nPas de courlis sur béliard et de nombreux envols"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="incomplet indiv ds les herbus"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="Kitesurf dans la RN"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="pas eu le temps de compter, et mauvaises conditions ensuite (envol, luminosité...)"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="présent pas compté difficulté lié aux conditions météo"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous-sestimé"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimation des bécasseau pb luminosité solei de face"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimé"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimé probablement"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimé probablement, nbx individus dans les prés salés à cause du vent"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimés"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="sous estimés prés salés"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="surement sous estimé"] <- "douteux"
Brieuc$qualite_comptage[Brieuc$remarques=="visibilité et conditions d'observation médiocre, probablement largement sous -estimé"] <- "douteux"

#Ajouter une colonne pour le nombre d'observation : 

nb_observation <- Brieuc %>% subset(abondance > 0) %>%
  count(espece)

Brieuc <- merge(Brieuc,nb_observation, by.x = "espece",by.y = "espece")
colnames(Brieuc)[39] <- "occurence_sp"

#Ajouter la colonne voie de migration 

Brieuc$voie_migration <- "est_atlantique"


id <- paste0(Brieuc$site,Brieuc$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Brieuc$site,Brieuc$secteur, Brieuc$protocole, Brieuc$qualite_comptage, Brieuc$voie_migration)
site <- unique(site) 
table(duplicated(site$id))
site %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_estran2005-12-07"] <- "ok"

site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2011-03-25"]<- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2010-01-07"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_estran2013-11-13"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2010-05-12"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2012-04-24"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_de_morieux_est2021-11-30"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2011-11-01"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2002-03-17"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_estran2005-12-07"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_ouest2004-05-10"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_de_morieux_est2012-03-05"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_de_morieux_est2013-10-16"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_est2012-10-26"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_de_morieux_est2020-03-07"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_est2012-07-09"] <- "ok"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_sud2014-04-25"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_zpr_est2012-05-25"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_fronteven2017-06-21"] <- "douteux"
site$Brieuc.qualite_comptage[site$id=="dpm_anse_d_yffiniac_estran2010-05-12"] <- "ok"

site <- unique(site) 

# 3. Création de la table inventaire : 

inv <- data.frame(id,Brieuc$date,Brieuc$obs,Brieuc$mois,Brieuc$annee)
inv <- unique(inv)
table(duplicated(inv$id))

inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

# Compilation des deux tables : 
Brieuc_inv <- merge(site,inv,by.x = "id", by.y = "id")

# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Brieuc$id <- paste0(Brieuc$espece,Brieuc$site,Brieuc$date)
Brieuc$id_inv <- paste0(Brieuc$site,Brieuc$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Brieuc$id_inv)
sp <- unique(Brieuc$espece)

inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)
Brieuc_obs <- data.frame(Brieuc$abondance, Brieuc$id)
Brieuc_obs <- aggregate(Brieuc_obs, Brieuc.abondance ~ Brieuc.id, median)

# Combinaison des deux tableaux : 

Brieuc_f <- merge(inventaire, Brieuc_obs, by.x = "id_sp", by.y = "Brieuc.id", all.x = T)

#remplacer les na par les 0 
Brieuc_f[is.na(Brieuc_f)] <- 0

# Var 2 -> espece :
colnames(Brieuc_f)[names(Brieuc_f)== "Var2"] <- "espece"
colnames(Brieuc_f)[names(Brieuc_f)== "Var1"] <- "id"


# Merge des tables : 
Brieuc_f <- merge(Brieuc_f, Brieuc_inv, by.x = "id", by.y = "id")
colnames(Brieuc_f) <- gsub("Brieuc.","",colnames(Brieuc_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Brieuc_f$abondance <- as.numeric(Brieuc_f$abondance)
setDT(Brieuc_f)
Brieuc_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Brieuc_f)
Brieuc_f <- subset(Brieuc_f, abondance_tot > 0)

write.csv2(Brieuc_f,"Data/Baie_saint_brieuc.csv")

######### 8 Marais d'Orx ############

Orx_2014 <- read.csv("Data/donnees_marais_d_orx_2014.csv", header = T, fileEncoding = "UTF-8", sep = ";")

colnames(Orx_2014) <- tolower(colnames(Orx_2014))

#Régler la forme de la date :
colnames(Orx_2014)[8] <- "date"
Orx_2014$date <- dmy(Orx_2014$date)

#Les noms des espèces : 
colnames(Orx_2014)[25] <- "identifiant_esp"
colnames(Orx_2014)[29] <- "espece" 
sort(unique(Orx_2014$espece))

Orx_2014[,29] <- tolower(Orx_2014[,29])
Orx_2014[,29] <- gsub(" ","_",Orx_2014[,29])
Orx_2014[,29] <- gsub("\\.","", Orx_2014[,29])
Orx_2014[,29] <- gsub("'","_",Orx_2014[,29])
Orx_2014[,29] <-iconv(Orx_2014[,29], from = 'UTF-8', to = 'ASCII//TRANSLIT')

sort(unique(Orx_2014$espece))

#Sélection des anatidés : 
Orx_2014 <- subset(Orx_2014,(Orx_2014$ordre=="Anseriformes"))

#Les abondances : 
colnames(Orx_2014)[35] <- "abondance"
Orx_2014[is.na(Orx_2014)] <- 0

#Les sites : 
colnames(Orx_2014)[17] <- "site"

Orx_2014[,17] <- tolower(Orx_2014[,17])
Orx_2014[,17] <- gsub(" ","_",Orx_2014[,17])
Orx_2014[,17] <- gsub("'","_",Orx_2014[,17])  
Orx_2014[,17] <- iconv(Orx_2014[,17], from = 'UTF-8', to = 'ASCII//TRANSLIT')  
  
sort(unique(Orx_2014$site))

#Les observateurs : 
colnames(Orx_2014)[7] <- "obs"

Orx_2014[,7] <- tolower(Orx_2014[,7])

#Données de 2015 à 2024 : 

Orx_2015_2024 <- read.csv("Data/donnees_marais_d_orx_2015_2024.csv", header = T, sep = ";",fileEncoding = "UTF-8")
Orx_2015_2024 <- Orx_2015_2024[-c(1),]

#Régler le problème de la date : 

colnames(Orx_2015_2024)[8] <- "date"
unique(Orx_2015_2024$date)


Orx_2015_2024$date <- Orx_2015_2024$date |> str_replace_all(c("janv"="01","févr"="02",
                                        "mars"="03","sept"="09","oct"="10",
                                        "nov"="11","déc"="12"))

Orx_2015_2024$date <- dmy(Orx_2015_2024$date)
                            
#Les noms des espèces : 
colnames(Orx_2015_2024)[3] <- "espece" 
sort(unique(Orx_2015_2024$espece))

Orx_2015_2024[,3] <- tolower(Orx_2015_2024[,3])
Orx_2015_2024[,3] <- gsub(" ","_",Orx_2015_2024[,3])
Orx_2015_2024[,3] <- gsub("\\.","", Orx_2015_2024[,3])
Orx_2015_2024[,3] <- gsub("'","_",Orx_2015_2024[,3])
Orx_2015_2024[,3] <-iconv(Orx_2015_2024[,3], from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Sélection des anatidés : 
unique(Orx_2015_2024$FAMILY_NAME)
Orx_2015_2024 <- subset(Orx_2015_2024,(Orx_2015_2024$FAMILY_NAME=="Anatidae"))
                                       
#Les abondances : 
colnames(Orx_2015_2024)[26] <- "abondance"
unique(Orx_2015_2024$abondance)

#Les sites : 
colnames(Orx_2015_2024)[16] <- "site"

Orx_2015_2024[,16] <- tolower(Orx_2015_2024[,16])
Orx_2015_2024[,16] <- gsub(" ","_",Orx_2015_2024[,16])
Orx_2015_2024[,16] <- gsub("'","_",Orx_2015_2024[,16])  
Orx_2015_2024[,16] <- iconv(Orx_2015_2024[,16], from = 'UTF-8', to = 'ASCII//TRANSLIT')  

sort(unique(Orx_2015_2024$site))

#site marais d'orx générique ==> agrégés à l'ensemble du marais
# rnn => probablement une aggrégation également (à vérifier)

#Les remarques : 
colnames(Orx_2015_2024)[14] <- "remarques"        
                          
#Fusionner les deux tableaux                       
help("bind_rows")
class(Orx_2014$abondance)
class(Orx_2015_2024$abondance)
Orx_2015_2024$abondance <- as.numeric(Orx_2015_2024$abondance)
Orx <- bind_rows(Orx_2014,Orx_2015_2024)

#Ajouter le mois et l'année 

Orx$mois <- month(Orx$date)
Orx$annee <- year(Orx$date)

#voir les espèces : 
sort(unique(Orx$espece))

#Voir les noms de sites : 
sort(unique(Orx$site))

#Voir les doublons 
tabledupli <- Orx %>%
  group_by_all() %>%
  filter(n()>1) %>%
  ungroup()

#Ajout protocole :
Orx$protocole <- "terrestre"

#Ajout secteur : 
Orx$secteur <- "marais_d_orx"

#Prendre en compte les remarques : 
unique(Orx$remarques)

#Création colonne qualité comptage : 
Orx$qualite_comptage <- "ok"

Orx$qualite_comptage[Orx$remarques=="Comptage sur l'ensemble du site, les chiffres sont approximatifs étant donné la superficie et les distances d'observation parfois importantes."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Dénombrement bimensuel, conditions météorologiques difficile, visibilité mauvaise, le Casier Burret n'est pas pris en compte dans ce comptage."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Comptage approximatif sur l'ensemble du site. Comptage difficile sur le casier Burret en raison de la végétation et de la visibilité."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Il pleut! Pas mal d'oiseaux sur le casier barrage, plus de 2500 canards comptabilisés ce matin toutes espèces confondues."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Comptage difficile en raison du niveau d'eau très haut sur le casier Central, de nombreux oiseaux cachés sous les saules et la végétation inondés (surtout sarcelle d 'hiver et canard colvert). Comptage difficile sur casier Burret en raison de la forte densité d'herbiers de jussie dans lesquels les oiseaux se remisent la journée."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Comptage très difficile et plus exhaustif que d'habitude en raison d'une inondation record sur le site. Toutes les berges et boisements humides sont sous l'eau et offrent de nombreuses zones de repos à l'abris des regards..."] <- "douteux"
Orx$qualite_comptage[Orx$remarques=="Comptage bimensuel, niveaux d'eau très haut sur l'ensemble des casiers"] <- "douteux"

Orx$voie_migration <- "est_atlantique"


id <- paste0(Orx$site,Orx$date)
unique(id)
# 2. Création de la table "site" : 
site <- data.frame(id, Orx$site,Orx$secteur, Orx$protocole, Orx$qualite_comptage, Orx$voie_migration)
site <- unique(site) 
table(duplicated(site$id))
site %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

#Création de la table inventaire
inv <- data.frame(id,Orx$date,Orx$obs,Orx$mois,Orx$annee)
inv <- unique(inv)
table(duplicated(inv$id))

inv %>%
  group_by(id) %>%
  filter(n()>1) %>%
  ungroup() %>% View()

# Compilation des deux tables : 
Orx_inv <- merge(site,inv,by.x = "id", by.y = "id")

# 4. Création de la table comptage (table d'observation) : 
# -> création d'un id pour fusionner les tables (avec les espèces)
Orx$id <- paste0(Orx$espece,Orx$site,Orx$date)
Orx$id_inv <- paste0(Orx$site,Orx$date)

#Création du tableau inventaire qu'on va croiser avec le jeu de données : 

id_inv <- unique(Orx$id_inv)
sp <- unique(Orx$espece)

inventaire <- expand.grid(id_inv, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$id_sp <- paste0(inventaire$Var2,inventaire$Var1)
Orx_obs <- data.frame(Orx$abondance, Orx$id)
Orx_obs <- aggregate(Orx_obs, Orx.abondance ~ Orx.id, median)

# Combinaison des deux tableaux : 

Orx_f <- merge(inventaire, Orx_obs, by.x = "id_sp", by.y = "Orx.id", all.x = T)

#remplacer les na par les 0 
Orx_f[is.na(Orx_f)] <- 0

# Var 2 -> espece :
colnames(Orx_f)[names(Orx_f)== "Var2"] <- "espece"
colnames(Orx_f)[names(Orx_f)== "Var1"] <- "id"

# Merge des tables : 
Orx_f <- merge(Orx_f, Orx_inv, by.x = "id", by.y = "id")
colnames(Orx_f) <- gsub("Orx.","",colnames(Orx_f))

# Rajouter la somme des abondances pour chaque espèce/site 
Orx_f$abondance <- as.numeric(Orx_f$abondance)
setDT(Orx_f)
Orx_f[, abondance_tot:= sum(abondance), by = .(espece,site)]
setDF(Orx_f)
Orx_f <- subset(Orx_f, abondance_tot > 0)

write.csv2(Orx_f,"Data/marais_d_orx.csv")


################### FUSION TABLEAU DONNEES ##############
help("rbind")
help("bind_rows")

Loire <- read.csv2("Data/estuaire_loire.csv", header = T)
Camargue <- read.csv2("Data/camargue.csv", header = T)
BA <- read.csv2("Data/Bassin_arcachon_limicoles.csv", header = T)
Cotentin <- read.csv2("Data/Baie_Cotentin.csv", header = T)
Aiguillon <- read.csv2("Data/Baie_aiguillon.csv", header = T)
Rhin <- read.csv2("Data/Reserve_du_rhin.csv", header = T)
Saint_Brieuc <- read.csv2("Data/Baie_saint_brieuc.csv", header = T)
Marais_orx <- read.csv2("Data/marais_d_orx.csv", header = T)

#Problème liés au noms 
colnames(Aiguillon) [11] <- "voie_migration" 
colnames(Cotentin) [9] <- "nb_annee_suivie"
colnames(Camargue) [24] <- "nb_annee_suivie"

# Fusion des tableaux (on rajoutera la camargue ultérieurement car déjà les colonnes liées à la taxonomie) 

data <- bind_rows(Loire, BA, Cotentin, Aiguillon, Rhin, Saint_Brieuc, Marais_orx)
data[,1] <- iconv(data[,1],from = "UTF-8",to = "ASCII//TRANSLIT")


#Fusionner avec le tableau espèce : 
espece <- read.csv("Data/espece.csv", header = T)
espece <- espece[-c(98),]

espece[,5] <- tolower(espece[,5])
espece[,5] <- gsub(" ","_",espece[,5])
espece[,5] <- gsub("'","_",espece[,5])
espece[,5] <- iconv(espece[,5],from = "UTF-8",to = "ASCII//TRANSLIT")
data <- merge(data, espece, by.x = "espece", by.y = "french_name", all.x = T)

#On peut rajouter le tableau de la Camargue : 

Camargue$X <- as.character(Camargue$X)
data <- bind_rows(data, Camargue)

# Rajouter famille et order pour les espèces indeterminées : 
unique(data$order_tax)
data[,1] <- gsub("bernache_du_pacifique","bernache_cravant_du_pacifique",data[,1])
data[,30] <- iconv(data[,30], from = "UTF-8", to = "ASCII//TRANSLIT")

sort(unique(data$espece))

data$order_tax[data$espece=="anatides_sp"] <- "Anseriformes"
data$order_tax[data$espece=="barges_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="becasseau_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="becasses"] <- "Charadriiformes"
data$order_tax[data$espece=="bernache_cravant_du_pacifique"] <- "Anseriformes"
data$order_tax[data$espece=="canard_sp"] <- "Anseriformes"
data$order_tax[data$espece=="chevalier_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="courlis_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="gravelot_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="harle_sp"] <- "Anseriformes"
data$order_tax[data$espece=="limicole_sp"] <- "Charadriiformes"
data$order_tax[data$espece=="macreuse_sp"] <- "Anseriformes"
data$order_tax[data$espece=="oie_sp"] <- "Anseriformes"
data$order_tax[data$espece=="sarcelle_sp"] <- "Anseriformes"
data$order_tax[data$espece=="sarcelle_a_ailes_vertes"] <- "Anseriformes"
data$order_tax[data$espece=="canard_des_bahamas"] <- "Anseriformes"
data$order_tax[data$espece=="canard_de_chiloe"] <- "Anseriformes"
data$order_tax[data$espece=="bernache_du_pacifique"] <- "Anseriformes"
data$order_tax[data$espece=="bernache_du_pacifique"] <- "Anseriformes"
data$order_tax[data$espece=="becasseau_rousset"] <- "Charadriiformes"
data$order_tax[data$espece=="oie_de_la_toundra"] <- "Anseriformes"
data$order_tax[data$espece=="hybride_tadorne_de_casarca_x_belon"] <- "Anseriformes"
data$order_tax[data$espece=="hybride_fuligule_milouin_x_morillon"] <- "Anseriformes"
data$order_tax[data$espece=="hybride_bernache_du_canada_x_oie_cendree"] <- "Anseriformes"
data$order_tax[data$espece=="harelde_de_miquelon"] <- "Anseriformes"
data$order_tax[data$espece=="fuligule_sp"] <- "Anseriformes"
data$order_tax[data$espece=="dendrocygne_fauve"] <- "Anseriformes"
data$order_tax[data$espece=="ouette_de_magellan"] <- "Anseriformes"
data$order_tax[data$espece=="eider_a_tete_grise"] <- "Anseriformes"
#On rajoute les familles : 
unique(data$family_tax)

data[,31] <- gsub("Anatidae","Anatides",data[,31])
data[,31] <- iconv(data[,31],from = "UTF-8",to = "ASCII//TRANSLIT")

data$family_tax[data$espece=="anatides_sp"] <- "Anatides"
data$family_tax[data$espece=="barges_sp"] <- "Scolopacides"
data$family_tax[data$espece=="becasseau_sp"] <- "Scolopacides"
data$family_tax[data$espece=="becasses"] <- "Scolopacides"
data$family_tax[data$espece=="canard_sp"] <- "Anatides"
data$family_tax[data$espece=="chevalier_sp"] <- "Scolopacides"
data$family_tax[data$espece=="courlis_sp"] <- "Scolopacides"
data$family_tax[data$espece=="gravelot_sp"] <- "Charadriides"
data$family_tax[data$espece=="harle_sp"] <- "Anatides"
data$family_tax[data$espece=="macreuse_sp"] <- "Anatides"
data$family_tax[data$espece=="oie_sp"] <- "Anatides"
data$family_tax[data$espece=="sarcelle_sp"] <- "Anatides"
data$family_tax[data$espece=="sarcelle_a_ailes_vertes"] <- "Anatides"
data$family_tax[data$espece=="canard_des_bahamas"] <- "Anatides"
data$family_tax[data$espece=="canard_de_chiloe"] <- "Anatides"
data$family_tax[data$espece=="bernache_du_pacifique"] <- "Anatides"
data$family_tax[data$espece=="becasseau_rousset"] <- "Scolopacides"
data$family_tax[data$espece=="oie_de_la_toundra"] <- "Anatides"
data$family_tax[data$espece=="hybride_tadorne_de_casarca_x_belon"] <- "Anatides"
data$family_tax[data$espece=="hybride_fuligule_milouin_x_morillon"] <- "Anatides"
data$family_tax[data$espece=="hybride_bernache_du_canada_x_oie_cendree"] <- "Anatides"
data$family_tax[data$espece=="harelde_de_miquelon"] <- "Anatides"
data$family_tax[data$espece=="fuligule_sp"] <- "Anatides"
data$family_tax[data$espece=="dendrocygne_fauve"] <- "Anatides"
data$family_tax[data$espece=="ouette_de_magellan"] <- "Anatides"
data$family_tax[data$espece=="eider_a_tete_grise"] <- "Anatides"

#On rajoute la classe : 
data$class_tax[data$espece=="anatides_sp"] <- "Oiseaux"
data$class_tax[data$espece=="barges_sp"] <- "Oiseaux"
data$class_tax[data$espece=="becasseau_sp"] <- "Oiseaux"
data$class_tax[data$espece=="becasses"] <- "Oiseaux"
data$class_tax[data$espece=="canard_sp"] <- "Oiseaux"
data$class_tax[data$espece=="chevalier_sp"] <- "Oiseaux"
data$class_tax[data$espece=="courlis_sp"] <- "Oiseaux"
data$class_tax[data$espece=="gravelot_sp"] <- "Oiseaux"
data$class_tax[data$espece=="harle_sp"] <- "Oiseaux"
data$class_tax[data$espece=="macreuse_sp"] <- "Oiseaux"
data$class_tax[data$espece=="oie_sp"] <- "Oiseaux"
data$class_tax[data$espece=="sarcelle_sp"] <- "Oiseaux"
data$class_tax[data$espece=="sarcelle_a_ailes_vertes"] <- "Oiseaux"
data$class_tax[data$espece=="canard_des_bahamas"] <- "Oiseaux"
data$class_tax[data$espece=="canard_de_chiloe"] <- "Oiseaux"
data$class_tax[data$espece=="bernache_du_pacifique"] <- "Oiseaux"
data$class_tax[data$espece=="becasseau_rousset"] <- "Oiseaux"
data$class_tax[data$espece=="oie_de_la_toundra"] <- "Oiseaux"
data$class_tax[data$espece=="hybride_tadorne_de_casarca_x_belon"] <- "Oiseaux"
data$class_tax[data$espece=="hybride_fuligule_milouin_x_morillon"] <- "Oiseaux"
data$class_tax[data$espece=="hybride_bernache_du_canada_x_oie_cendree"] <- "Oiseaux"
data$class_tax[data$espece=="harelde_de_miquelon"] <- "Oiseaux"
data$class_tax[data$espece=="fuligule_sp"] <- "Oiseaux"
data$class_tax[data$espece=="dendrocygne_fauve"] <- "Oiseaux"
data$class_tax[data$espece=="ouette_de_magellan"] <- "Oiseaux"
data$class_tax[data$espece=="eider_a_tete_grise"] <- "Oiseaux"


#Du chipotage (mais pour qu'un maximum d'informations soient renseignées dans le tableau)
data$pk_species[data$espece=="oie_de_la_toundra"] <- "ANSFABROS"
data$scientific_name[data$espece=="oie_de_la_toundra"] <- "Anser fabalis rossicus"
data$euring[data$espece=="oie_de_la_toundra"] <- "1574"
data$taxref[data$espece=="oie_de_la_toundra"] <- "2724"
data$english_name[data$espece=="oie_de_la_toundra"] <- "Russian Taiga Bean Goose"
data$niveau_taxo[data$espece=="oie_de_la_toundra"] <- "sous-espece"

data$scientific_name[data$espece=="dendrocygne_fauve"] <- "Dendrocygna_bicolor"
data$scientific_name[data$espece=="harelde_de_miquelon"] <-"Clangula hyemalis"
data$scientific_name[data$espece=="becasseau_rousset"] <- "Calidris_subruficollis"
data$scientific_name[data$espece=="bernache_du_pacifique"] <- "Branta_bernicla_nigricans"
data$scientific_name[data$espece=="canard_de_chiloe"] <- "Mareca_sibilatrix"
data$scientific_name[data$espece=="canard_des_bahamas"] <- "Anas_bahamensis"
data$scientific_name[data$espece=="sarcelle_a_ailes_vertes"] <- "Anas_carolinensis"
data$scientific_tax[data$espece=="ouette_de_magellan"] <- "Chloephaga_picta"
data$scientific_tax[data$espece=="eider_a_tete_grise"] <- "Somateria_spectabilis"

data$niveau_taxo[data$espece=="anatides_sp"] <- "famille"
data$niveau_taxo[data$espece=="chevalier_sp"] <- "famille"
data$niveau_taxo[data$espece=="becasses_sp"] <- "famille"
data$niveau_taxo[data$espece=="becasseau_sp"] <- "famille"
data$niveau_taxo[data$espece=="courlis_sp"] <- "genre"
data$niveau_taxo[data$espece=="barges_sp"] <- "genre"
data$niveau_taxo[data$espece=="gravelot_sp"] <- "genre"
data$niveau_taxo[data$espece=="macreuse_sp"] <- "genre"
data$niveau_taxo[data$espece=="harle_sp"] <- "famille"
data$niveau_taxo[data$espece=="oie_sp"] <- "famille"
data$niveau_taxo[data$espece=="sarcelle_sp"] <- "genre"
data$niveau_taxo[data$espece=="limicole_sp"] <- "ordre"
data$niveau_taxo[data$espece=="fuligule_sp"] <- "genre"
data$niveau_taxo[data$espece=="bernache_du_pacifique"] <- "sous-espece"
data$niveau_taxo[data$espece=="ouette_de_magellan"] <- "espece"
data$niveau_taxo[data$espece=="eider_a_tete_grise"] <- "espece"

data$english_name[data$espece=="anatides_sp"] <- "Anatidae_sp"
data$english_name[data$espece=="limicole_sp"] <- "Waders_sp"
data$english_name[data$espece=="barges_sp"] <- "godwit_sp"
data$english_name[data$espece=="canard_sp"] <- "duck_sp"
data$english_name[data$espece=="gravelot_sp"] <- "plover_sp"
data$english_name[data$espece=="macreuse_sp"] <- "scoter_sp"
data$english_name[data$espece=="oie_sp"] <- "goose_sp"
data$english_name[data$espece=="sarcelle_a_ailes_vertes"] <- "Green-winged Teal"
data$english_name[data$espece=="canard_des_bahamas"] <- "White-cheeked Pintail"
data$english_name[data$espece=="canard_de_chiloe"] <- "Chiloe Wigeon"
data$english_name[data$espece=="bernache_du_pacifique"] <- "Brant Goose (Nigricans)"
data$english_name[data$espece=="becasseau_rousset"] <- "Buff-breasted Sandpiper"
data$english_name[data$espece=="harelde_de_miquelon"] <- "Long-tailed Duck"
data$english_name[data$espece=="dendrocygne_fauve"] <- "Fulvous Whistling Duck"
data$english_name[data$espece=="ouette_de_magellan"] <- "Upland Goose"
data$english_name[data$espece=="eider_a_tete_grise"] <- "Kind Eider"

data$scientific_name[data$espece=="courlis_sp"] <- "Numenius_sp"
data$scientific_name[data$espece=="gravelot_sp"] <- "Charadrius_sp"
data$scientific_name[data$espece=="macreuse_sp"] <- "Melanitta_sp"
data$scientific_name[data$espece=="sarcelle_sp"] <- "Anas_sp"
data$scientific_name[data$espece=="fuligule_sp"] <- "Aythya_sp"
data$scientific_name[data$espece=="barges_sp"] <- "Limosa_sp"

#Rajouter la colonne pour les jours julien 
data$jour_julien <- yday(data$date)

#Rajouter une colonne pour les occurence des espèces dans chaque secteurs : 

occurence_sp <- 
  data %>% count(espece,secteur,abondance) %>% filter(abondance > 0)

occurence_sp <- occurence_sp %>% count(espece, secteur)

occurence_sp$id <- paste0(occurence_sp$espece,occurence_sp$secteur)

data$id_sect <- paste0(data$espece,data$secteur)

data <- merge(data, occurence_sp, by.x = "id_sect", by.y = "id")

setDT(data)
data[, c('espece.y','secteur.y','id_sect'):=NULL]
setDF(data)

colnames(data) [1] <- "espece"
colnames(data) [7] <- "secteur"
colnames(data) [35] <- "occurence_sp"

#Créer une colonne avec les jours juliens correspondant à la période d'hivernage 
data$jour_julien_hiver <- ifelse(data$mois<6, 
                                 yday(data$date)+yday(as.Date(paste0(data$annee-1,"-12-31")))-yday(as.Date(paste0(data$annee-1,"-05-31"))),
                                 yday(data$date)-yday(as.Date(paste0(data$annee,"-05-31"))))
#Création des colonnes années hiver et mois hiver : 
setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
data[,mois_hiver := ifelse(annee == annee_hiver, mois - 5, mois + 7)]
data[,mois_hiver_txt := as.character(mois_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

#Création d'une colonne site retenu :

nb_suivi_site <-  
  data %>% 
  count(site, annee_hiver)

nb_suivi_site <- 
  nb_suivi_site %>%
  count(site)

data <- merge(data,nb_suivi_site, by.x = "site", by.y = "site")

colnames(data)[41] <- "nb_saison_suivi"

data$site_retenu <- with(data, ifelse(data$nb_saison_suivi < 3 ,"non","oui"))

#Création colonne pour les outliers et faraway : 
#Outliers : 
#IQR => Q3-Q1 
# Q1 - 1.25*IQR et Q3 + 1.25*IQR
# Far Away 
# Q1 - 3*IQR et Q3 + 3*IQR

#Tableau des quantiles : 
Tab_quant <- data %>%
  group_by(espece,mois,secteur,annee) %>%
  summarise(Q1 = quantile(abondance, probs =  c(0.25)),
            Q2 = quantile(abondance, probs = c(0.50)),
            Q3 = quantile(abondance, probs = c(0.75)))

#Ajout des "bornes" inf et sup pour les outliers : 

#IQR : 
Tab_quant$IQR <- Tab_quant$Q3-Tab_quant$Q1

#Pour les outliers 
#Borne inf 
Tab_quant$Outlier_inf <-Tab_quant$Q1-1.25*(Tab_quant$IQR)

#Borne sup
Tab_quant$Outlier_sup <-Tab_quant$Q3+1.25*(Tab_quant$IQR)

#Pour les Faraway : 
#Borne inf : 
Tab_quant$Faraway_inf <-Tab_quant$Q1-3*(Tab_quant$IQR)

#Borne sup : 
Tab_quant$Faraway_sup <- Tab_quant$Q3+3*(Tab_quant$IQR)


#Création d'un identifiant pour merge les deux tableaux : 
data$id_Quantile <- paste0(data$espece, data$mois, data$secteur, data$annee)

Tab_quant$id_Quantile <- paste0(Tab_quant$espece, Tab_quant$mois, Tab_quant$secteur, Tab_quant$annee)

#Merge des deux tableaux : 

data <- merge(data, Tab_quant, by.x = "id_Quantile",by.y = "id_Quantile")

rm(list = c("Tab_quant"))

setDT(data)
data[, c('espece.y','secteur.y','mois.y','annee.y'):=NULL]
setDF(data)

colnames(data) <- gsub("espece.x","espece",colnames(data))
colnames(data) <- gsub("mois.x","mois",colnames(data))
colnames(data) <- gsub("secteur.x","secteur",colnames(data))
colnames(data) <- gsub("annee.x","annee",colnames(data))

#Vérifier que les abondances rentrent dans l'intervalle :
#Pour les outliers 

data$outlier_verif <- with(data, ifelse(data$abondance < data$Outlier_inf, "outlier",
                                        ifelse(data$abondance > data$Outlier_sup, "outlier","clean")))        


data$faraway_verif <- with(data, ifelse(data$abondance < data$Faraway_inf, "faraway",
                                        ifelse(data$abondance > data$Faraway_sup, "faraway","clean")))

table(data$outlier_verif)

table(data$faraway_verif)
# -> 42 012 faraway & 487 980 clean 

#Tableau de contingence : 
table <- table(data$qualite_comptage, data$outlier_verif)
table
#Proportion de outlier qui correspond véritablement à un comptage douteux : 


table <- table(data$qualite_comptage, data$faraway_verif)
table

#Retirer les données d'outlier et faraway : 
data_clean <- subset(data, !(data$outlier_verif=="outlier" | data$faraway_verif=="faraway"))
data_clean <- subset(data_clean, !(data_clean$site==""))

#Rajouter en tant que site non retenu les sites agrégés du rhin et Baracon (focus sur les espèces gibier) + RNN de Beauguillot 

data_clean$site_retenu[data_clean$site=="baracon"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn_beauguillot"] <- "non"
data_clean$site_retenu[data_clean$site=="vieuxrhin_grandcanalalsace"] <- "non"
data_clean$site_retenu[data_clean$site=="secteursud_rhinland_basenautique"] <- "non"
data_clean$site_retenu[data_clean$site=="amontbarrage_avalbarrage"] <- "non"
data_clean$site_retenu[data_clean$site=="marais_d_orx_(generique)"] <- "non"
data_clean$site_retenu[data_clean$site=="reserve_naturelle_nationale_du_marais_d_orx/4010/casier_burret_(roe)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(1)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(2)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(3)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(4)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(6)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(7)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(10)"] <- "non"
data_clean$site_retenu[data_clean$site=="rnn123_01_(11)"] <- "non"

unique(Orx$site)

#Enregistrement des jeux de données : 
write.csv2(data, "Data/data_clean.csv") #Données sans les outliers 


write.csv2(data, "Data/data.csv")


                                        






