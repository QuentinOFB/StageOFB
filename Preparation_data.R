setwd("C:/Users/quentin.petit/Documents/Git/StageOFB")

library(lubridate)
library(plyr)
library(dplyr)

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

      ######### 1. Donnees estuaire : ###########

colnames(data)[5] <- "site"
colnames(data) [4] <- "secteur"
colnames(data) [6] <- "espece"
colnames(data) [7] <- "abondance"
colnames(data) [8] <- "observateur" 

# -> Noms de colonnes (pas de point, pas d'espace, pas d'accents) : 
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ","_",colnames(data))
colnames(data) <- gsub("\\.","_", colnames(data))
colnames(data) <-iconv(colnames(data), from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Noms des espèces:
data[,6] <- tolower(data[,6])
data[,6] <- gsub(" ","_",data[,6])
data[,6] <- gsub("\\.","", data[,6])
data[,6] <- gsub("é","e",data[,6]) # [RL] très bien, normalement la fonction iconv() permet de faire ça mais en tout cas c'est important de ne plus avoir d'accent.
data[,6] <- gsub("à","a",data[,6])
data[,6] <- gsub("'","_",data[,6])
data[,6] <-iconv(data[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')

unique(data$espece)

# -> Remplacer chevalier combattant par combattant varié :

data[,6] <- gsub("chevalier_combattant","combattant_varie",data[,6])
data[,6] <- gsub("bernache_nonette","bernache_nonnette", data[,6])
data[,6] <- gsub("tournepierre","tournepierre_a_collier", data[,6])

# Forme des dates: regarder les formats (JJ/MM/YYYY)
class(data$date)
unique(data$date)

data$date <- dmy(data$date)
# Enlever les lignes qui ne continnent pas des dates mais : "comptage annulé + date) 
data <- data[-c(43249,56643,56644,66910,66911,75111,75112,67651),]


#  Nom des sites :

unique(data$site)
data[,5] <- tolower(data[,5])
data[,5] <- gsub(" ","_",data[,5])
data[,5] <- gsub("-","_", data[,5])
data[,5] <- gsub("/","", data[,5])
data[,5] <- gsub("é","e",data[,5])
data[,5] <-iconv(data[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Problèmes avec certains noms :

data[,5] <- gsub("paimboeuf___corsept","paimboeuf_corsept",data[,5])
data[,5] <- gsub("paimboeuf_corsept_","paimboeuf_corsept",data[,5])
data[,5] <- gsub("saint_brevin__mean","saint_brevin_mean",data[,5])

#  Nom du secteur : (enlever les majuscules)
unique(data$secteur)
data[,4] <- tolower(data[,4])

#Remplacement des cases vides par le nom du site :
data$site[data$secteur == ""] <- "estuaire"

# Uniformiser les noms des observateurs : ## Compliqué d'unifier les noms (des fois il y'a les initiales du prénom, des fois pas, plusieurs cas de figures)
data[,8] <- tolower(data[,8])
data[,8] <-iconv(data[,8], from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(data$observateur)

data[,8] <- gsub("guenezan m ","guenezan m",data[,8])
data[,8] <- gsub(" guenezan m","guenezan m",data[,8])

# Vérifier après... 
data[16379,5] <- gsub("corsept","grand_bilho",data[16379,5])
data[27248,8] <- gsub("","cochard, touze",data[27248,8])
data[30820,8] <- gsub("latraube, frelon","becot, latraube, drouyer",data[30820,8])
data[64699,1] <- gsub("2017-12-15","2017-11-15",data[64699,1])

#Enlever les comptages partiels : 
data[,9] <- tolower(data[,9])

data <- subset(data, !(data$remarques=="comptage partiel"|data$remarques=="compatge partiel, manque partie est de saint nicaols"
                       |data$remarques == "houle, les compteurs ont dénombrés depuis le bateau donc comptage partiel"|data$remarques==" comptage partiel"))

# Enlever les NC (non compté) à cause d'une remorque cassée, ou annulé pour cause de tir d'ibis la veille du comptage : 

data <- subset(data, !(data$abondance=="NC"|data$abondance=="nc"|data$abondance=="Nc"|data$abondance=="non compté"))

# Enlever les comptages avec du dérangements + pollution : 

data <- subset(data, !(data$remarques=="pollution hydrocarbures du 16/03/08 (180tonnes) + hélicot survol"|data$remarques=="dérangement : kite surf et chien non tenu en laisse"))
unique(data$remarques) 

 # Beaucoup de dérangement le 20/08/2021 à St Brévin : 

data <- subset(data, !(data$date=="2021-08-20" & data$site =="saint_brevin"))

#Enlever le "comptage trop tardif par rapport à la marrée" : 

data <- subset(data, !(data$remarques=="comptage trop tardif par rapport à la marée"))
                         
# Migron : comptage uniquement sur le bras de Migron (donc partiel) + fait à marée basse : 
 data <- subset(data, !(data$date=="2017-09-19" & data$site=="migron"))

# Supprimer les données agrégées entre Imperlay et Saint-Nicolas (une autre option serait d'agrégér Imperlay/St Nicolas pour toutes les dates ?)
 
data <- subset(data, !(data$date=="2017-01-10" & data$site=="imperlay"|data$date=="2017-01-10" & data$site=="saint_nicolas")) 
data <- subset(data, !(data$date=="2017-02-09" & data$site=="imperlay"|data$date=="2017-02-09" & data$site=="saint_nicolas")) 
data <- subset(data, !(data$date=="2022-01-20" & data$site=="imperlay"|data$date=="2022-02-20" & data$site=="saint_nicolas")) 

#Les erreurs d'entrée de données qui bloquent la création des tables inv et sites : 

# Lié au nom d'observateur (avec des espaces avant et après)
data[,8] <- gsub("guenezan m ","guenezan m",data[,8])
data[,8] <- gsub(" guenezan m","guenezan m",data[,8])

  #Erreur d'entrée de date : 2017-12-15 au lieu de 2017-11-15
data[57093,1] <- gsub("2017-12-15","2017-11-15",data[57093,1])

# Enlever les noms de sites absents : 
data <- subset(data, !(data$site==""))

#Je serais d'avis d'enlever le site "Estuaire" car personne ne sait à quoi il correspond sur la carte des sites. 
# De plus, j'enlèverai aussi le site Baracon : c'est une réserve de chasse où les comptages sont centrés sur les espèces gibiers

data <- subset(data, !(data$site=="baracon"|data$site=="estuaire"))

# Ne conserver que les mois où les hivernants sont présents ? (oct-nov-dec-janv-fev-mars)
# + septembre et avril pour se laisser une marge ? 

unique(data$mois)
data$mois <- month(data$date)

data <- subset(data, !(data$mois=="5"|data$mois=="6"|data$mois=="7"|data$mois=="8"))

unique(data$annee)
data$annee <- year(data$date)
#Retirer les sites qui sont sur moins de 3 saisons : Lavau, Chevalier et Saint_brévin/méan : 

data <- subset(data, !(data$site=="lavau"|data$site=="chevallier"|data$site=="saint_brevin_mean"))

# Enlever le comptage du 10/11/2008 : X2 passage sur tous les sites, avec des conditions météo nulles
# + comptage partiel -> remplacé par les comptages du 12/11 et du 24/11
# (déjà enlever précédement avec les comptages partiels)

# Double comptage Corsept 18/07/2008 -> erreur de saisi des données, c'est un doublon !
# Voir pour les doubles comptages avec Matthieu Bécot sur Pierre Rouge : 

##Selectionner les espèces limicoles et anatidés et enlever les autres : 
espece <- read.csv("Data/espece.csv")
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
data_esp <- merge(data,espece, by.x = "espece", by.y = "french_name")
View(data_esp)
unique(data_esp$espece)

# Selectionnner les espèces qui nous intéressent : Anatidés + Limicoles
# -> Choix des Ansériformes + Charadriiformes

data <- subset(data_esp, data_esp$order_tax == "Ansériformes"|data_esp$order_tax == "Charadriiformes")
View(data)
unique(data$espece)

# Dégager les laridés + la sterne naine (Sternidés) :

data <- subset(data, !(data$family_tax=="Laridés"|data$family_tax=="Sternidés"))
unique(data$espece)
# -> 53 espèces anatidés/limicoles recensées sur tous les comptages estuaire Loire
## Retrier les colonnes qui ne servent pas à grand chose : 

data <- data[,-c(15,16,17,19,20:28)]

  #Jeux de données nettoyé (si rien n'a été oublié) pour estuaire de la Loire ! 


      ######## 2. La Camargue #############






#### PARTIE 2 : 

# Création d'un ID qui va permettre de compiler les trois tables : 
ID <- paste(data$site,data$date)

site <- data.frame(ID,data$site,data$secteur)
site <- unique(site)

# Création table inventaire : 

date_jj <- yday(data$date)
mois <- month(data$date)
annee <- year(data$date)

inv <- data.frame(ID,data$date, date_jj,mois,annee)
inv <- unique(inv)

#Partie observateurs : Problème du double comptage pour St-Brévin 2008-07-18 (ligne 16793 et 17471) 
# supposition : comptage dans la même journée, ils ont refait un comptage pour avoir de la donnée car pas d'observation pour le 1er comptage
# Décider quel jour on garde ? 
obs <- data.frame(ID,data$observateur)
obs <- unique(obs)
                        # Compiler observateurs et inv :
duplicated(obs$ID)
duplicated(obs$ID[1000:2009])
sum(duplicated(obs$ID))
View(obs[1000:2009,])

# Compilation des deux tables : Table invXsite : 
data_inv <- merge(site,inv, by.x = "ID",by.y = "ID")


### Compiler le tableau "espece" et jeu de données + sélectionner les taxons d'intérêt :


  # Tentative d'ajout des lignes espèces manquantes dans le jeu de données : (faire la table d'observation)

# Création d'un ID dans data pour pouvoir ensuite "fusionner" les deux tableaux :
data$ID <- paste(data$secteur,data$date,data$espece)

# Création du tableau "inventaire" à croiser avec le jeu de données Data :

data$ID <- paste(data$secteur,data$date)

ID <- unique(data$ID)
sp <- unique(data$espece)

# Construction inventaire (pour chaque ID = la liste des 53 espèces anatidés limicoles)
inventaire <- expand.grid(ID, sp) # [RL] très bien
View(inventaire)

# Création d'un ID dans inventaire prenant en compte les espèces pour le combiner ensuite avec un ID
# dans les data

inventaire$ID_Sp <- paste(inventaire$Var1,inventaire$Var2)

# Création de l'ID espece dans le jeu de données :

data$ID_Sp <- paste(data$ID,data$espece)

# Combinaison des deux tableaux :
data <- merge(inventaire, data, by.x = "ID_Sp", by.y = "ID_Sp", all.x = T)
View(data)

# Utilisation de cette fonction pour séparer la date et les secteurs dans l'ID :

library(tidyr)
#data <- separate(data_F, col = "Var1",into = c("secteur_v","date_v"),sep = " ",remove =T)
#help("separate")

# Faire en sorte que le site "estuaire" soit renseigné dans toute la colonne :
data$site <- replace_na(data$site,"estuaire")

# Remplacement des NA par des 0
data$effectif[is.na(data$effectif)] = 0
View(data)

# On peut maintenant retirer les "anciennes" colonnes pour date, secteur et espece et ID 
  # pour obtenir la table observation : 
data <- data[,-c(1,4,5,6,7,8,9,11:34)]

# Var 2 -> espece :
colnames(data)[names(data)== "Var2"] <- "espece"
colnames(data)[names(data)== "Var1"] <- "id"

# On remet les noms latins + famille + ordre

data <- merge(data,espece, by.x = "espece", by.y = "french_name")
data <- data[,-c(4,6,7:15)]

# Combinaison de tous les table : 

data_fin <- merge(data,data_inv,by.x="id",by.y="ID")
unique(data_fin$espece)


## Voir pour les doubles comptages

                                        # [RL] conseils :
                                        # [RL] 1- créer une table site (id, nom, caratéristique,...)
                                        # [RL] 2- créer une table inventaire (id, id_site,date, annee, mois,jour_julien,observateur,...)
                                        # [RL] 3- tu as ta table observation (id_inventaire, espece, abondance)




#################### Les autres jeu de données : 
# 2. Donnees Camargue : 
colnames(Camargue) <- tolower(colnames(Camargue))
colnames(Camargue) <-iconv(colnames(Camargue), from = 'UTF-8', to = 'ASCII//TRANSLIT')

colnames(Camargue) [4] <- "site"
colnames(Camargue) [5] <- "mois"
colnames(Camargue) [6] <- "annee"
colnames(Camargue) [8] <- "espece_lat"
colnames(Camargue) [9] <- "abondance"
colnames(Camargue) [10] <- "observateur" 

# -> Attention ! Pour les noms d'observateur uniquement les initiales 

unique(Camargue$espece_lat)

# Ajouter les noms vernaculaires aux données camargue : 

espece <- read.csv("Data/espece.csv")
espece[,3] <- gsub(" ","_",espece[,3])

# Modifier le nom latin du cygne de Bewick dans les données, sinon pas de correspondance : 

Camargue[,8] <- gsub("Cygnus_columbianus_bewickii","Cygnus_columbianus",Camargue[,8])
Camargue <- merge(Camargue,espece, by.x = "espece_lat", by.y = "scientific_name_2")
Camargue <- Camargue[,-c(1,14,16,18:27)]

colnames(Camargue)[13] <- "espece_lat"
colnames(Camargue)[14] <- "espece" 

# Rajouter une colonne avec le nom du secteur : 

Camargue$secteur <- "camargue"
Camargue$remarques <- "NA"

# 3. Donnees Aiguillon : 

colnames(Baie) [3] <- "espece"
colnames(Baie) [5] <- "site"
colnames(Baie) [6] <- "observateur"
colnames(Baie) [8] <- "date" 
colnames(Baie) [12] <- "abondance"
colnames(Baie) [18] <- "mois" 
colnames(Baie) [19] <- "annee" 

# Rajouter une colonne avec le nom du secteur : 
Baie$secteur <- "baie_aiguillon"

# 4. Donnees Cotentin : 

colnames(Cotentin) [11] <- "espece"
colnames(Cotentin) [13] <- "observateur"
colnames(Cotentin) [20] <- "abondance"
colnames(Cotentin) [15] <- "date_1" 
colnames(Cotentin) [16] <- "date"
colnames(Cotentin) [18] <- "site" 
colnames(Cotentin) [25] <- "remarques" 

Cotentin$secteur <- "cotentin"
Cotentin <- Cotentin[-c(1),]
Cotentin$date <- ymd(Cotentin$date)

# Ajout des tableaux : 

data <- rbind.fill(data,Camargue,Cotentin,Baie)
unique(data$secteur)


