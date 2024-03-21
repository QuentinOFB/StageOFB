setwd("C:/Users/quentin.petit/Documents/Git/StageOFB")

library(lubridate)

# Ouverture du jeu de données:
data <- read.csv("Data/Comptage_estuaire_2004_2024.csv", header = T, fileEncoding = "utf-8", sep = ";")
View(data)
str(data)

### 1. Régler les problèmes liés au noms :

#  Noms colonnes (pas de point, ni d'espace, ni les accents)
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

data[,1] <- gsub("13/06/22","13/06/2022",data[,1]) # [RL] un peu barbar et fastidieux mais efficace. la fonction mdy() de lubridate fait ça tout seul
data[,1] <- gsub("10/11/22","10/11/2022",data[,1])
data[,1] <- gsub("20/01/23","20/01/2023",data[,1])
data[,1] <- gsub("17/04/23","17/04/2023",data[,1])
data[,1] <- gsub("18/04/23","18/04/2023",data[,1])
data[,1] <- gsub("14/09/23","14/09/2023",data[,1])
data[,1] <- gsub("12/10/23","12/10/2023",data[,1])
data[,1] <- gsub("12/12/23","12/12/2023",data[,1])
data[,1] <- gsub("10/01/24","10/01/2024",data[,1])
data[,1] <- gsub("11/01/24","11/01/2024",data[,1])


#  Nom des secteurs :

unique(data$secteur)
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

#  Nom du site : (enlever les majuscules)
unique(data$site)
data[,4] <- tolower(data[,4])

#Remplacement des cases vides par le nom du site :
data$site[data$site == ""] <- "estuaire"

# Uniformiser les noms des observateurs : ## Compliqué d'unifier les noms (des fois il y'a les initiales du prénom, des fois pas, plusieurs cas de figures)
data[,8] <- tolower(data[,8])
data[,8] <- gsub("\\.","", data[,8])
data[,8] <- gsub("é","e",data[,8]) 
data[,8] <- gsub("à","a",data[,8])
data[,8] <- gsub(" ","",data[,8])
data[,8] <-iconv(data[,8], from = 'UTF-8', to = 'ASCII//TRANSLIT')
unique(data$compteur)

#### PARTIE 2 : 

### Compiler le tableau "espece" et jeu de données + sélectionner les taxons d'intérêt :

  # Ajouter les noms latins au jeu de données :

espece <- read.csv("Data/espece.csv")
View(espece)

help("merge")

espece[,5] <- tolower(espece[,5])
espece[,5] <- gsub(" ","_",espece[,5])
espece[,5] <- gsub("\\.","_",espece[,5])
espece[,5] <- gsub("é","e",espece[,5])
espece[,5] <- gsub("à","a",espece[,5])
espece[,5] <- gsub("'","_",espece[,5])
espece[,5] <-iconv(espece[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Retirer la ligne de la sous-espèce de la bernache cravant (sinon ça dédouble les données pour cette espèce)

espece <- espece[-c(98),]

#Fusion des deux jeux de données :

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


# Création table site : 
# [ Quentin ] : Soucis dans la création des tables : la table site et la table inventaire n'ont pas le même de lignes. 
# Alors qu'en théorie, je suppose, qu'elles devraient avoir le même nombre, puisque les ID sont les même. Sauf que lorsqu'on rajoute les observateurs
# ou d'autres colonnes ca rajoute des lignes.
# J'ai essayé d'uniformiser les noms des observateurs, mais il y a trop de différence dans la façon dont ils ont été entrés. 


  # Création d'un ID qui va permettre de compiler les trois tables : 
ID <- paste(data$secteur,data$date)

site <- data.frame(ID,data$site,data$secteur)
site <- unique(site)
unique(site$ID)
# Création table inventaire : 

date_jj <- yday(data$date)
mois <- month(data$date)
annee <- year(data$date)

inv <- data.frame(ID,data$date)
inv <- unique(inv)

# Compilation des deux tables : 

data_inv <- merge(site,inv, by.x = "ID",by.y = "ID")


  # Tentative d'ajout des lignes espèces manquantes dans le jeu de données :

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

# ATTENTION Rajoute 10 000 lignes... 
# Problème en cours de résolution... 
## Pour poursuivre

## Voir pour les doubles comptages

                                        # [RL] conseils :
                                        # [RL] 1- créer une table site (id, nom, caratéristique,...)
                                        # [RL] 2- créer une table inventaire (id, id_site,date, annee, mois,jour_julien,observateur,...)
                                        # [RL] 3- tu as ta table observation (id_inventaire, espece, abondance)




