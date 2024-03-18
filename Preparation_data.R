setwd("C:/Users/quentin.petit/Documents/Git/StageOFB") 

########### Ouverture du jeu de données: 
data <- read.csv("Data/Comptage_estuaire_2004_2024.csv", header = T, fileEncoding = "utf-8", sep = ";")
View(data)
str(data)

# Changement noms colonnes (pas de point, ni d'espace, ni les accents)
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ","_",colnames(data))
colnames(data) <- gsub("\\.","_", colnames(data))
colnames(data) <-iconv(colnames(data), from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Vérification noms des espèces: 
data[,6] <- tolower(data[,6])
data[,6] <- gsub(" ","_",data[,6])
data[,6] <- gsub("\\.","", data[,6])
data[,6] <- gsub("é","e",data[,6])
data[,6] <- gsub("à","a",data[,6])
data[,6] <- gsub("'","_",data[,6])
data[,6] <-iconv(data[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')

unique(data$espece)

# -> Remplacer chevalier combattant par combattant varié : 

data[,6] <- gsub("chevalier_combattant","combattant_varie",data[,6])
data[,6] <- gsub("bernache_nonette","bernache_nonnette", data[,6])
data[,6] <- gsub("tournepierre","tournepierre_a_collier", data[,6])

#Vérification foromes des dates: regarder les formats (JJ/MM/YYYY) 
class(data$date)
unique(data$date)

data[,1] <- gsub("13/06/22","13/06/2022",data[,1])
data[,1] <- gsub("10/11/22","10/11/2022",data[,1])
data[,1] <- gsub("20/01/23","20/01/2023",data[,1])                                  
data[,1] <- gsub("17/04/23","17/04/2023",data[,1])                    
data[,1] <- gsub("18/04/23","18/04/2023",data[,1])                   
data[,1] <- gsub("14/09/23","14/09/2023",data[,1])                 
data[,1] <- gsub("12/10/23","12/10/2023",data[,1])
data[,1] <- gsub("12/12/23","12/12/2023",data[,1])
data[,1] <- gsub("10/01/24","10/01/2024",data[,1])                 
data[,1] <- gsub("11/01/24","11/01/2024",data[,1])

# Mettre au format DATE : 

data$date <- as.Date(data$date, format = "%d/%m/%Y")
class(data$date)
## Format date : YYYY - MM - JJ 
data$Date <- format(data$date,"%d/%m/%Y") #Ca renvoie au format "character" 

# Vérification nom des sites :

unique(data$secteur)
data[,5] <- tolower(data[,5])
data[,5] <- gsub(" ","_",data[,5])
data[,5] <- gsub("-","_", data[,5])
data[,5] <- gsub("/","", data[,5])
data[,5] <- gsub("é","e",data[,5])
data[,5] <-iconv(data[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

########### Problèmes avec certains noms : 
 
data[,5] <- gsub("paimboeuf___corsept","paimboeuf_corsept",data[,5])
data[,5] <- gsub("paimboeuf_corsept_","paimboeuf_corsept",data[,5])
data[,5] <- gsub("saint_brevin__mean","saint_brevin_mean",data[,5])


######################### AJOUTER LES LIGNES AVEC LES ESPECES MANQUANTES : 

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
 
# Tentative d'ajout des espèces manquantes dans le jeu de données : 
######### En phase de test : 
# Création d'un identifiant pour "fusionner les deux tableaux" 

data$ID <- paste(data$secteur,data$date,data$espece)
View(data)
#Création tableau "inventaire" à croiser avec le jeu de données :

ID2 <- paste(data$secteur,data$date)
new_data <- data.frame(ID2, data$espece)
View(new_data)

data <- merge(data,new_data, by.x = "ID", by.y = "ID2")
View(data)
# Trouver autre chose, car ça ne fonctionne pas. 

# 1 Création d'un ID 
secteur <- unique(data$secteur)
date <- unique(data$date)
ID_1 <- paste(secteur,date)
str(ID_1)
View(ID_1)

sp <- unique(data$espece)
new_tab <- (ID_1,sp)
help("data.frame")


new_tab <- data.frame(ID_1,data$espece)
new_tab$ID_esp <- paste(ID_1,data$espece)
View(new_tab)
View(ID_1)


data_F <- merge(new_tab,data, by.x = "ID_esp", by.y = "ID", all.x = TRUE)
View(data_F)
table(data_F$effectif)
