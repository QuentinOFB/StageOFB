setwd("C:/Users/quentin.petit/Documents/Git/StageOFB") 

# ouverture du jeu de données 
data <- read.csv("Data/Comptage_estuaire_2004_2024.csv", header = T, fileEncoding = "utf-8", sep = ";")
View(data)
str(data)

# Changement noms colonnes (pas de point, ni d'espace, ni les accents)
colnames(data) <- tolower(colnames(data))
colnames(data) <- gsub(" ","_",colnames(data))
colnames(data) <- gsub("\\.","_", colnames(data))
colnames(data) <-iconv(colnames(data), from = 'UTF-8', to = 'ASCII//TRANSLIT')

#Vérification espèces: 
data[,6] <- tolower(data[,6])
data[,6] <- gsub(" ","_",data[,6])
data[,6] <- gsub("\\.","", data[,6])
data[,6] <- gsub("é","e",data[,6])
data[,6] <- gsub("à","a",data[,6])
data[,6] <- gsub("'","_",data[,6])
data[,6] <-iconv(data[,6], from = 'UTF-8', to = 'ASCII//TRANSLIT')

unique(data$espece)

########### -> Remplacer chevalier combattant par combattant varié : 

data[,6] <- gsub("chevalier_combattant","combattant_varie",data[,6])


#Vérification dates: regarder les formats (JJ/MM/YYYY) 
class(data$Date)
unique(data$Date)

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

######### Mettre au format DATE : 

data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
class(data$Date)
data$Date <- format(data$Date,"%d/%m/%Y") #Ca renvoie au format "character" 


# Vérification nom des sites :

unique(data$secteur)
data[,5] <- tolower(data[,5])
data[,5] <- gsub(" ","_",data[,5])
data[,5] <- gsub("-","_", data[,5])
data[,5] <- gsub("/","", data[,5])
data[,5] <- gsub("é","e",data[,5])
data[,5] <-iconv(data[,5], from = 'UTF-8', to = 'ASCII//TRANSLIT')

########### Pb noms : 
 
data[,5] <- gsub("paimboeuf___corsept","paimboeuf_corsept",data[,5])
data[,5] <- gsub("paimboeuf_corsept_","paimboeuf_corsept",data[,5])
data[,5] <- gsub("saint_brevin__mean","saint_brevin_mean",data[,5])


#Porblème des 0 : créer table de tous les inventaires unique(un site,date) 
unique(site,date)
ID <- paste(site,date) 
unique(data$espece)