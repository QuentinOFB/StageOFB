library(dplyr)

#Ouverture du jeu de données : 
data <- read.csv2("Data/data_clean_nonagglo.csv", header = T)

#Réunir les bernaches : 
sort(unique(data$espece))
data[,4] <- gsub("bernache_cravant_du_pacifique","bernache_cravant",data[,4])
data[,4] <- gsub("bernache_cravant_occidentale","bernache_cravant",data[,4])
data[,4] <- gsub("oie_de_la_toundra","oie_des_moissons",data[,4])

data <- subset(data, annee_hiver > 2003)
#Tri des espèces : 
liste <- read.csv("Data/liste_sp.csv",header = T, sep = ";")
data <- merge(data,liste, by.x = "espece", by.y = "espece")
data <- subset(data, data$tri=="Oui") 

tabQuant <- data %>% group_by(espece,site,secteur,annee_hiver) %>% 
  summarise(Q1 = quantile(abondance, probs =  c(0.25)),
           Q2 = quantile(abondance, probs = c(0.50)),
            Q3 = quantile(abondance, probs = c(0.75)))

  