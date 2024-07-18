library(dplyr)

#Ouverture du jeu de données : 
data <- read.csv2("Data/data_clean_nonagglo.csv", header = T)

data$annee_hiver_txt <- as.character(data$annee_hiver_txt)
data$mois_hiver_txt <- as.character(data$mois_hiver_txt)

#Ne sélectionner que la saison d'hivernage 
unique(data$saison)
data <- subset(data, saison=="hivernage" & site_retenu=="oui")

#Valeur médiane du mois hiver (+ecart-type)
Tab1 <- data %>% group_by(site,annee_hiver_txt, secteur) %>% 
  summarise(mois_median = median(mois_hiver),
            mois_sd = sd(mois_hiver), nombre_mois = length(unique(mois_hiver_txt)))
  #Créer une colonne : 
  Tab1$site_retenu_pheno <- with(Tab1, ifelse(nombre_mois<5,"non","oui"))
  boxplot(mois_median~annee_hiver_txt, data = subset(Tab1, secteur == "estuaire" & site_retenu_pheno=="oui"))


#calculer du nombre moyen de mois par année hiver et site pour chaque secteur :
dt <- data.frame(data$site,data$mois_hiver_txt,data$annee_hiver_txt,data$secteur)
dt2 <- unique(dt)
colnames(dt2) <- gsub("data.","", colnames(dt2))

tab <- dt2 %>% group_by(site, annee_hiver_txt, secteur) %>%
  count(mois_hiver_txt)

tab2 <- tab %>% group_by(site, annee_hiver_txt, secteur) %>%
  count(n)
colnames(tab2)[5] <- "nombre_mois"
  
tab3 <- tab2 %>% group_by(secteur, annee_hiver_txt) %>% 
  summarise(moy = mean(nombre_mois),
            sd = sd(nombre_mois))

#Avec les mois numériques 
tab5 <- data.frame(data$site,data$mois_hiver,data$annee_hiver_txt,data$secteur)
tab5 <- unique(tab5)

colnames(tab5) <- gsub("data.","", colnames(tab5))
tab6 <- subset(tab5, secteur == "estuaire")
boxplot(mois_hiver~annee_hiver_txt, data = tab6)

tab4 <- data %>% group_by(secteur, annee_hiver_txt) %>% 
  summarise(moy = mean(mois_hiver),
            sd = sd(mois_hiver))


dt_est <- subset(tab4, secteur == "estuaire")
boxplot(moy~annee_hiver_txt, dt_est)

#Ordonner les jours juliens : 
data <- data %>% arrange(jour_julien_hiver, espece,site,secteur,annee_hiver)

#Calcul des effectifs cumulés & abondance totale : 
data <- data %>% group_by(espece,site,secteur,annee_hiver) %>%
  mutate(cum_sum = cumsum(abondance), abondance_totale = sum(abondance))

data$prop_cumsum <- data$cum_sum/data$abondance_totale

#Retirer les données avec abondance totalte à 0 
data2 <- subset(data, !(abondance_totale==0))


data2$mediane_pourcentage <- 0.5 * data2$abondance_tot
mediane_date <- data2 %>% group_by(espece,site,secteur,annee_hiver) %>% 
  filter(cum_sum >= mediane_pourcentage) %>% slice(1) %>% pull(jour_julien_hiver)

#Boucle : 
out_init <- FALSE

#Obtenir la liste des espèces : 
vecsp <- unique(data2$espece)

for (isp in 1:length(vecsp)) {
 sp <- vecsp[isp] 

 data_test1 <- subset(data2, espece==sp)
 vecsite <- unique(data_test1$site)
 
 for (ist in 1:length(vecsite)) {
   st <- vecsite[ist]
   
   data_test2 <- subset(data_test1, site==st)
   vecan <- unique(data_test2$annee_hiver_txt)
   
   sct <- unique(data_test2$secteur)
   
   for (ianv in 1:length(vecan)) {
    anvtxt <- vecan[ianv]

    data_test3 <- subset(data_test2, annee_hiver_txt== anvtxt)
    
    data_test3$mediane_pourcentage <- 0.5 * data_test3$abondance_tot
    mediane_date <- data_test3 %>% 
      filter(cum_sum >= mediane_pourcentage) %>% slice(1) %>% pull(jour_julien_hiver) 
    
    #Transformer le résultats en data.frame
    tab_pheno <- as.data.frame(mediane_date)
    
    #sélectionner les trois premières lignes 
    tab_pheno <- tab_pheno[c(1),]
   tab_pheno <- as.data.frame(tab_pheno)
              
    #Ajouter l'espèce 
    tab_pheno$sp <- sp
    
    #Ajouter le site 
    tab_pheno$site <- st
    
    #Ajouter l'année hiver :
    tab_pheno$annee_hiver <- anvtxt
    
    #Ajouter le secteur : 
    tab_pheno$secteur <- sct 
    
if (!out_init) { 
  
  data_pheno <- tab_pheno
  out_init <- TRUE 

  } else {
    data_pheno <- rbind(data_pheno,tab_pheno)
   }
   }}}   




  
 

