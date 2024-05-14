library(ggplot2)
library(lubridate)
library(dplyr)
library(data.table)
library(glmmTMB)
library(glm.predict)
#Ouverture du jeu de données : 
data <- read.csv2("Data/data.csv", header = T)

#Travailler sur des données à partir de 2004 : 

data <- subset(data, !(data$annee<2004))

#Création jour julien :     

data$jour_julien_hiver <- ifelse(data$mois<6, 
                                 yday(data$date)+yday(as.Date(paste0(data$annee-1,"-12-31")))-yday(as.Date(paste0(data$annee-1,"-05-31"))),
                                 yday(data$date)-yday(as.Date(paste0(data$annee,"-05-31"))))

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
# -> 478 898 clean & 51 094 Outliers 

table(data$faraway_verif)
# -> 42 012 faraway & 487 980 clean 

#Tableau de contingence : 
table <- table(data$qualite_comptage, data$outlier_verif)
table
#Proportion de outlier qui correspond véritablement à un comptage douteux : 
  # 27% de outlier qui sont des comptages douteux 
  # 37% de clean qui sont des comptages douteux 

table <- table(data$qualite_comptage, data$faraway_verif)
table

#Retirer les données d'outlier et faraway : 
data <- subset(data, !(data$outlier_verif=="outlier" | data$faraway_verif=="faraway"))

write.csv2(data, "Data/Data_clean.csv")
rm(list =c("data"))

data <- read.csv2("Data/Data_clean.csv", header = T)

#Ajout colonnes année hiver + mois hiver 
setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
data[,mois_hiver := ifelse(annee == annee_hiver, mois - 5, mois + 7)]
data[,mois_hiver_txt := as.character(mois_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

#Tableau summary (espèces, mois et secteurs et année) -> médiane, Q1 et Q3 
#Pour la création des différentes figures 
#selection : (secteurs et groupe taxonomique)

data <- subset(data, (data$secteur=="reserve_du_rhin" & data$order_tax=="Charadriiformes"))

#Aggrégation des données par la médiane selon l'espèce, secteur, annee et mois : 
data <- data %>%
  group_by(espece, secteur, annee, mois) %>%
  summarise(abondance = median(abondance))

#Création de nouvelles colonnes correspondant à l'année hivernale (la saison de compage en quelque sorte)
# Plus les mois d'hiver (1 = Juin et 12 = mai)
setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
data[,mois_hiver := ifelse(annee == annee_hiver, mois - 5, mois + 7)]
data[,mois_hiver_txt := as.character(mois_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

color_values <- data.frame(annee_hiver = vec_annee_hiver,hex = scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(vec_annee_hiver
))))

gg <- ggplot(data = data, mapping = aes(x = mois_hiver, y = abondance, color = annee_hiver_txt, group = annee_hiver_txt))
gg <- gg + geom_point() + geom_line() + theme_classic()
gg <- gg + facet_wrap(.~espece, scales = "free_y") + scale_y_log10()
gg <- gg + scale_color_manual(values = color_values$hex, labels = color_values$annee_hiver) 
gg

ggsave("out/figure_rhin_log(limi).png",width = 20, height = 10)

#Voir ce que ça donne avec les jours juliens hiver : 

#Faire une médiane par jour julien hiver : 
#Exemple avec juste les canards : 
data <- subset(data, (data$order_tax=="Anseriformes"))

setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

color_values <- data.frame(annee_hiver = vec_annee_hiver,hex = scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(vec_annee_hiver
))))

data <- data %>% 
  group_by(espece, secteur, jour_julien_hiver, annee_hiver_txt) %>% 
  summarise(abondance=median(abondance))

data <- subset(data, (data$secteur=="baie_aiguillon"))

gg <- ggplot(data = data, mapping = aes(x = jour_julien_hiver, y = abondance, color = annee_hiver_txt, group = annee_hiver_txt))
gg <- gg + geom_point() + theme_classic()
gg <- gg + facet_wrap(.~espece, scales = "free_y") + scale_y_log10()
gg <- gg + scale_color_manual(values = color_values$hex, labels = color_values$annee_hiver) 
gg

ggsave("out/figure_baie_ana_jjhiver.png",width = 20, height = 10)

#Les limicoles 
data <- subset(data, (data$order_tax=="Charadriiformes"))

setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

color_values <- data.frame(annee_hiver = vec_annee_hiver,hex = scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(vec_annee_hiver
))))

data <- data %>% 
  group_by(espece, secteur, jour_julien_hiver, annee_hiver_txt) %>% 
  summarise(abondance=median(abondance))

data <- subset(data, (data$secteur=="estuaire"))

gg <- ggplot(data = data, mapping = aes(x = jour_julien_hiver, y = abondance, color = annee_hiver_txt, group = annee_hiver_txt))
gg <- gg + geom_point() + theme_classic()
gg <- gg + facet_wrap(.~espece, scales = "free_y") + scale_y_log10()
gg <- gg + scale_color_manual(values = color_values$hex, labels = color_values$annee_hiver) 
gg

ggsave("out/figure_estuaire_limi_jjhiver.png",width = 20, height = 10)

#Analyses statistiques : 

#Variation effectif ~ annee (num)


#Variation effectif ~ annee (character)

#Tuckey effency (sur les données brutes)
#Colonne Q1 + Q4 puis calcul du Tuckey 
# Q1 et q4 que sur les données comptage confiance

#Tableau résumé : 
#Grouper données par espèces, mois et secteurs (faire une médiane pour les sp + Q1 et Q4)

#Tableau quantile (espèce, secteur, mois, Q1 et Q4) Ensuite rajouter colonnes (outlier bas et haut + faraway bas et haut)
# + merge aux tablaeux de données brutes + regarder si ça rentre dans l'intervalle 
# + rajouter colonnes si ça rentre dans l'intervalle 
# Combien on dégage de données si on enlève les outlier et/ou les Faraway
# Ensuite nettoyage puis aggrégation avec la médiane, mois et secteurs 

        ####### Estuaire de la Loire#####
        
# Sélection des anatidés : 

data_ana <- subset(data, (order_tax=="Anseriformes"))
data_ana <- subset(data_ana, (secteur=="estuaire"))
rm(list = c("data"))

#Voir la variation des abondances en fonction des mois :  
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee, group = annee))
gg <- gg + geom_point() + geom_line() + theme_classic()
gg

gg <- gg + facet_wrap(.~espece, scales = "free_y")
gg

ggsave("out/Anatide_estuaire.png",width = 10, height = 8)

ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth
#De manière générale on observe une diminution des effectifs à partir de janvier, avec un creux
#entre les mois d'avril, mai, juin et juillet avant que les effectifs remontent à partir du mois d'août
#Les effectifs maximaux semblent se situer entre novembre, décembre et janvier pour les anatidés

#Les courbes "smooth" ne semblent pas montrer de pattern (du moins c'est pas évident) de variation
#d'abondance par mois, ni entre les années. 

plot(data_ana$mois,data_ana$abondance)

#Voir avec la qualité du comptage :
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = qualite_comptage))
gg <- gg + geom_point() + theme_classic()
gg



#On a du mal à bien voir notamment pour les espèces qui ont de faibles abondances 

# On teste une transformation log pour mieux analyse les faibles abondances ? 

gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point() + theme_classic()
gglog

gglog <- gglog + facet_wrap(.~espece)
gglog

#Observations graphique : 
#Ce qui apparait semble déjà "mieux". Du moins on arrive à mieux apercevoir les patterns d'abondance
# qui varient au cours des mois. 

# Bernache cravant : diminution des effectifs à partir de janvier avec un "creux" assez marqué entre avril et aout
# remontée des effectifs à partir de septembre. 

#Bernache du Canada, les données semblent insufisantes pour observer quelque chose 
# Normal : c'est une espèce qu'on ne trouve pas en France (individus feraux ou "perdus")

#Bernache nonette : apparition d'un pattern (pas aussi évident que pour la cravant) 
#Espèce plus rare que la cravant (pas dans l'aire de répartition de la nonette)
#Toutefois on observe des effectifs pendant l'hiver qui diminue fortement en avril 
# et ça repart à la hausse en novembre... 

#Canard Chipeau : Diminution des effectifs, creux pendant la période de repro et ça repart à partir de septembre
#Canard Pilet : diminution avril et ça repart en septembre
#Canard siffleur : IDEM 
#Canard souchet : diminution avril + creux entre juin et juillet 
#Canard colvert : diminution qui est observée mais qui reste relativement faible (moins de variation apparente que pour les autres espèces)
#Sarcelle d'hiver : diminution à partir d'avril et ça repart en aout 
#Oie cendrée : creux situé entre juillet et aout 

#Cygne noir (espèce introduite - originaire d'australie)

gglog <- gglog + geom_smooth (aes (group = annee, fill = annee))
gglog

    # -> histogramme des abondances : 

gg_hist <- ggplot(data = data_ana, mapping = aes(x=abondance))
gg_hist <- gg_hist + geom_histogram(aes(color = qualite_comptage))
gg_hist

#Bcp de données de faible abondance (valeurs de 0) 
#Peu de grandes valeurs 
table(data_ana$abondance)
# 18 827 valeurs de 0 sur 23 609 observations 

gg_hist <- gg_hist + facet_wrap(.~espece)
gg_hist
 

# voir les abondance par sites 
gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() + theme_classic()
gg <- gg + facet_wrap(.~site, scales = "free_y")
gg
# Observations : des disparités qui paraissent importantes pour les abondances entre les sites
# avec des sites qui accueillent peu d'individus comparés à d'autres où les observations sont plus nombreuses : 
# grand Bilho + Massereau + Paimboeuf + Saint Nicolas + Petit Bilho accueillent de grand nombre d'effectifs d'anatidés

gg <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance))
gg <- gg + geom_point() + theme_classic()
gg <- gg + facet_grid(site~espece, scales = "free_y")
gg

# Voir avec les jours juliens : 
gg <- ggplot(data = data_ana, mapping = aes(x = jour_julien, y = abondance, color = annee))
gg <- gg + geom_point() + theme_classic() 
gg

gg <- gg + facet_wrap(.~espece)
gg

# Observations :
# On retrouve les mêmes tendances globales intra-annuelle avec le choix des mois


#Voyons voir ce qu'on obtiens si on ne conserve que les sites retenus : 

data_ana <- subset(data_ana, (data_ana$site_retenu=="oui"))
unique(data_ana$site)
# ATTENTION ! Site Estuaire est une énigme ! (on sait pas à quoi il correspond)

# Abondance des anatidés par mois sur plusieurs années 
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() + theme_classic()
gg
gg <- gg + geom_smooth(aes(group = annee, fill = annee))
gg 

# par espèce : 
gg <- gg +facet_wrap(.~espece)
gg 

#Essaie avec la fonction log ? 
gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog <- gglog + geom_smooth(aes(group = annee, fill = annee))
gglog

# par espèce 
gglog <- gglog + facet_wrap(.~espece)
gglog

#Histograme des abondances : 
gg_hist <- ggplot(data=data, mapping = aes(x = mois, y = abondance, color = annee))
gg_hist <- gg_hist + geom_histogram(aes(color = qualite_comptage))
gg_hist 

#Avec les jours julien ? 
gg <- ggplot(data = data_ana, aes(x=jour_julien, y=abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + geom_smooth(aes(group = annee, fill = annee))
gg

      #Les limicoles : 
data_limi <- subset(data, (order_tax=="Charadriiformes"))
data_limi <- subset(data_limi, (secteur=="estuaire"))

# Variation d'abondance intra-anuelle : 
gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg
# Observations : 
#Globalement on observe un creux des effectifs pour les mois de juin et juillet
# Avec une augmentation à partir du mois aout 
#On remarque un pic des effectifs qui se situe au niveau des mois de novembre, dec, janv et fev

gg <- gg + geom_smooth (aes (group = annee, fill = annee))
gg

#On regarde maintenant les abondance par mois/ pour chaque espece : 
gg <- gg + facet_wrap(.~espece)
gg

#Appliquer la fonction log :
gglog <- ggplot(data = data_limi, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog
gglog <- gglog + facet_wrap(.~espece)
gglog <- gglog + geom_smooth(aes(group = annee, fill = annee))
gglog

#Histograme des abondances : 
gg_hist <- ggplot(data = data_limi, mapping = aes(x = abondance))
gg_hist <- gg_hist + geom_histogram(aes(color = qualite_comptage))
gg_hist

# Regarder les abondances par sites : 
gg  <- ggplot(data=data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg

gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color =annee))
gg <- gg + geom_point() 
gg <- gg + facet_grid(site~espece)

#Ca donne quoi avec les jours juliens : 
gg <- ggplot(data = data_limi, mapping = aes(x = jour_julien, y = abondance, color = annee))
gg <- gg + geom_point() 
gg 
gg <- gg + facet_wrap(.~espece) 

#Ne conserver que les sites retenus ? 
data_limi <- subset(data_limi, (data_limi$site_retenu=="oui"))

gg <- ggplot(data = data_limi, aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + geom_smooth(aes(group = annee, fill = annee))
gg

gg <- gg + facet_wrap(.~espece)
gg 

#Histogramme des abondances : 
gg2 <- ggplot(data = data_limi, mapping = aes(x=abondance))
gg2 <- gg2 + geom_histogram(aes(color = qualite_comptage))
gg2
# Observations : idem que pour les anatidés 
# Beaucoup de données d'absence (0) et peu de données de comptage élevé
# 26 449 données de 0 sur 32443 observations : 
table(data_limi$abondance)

#Comparaison limicoles / anatidés : 
data <- read.csv2("Data/data.csv", header = T)
data <- subset(data,(data$secteur=="estuaire"))

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~order_tax)


              ########## Baie de l'aiguillon ######
data <- read.csv2("Data/data.csv", header = T)

    # Les anatidés 
data_ana <- subset(data, (order_tax=="Anseriformes"))
data_ana <- subset(data_ana, (secteur=="baie_aiguillon"))


#Voir la variation des abondances en fonction des mois :  
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg
#Observations : 
# On voit le pattern lié à la saisonnalité : abondance plus forte pendant les mois d'hiver
# Diminution des effectifs avec un creux pendant avril, mai, jui et juillet, puis une hausse des effectifs
# Attention, ici on regarde les années depuis 1980 !!!!

ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth

#Focus sur les espèces : 
gg <- gg + facet_wrap(.~espece)
gg

#Transformation en log
gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog <- gglog + facet_wrap(.~espece) 

#Histogramme des abondances 
gg_hist <- ggplot(data = data_ana, mapping = aes(x = abondance))
gg_hist <- geom_histogram(aes(color = qualite_comptage))
gg_hist

#Abondance et répartition des espèces sur les différents sites : 
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_grid(site~espece)
gg 

#Qu'est que ce qui se passe si on ne s'intéresse qu'au années à partir de 2004 ? 
data_ana <- subset(data_ana, !(data_ana$annee < 2004))

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg

#On retrouve le même pattern, avec une diminution entre les mois qui parait plus douce 
 
ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth

#Les courbes sont pas ouf "un peu écrasé vers les valeurs de 0" 

#On regarde par espèce ce que ça donne : 

gg <- gg + facet_wrap(.~espece)
gg

#La saisonnalité dans les abondances n'est visible que pour certaines espèces (celle avec de fortes abondance)

#essai avec log ? 

gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point() 
gglog

gglog <- gglog + geom_smooth(aes(group=annee, fill = annee))
gglog 

#espece : 

gglog <- gglog + facet_wrap(.~espece)
gglog

#Histograme des abondances : 
gg_hist <- ggplot(data = data_ana, mapping = aes(x = abondance))
gg_hist <- gg_hist + geom_histogram(aes(color = qualite_comptage))
gg_hist

#Regarder les abondances et la répartition des espèces par sites ? 
gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg

gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_grid(site~espece)
gg

#Les jours juliens : 
gg  <- ggplot(data=data_ana, mapping = aes(x = jour_julien, y = abondance, color = annee))
gg <- gg + geom_point()
gg

#Focus sur les sites "retenus" : 
data_ana <- subset(data_ana, (data_ana$site_retenu=="oui"))

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg <- geom_smooth(group = annee, fill = annee)
gg
gg <- gg + facet_wrap(.~espece)
gg

#Log 
gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point() 
gglog <- geom_smooth(group = annee, fill = annee)
gglog <- gglog + facet_wrap(.~espece)
gglog

#Histograme des abondances : 
gg_hist <- ggplot(data = data_ana, mapping = aes(x=abondance))
gg_hist <- gg_hist + geom_histogram(aes(color = qualite_comptage))
gg_hist          
      
    # Pour les limicoles : 
data_limi <- subset(data, (order_tax=="Charadriiformes"))
data_limi <- subset(data_limi, (secteur=="baie_aiguillon")) 

#Voir la variation des abondances en fonction des mois :  
gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg

# Attention, ici on regarde les années depuis 1980 !!!!

ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth

#Qu'est que ce qui se passe si on ne s'intéresse qu'au années à partir de 2004 ? 

data_limi <- subset(data_limi, !(data_limi$annee < 2004))
data_limi <- subset(data_limi,(data_limi$site_retenu=="oui"))

gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg

ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth

#Les courbes sont pas ouf "un peu écrasé vers les valeurs de 0" 

#On regarde par espèce ce que ça donne : 

gg <- gg + facet_wrap(.~espece)
gg

#La saisonnalité dans les abondances n'est visible que pour certaines espèces (celle avec de fortes abondance)

#essai avec log ? 

gglog <- ggplot(data = data_limi, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point() 
gglog

gglog <- gglog + geom_smooth(aes(group=annee, fill = annee))
gglog 

#espece : 

gglog <- gglog + facet_wrap(.~espece)
gglog

#Regarder les abondances par sites ? 
gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg

#Histograme des abondance
gg2 <- ggplot(data = data_limi, mapping = aes(x=abondance))
gg2 <- gg2 + geom_histogram()
gg2

# Avec les jours juliens ? 
gg <- ggplot(data = data_limi, aes(x = jour_julien, y = abondance, color = annee))
gg <- gg + geom_point()
gg

gg <- gg + geom_smooth()
gg

      ######## Bassin d'Arcachon ############

data <- subset(data, (data$secteur=="arcachon"))

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() + theme_classic()
gg <- gg + geom_smooth(aes(group = annee, fill = annee))
gg

# Les espèces 

gg <- gg + facet_wrap(.~espece)
gg

gglog <- ggplot(data = data, mapping = aes(x = mois, y = log(abondance+1), color = annee))
gglog <- gglog + geom_point() + theme_classic()
gglog <- gglog + facet_wrap(.~espece)                
gglog

# Voir qualite de comptage 

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = qualite_comptage))
gg <- gg + geom_point() + theme_classic()
gg

#Histograme des abondance 

gg_hist <- ggplot(data = data, mapping = aes(x = abondance))
gg_hist <- gg_hist + geom_histogram() + theme_classic()
gg_hist 

table(data$abondance)

######### Pour tous les anatidés de toutes les zh ############
data <- read.csv2("Data/data.csv", header = T)
data_ana <- subset(data, (order_tax=="Anseriformes"))

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg

gg <- gg + geom_smooth (aes (group = annee, fill = annee))
gg

#est-ce qu'on observe la même chose sur tous les secteurs ? 

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg 

gg <- ggplot(data = data_ana, mapping = aes(x = jour_julien, y = abondance, color = secteur))
gg <- gg + geom_point()
gg

# Voir les abondances par secteur
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() + theme_classic() 
gg <- gg + facet_grid(secteur~espece)
gg
 
############## Pour les limicoles de toutes les ZH ############

data_limi <- subset(data, (order_tax=="Charadriiformes"))

gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- geom_point()
gg