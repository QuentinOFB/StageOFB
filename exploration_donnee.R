library(ggplot2)

data <- read.csv2("Data/data.csv", header = T)

#Histogramme abondance par espèce : 
  
    #Anatidae :  
        # Les anatidés pour l'estuaire de la Loire : 
data_ana <- subset(data, (order_tax=="Anseriformes"))
data_ana <- subset(data_ana, (secteur=="estuaire"))

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
help("geom_point")
gg <- gg + geom_point()
gg
ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth
#De manière générale on observe un creux pour les mois d'avril, mai, juin et juillet, avant que les effectifs 
# Ne remonte à partir d'un mois d'aout. Les effectifs max semblent se situer autour de décembre et janvier

plot(data_ana$mois,data_ana$abondance)

#On regarde par espece : 
gg <- gg + facet_wrap(.~espece)
gg
gg <- gg + geom_smooth (aes (group = annee, fill = annee))
gg
#On a du mal à bien voir notamment pour les espèces qui ont de faibles abondances 

#test avec une transformation log (pas ouf pour le geom_smooth ?) 
gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log10(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog
gglog <- gglog + facet_wrap(.~espece)
#Observations graphique : 
#"Joli" pattern pour la bernache cravant, canard chipeau, pilet, siffleur, souchet, oie cendrée et sarcelle d'hiver
# moins évident pour le cygne tuberculé, la sarcelle d'été, bernache nonette, fuligule milouin et morillon
#Pour colvert et Tadorne, saisonnalité semble moins évidente 
# Pour les colverts c'est peut-être dû aux individus relachés pour la chasse (Yesou et al. 2017)
gglog <- gglog + geom_smooth (aes (group = annee, fill = annee))
gglog

  # -> histogramme des abondances : 
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance))
gg <- gg + geom_col()
gg

gg2 <- ggplot(data = data_ana, mapping = aes(x=abondance))
gg2 <- gg2 + geom_histogram()
gg2

# Grand nombre de données d'abondance faible (surtout les valeurs de 0) et peu de grandes valeurs d'abondance
# Exponentielle inverse ?  
#Pas trouvé "method = "
table(data_ana$abondance)

gg  <- ggplot(data=data_ana, mapping = aes(x = abondance, group = qualite_comptage))
gg <- gg + facet_grid(site~espece, scales = "free")
gg

      #Les limicoles : 
data_limi <- subset(data, (order_tax=="Charadriiformes"))
data_limi <- subset(data_limi, (secteur=="estuaire"))

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

#Appliquer la fonction log10 :
gglog <- ggplot(data = data_limi, mapping = aes(x = mois, y = log10(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog
gglog <- gglog + facet_wrap(.~espece)




#Voir pour les anatidés pour tous nos sites : 
data_ana <- subset(data, (order_tax=="Anseriformes"))

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = annee))
help("geom_point")
gg <- gg + geom_point()
gg



