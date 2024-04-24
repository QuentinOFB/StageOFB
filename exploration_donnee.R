library(ggplot2)

data <- read.csv2("Data/data.csv", header = T)

#Histogramme abondance par espèce : 
  
    ####### Estuaire de la Loire#####
        # Les anatidés pour l'estuaire de la Loire : 
data_ana <- subset(data, (order_tax=="Anseriformes"))
data_ana <- subset(data_ana, (secteur=="estuaire"))

#Voir la variation des abondances en fonction des mois :  
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg

ggsmooth <- gg + geom_smooth(aes(group=annee, fill = annee))
ggsmooth
#De manière générale on observe une diminution des effectifs à partir de janvier, avec un creux
#entre les mois d'avril, mai, juin et juillet avant que les effectifs remontent à partir du mois d'août
#Les effectifs maximaux semblent se situer entre novembre, décembre et janvier pour les anatidés

#Les courbes "smooth" ne semblent pas montrer de pattern (du moins c'est pas évident) de variation
#d'abondance par mois, ni entre les années. 

plot(data_ana$mois,data_ana$abondance)

#On regarde l'abondance pour les différentes espèces d'anatidés par mois 

gg <- gg + facet_wrap(.~espece)
gg
#Observations : 
# On a du mal à bien analyser les graph pour des espèces avec de faible valeurs d'abondance
# Toutefois, on peut déceler un pattern pour certaine espèce dont les abondances sont les plus fortes
# Canard siffleur, souchet, colvert, sarcelle d'hiver 

ggsmooth <- gg + geom_smooth (aes (group = annee, fill = annee))
ggsmooth
#On a du mal à bien voir notamment pour les espèces qui ont de faibles abondances 

# On teste une transformation log pour mieux analyse les faibles abondances ? 

gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log10(abondance+1), color = annee))
gglog <- gglog + geom_point()
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
gg_hist <- gg_hist + geom_histogram()
gg_hist

hist(data_ana$abondance, breaks = 200)

#Bcp de donnes de faible abondance (valeurs de 0) 
#Peu de grandes valeurs 

 
#Pas trouvé "method = "
table(data_ana$abondance)

# voir les abondance par sites 
gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg
# Observations : des disparités qui paraissent importantes pour les abondances entre les sites
# avec des sites qui accueillent peu d'individus comparés à d'autres où les observations sont plus nombreuses : 
# grand Bilho + Massereau + Paimboeuf + Saint Nicolas + Petit Bilho accueillent de grand nombre d'effectifs d'anatidés

gg <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance))
gg <- gg + geom_point()
gg <- gg + facet_grid(site~espece)
gg

#Voyons voir ce qu'on obtiens si on ne conserve que les sites retenus : 

data_ana <- subset(data_ana, (data_ana$site_retenu=="oui"))

# Abondance des anatidés par mois sur plusieurs années 
gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point() 
gg
gg <- gg + geom_smooth(aes(group = annee, fill = annee))
gg 

# par espèce : 
gg <- gg +facet_wrap(.~espece)
gg 

#Essaie avec la fonction log ? 
gglog <- ggplot(data = data_ana, mapping = aes(x = mois, y = log10(abondance+1), color = annee))
gglog <- gglog + geom_point()
gglog <- gglog + geom_smooth(aes(group = annee, fill = annee))
gglog

# par espèce 
gglog <- gglog + facet_wrap(.~espece)
gglog



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
gglog <- gglog + geom_smooth(aes(group = annee, fill = annee))
gglog

# Regarder les abondances par sites : 
gg  <- ggplot(data=data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg



#Histogramme des abondances : 
gg2 <- ggplot(data = data_limi, mapping = aes(x=abondance))
gg2 <- gg2 + geom_histogram()
gg2
# Observations : idem que pour les anatidés 


 ########## Baie de l'aiguillon ######

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

#Qu'est que ce qui se passe si on ne s'intéresse qu'au années à partir de 2004 ? 

data_ana <- subset(data_ana, !(data_ana$annee < 2004))
data_ana <- subset(data_ana,(data_ana$site_retenu=="oui"))

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

#Regarder les abondances par sites ? 
gg  <- ggplot(data=data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~site)
gg

#Histograme des abondance
gg2 <- ggplot(data = data_ana, mapping = aes(x=abondance))
gg2 <- gg2 + geom_histogram()
gg2

# Pour les limicoles 
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



######### Pour tous les anatidés de toutes les zh ############

data_ana <- subset(data, (order_tax=="Anseriformes"))

gg <- ggplot(data = data, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg

gg <- gg + geom_smooth (aes (group = annee, fill = annee))
gg

#est-ce qu'on observe la même chose sur tous les secteurs ? 

gg <- ggplot(data = data_ana, mapping = aes(x = mois, y = abondance, color = annee))
gg <- gg + geom_point()
gg <- gg + facet_wrap(.~secteur)
gg 

############## Pour les limicoles de toutes les ZH ############

data_limi <- subset(data, (order_tax=="Charadriiformes"))

gg <- ggplot(data = data_limi, mapping = aes(x = mois, y = abondance, color = annee))
gg <- geom_point()
gg