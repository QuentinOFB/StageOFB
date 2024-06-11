######## Error in fitTMB(TMBStruc) : negative log-likelihood is Nan at starting parameter values
# -> Apparement problème avec le package Matrix 
  # Il faut le réinstaller (v 1.6-2)

#1. Réinstaller Matrix 

#2. Réinstaller TMB 

#3. Réinstaller GLMMTMB

library(ggplot2)
library(data.table)
library(glmmTMB)
library(ggeffects)
library(dplyr)
library(DHARMa)
data <- read.csv2("Data/data_clean.csv", header = T)

#Données non aggrégate : 
data <- read.csv2("Data/data_clean_nonagglo.csv", header = T)

#Réunir les bernaches : 
sort(unique(data$espece))
data[,4] <- gsub("bernache_cravant_du_pacifique","bernache_cravant",data[,4])
data[,4] <- gsub("bernache_cravant_occidentale","bernache_cravant",data[,4])
data[,4] <- gsub("oie_de_la_toundra","oie_des_moissons",data[,4])

#Combien d'espèces intialement ? 
sort(unique(data$espece))

data <- subset(data, annee_hiver > 2003)


#Occurence totale (nombre de fois où l'espèce a été vue sur tous les secteurs)
occurence_tot <- data %>% count(espece,abondance, annee_hiver) %>% filter(abondance > 0)

occurence_tot <-  aggregate(occurence_tot, n ~ espece + annee_hiver, sum)
  
occurence_tot_ann <- occurence_tot %>% count(n, espece,annee_hiver)  
occurence_tot_ann <- occurence_tot_ann %>% count(nn, espece)  

data <- merge(data, occurence_tot, by.x = "espece", by.y = "espece")

#Tri des espèces : 

liste <- read.csv("Data/liste_sp.csv",header = T, sep = ";")
data <- merge(data,liste, by.x = "espece", by.y = "espece")
data <- subset(data, data$tri=="Oui")


#Création de l'année de référence : 
#sélectionner les années à partir de 2004
data <- subset(data, annee_hiver > 2003)

#setDT(data)
#dd <- data[, occ := sum(abondance > 0), by = .(espece, annee_hiver)]
#dd <- dd[,c(1,37,55)]
#dd[,occ_max := max(occ), by= .(espece)]
#dd[,occ_max := occ]##### ? 
#dd[,inc:= 1:.N, by=.(espece)]
#dd[inc==1,.(espece,annee_hiver,occ_max)]
#setnames(dd,"annee_hiver","annee_hiver_max")
#data <- merge(data,dd,by=("espece"),all.x = TRUE)
#data[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max,paste0("1.",annee_hiver),as.character(annee_hiver))]
#setDF(data)
#sort(unique(data$annee_hiver_txt))
#data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)

#Autre méthode 
d <- aggregate(data, abondance > 0 ~ annee_hiver + espece, sum)
setDT(d)
d[,occ_max := max(`abondance > 0`),by = .(espece)]
d <- subset(d, d$`abondance > 0`== d$occ_max)
sort(unique(d$espece))

d <- d %>% group_by(occ_max,espece) %>% filter(!duplicated(occ_max))

unique(d$espece)
setnames(d,"annee_hiver","annee_hiver_max")
data <- merge(data,d,by=("espece"),all.x = TRUE)
setDT(data)
data[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max,paste0("1.",annee_hiver),as.character(annee_hiver))]
setDF(data)
data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)

#  protocole :

data$protocole[data$protocole=="terrestre ?"] <- "terrestre"


#Ajout colonnes année hiver + mois hiver (déjà dans le tableau de données)
setDT(data)
data[,annee_hiver := ifelse(mois > 5, annee,annee - 1)]
data[,annee_hiver_txt := as.character(annee_hiver)]
data[,mois_hiver := ifelse(annee == annee_hiver, mois - 5, mois + 7)]
data[,mois_hiver_txt := as.character(mois_hiver)]
vec_annee_hiver <- sort(unique(data[,annee_hiver_txt]))
setDF(data)

########### Analyses statistiques #########
class(data$abondance)
data$abondance <- round(data$abondance, digits = 0)
# pour les arrondis 
data$abondance <- as.integer(data$abondance)

unique(data$protocole)

# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2"))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  #help("glmmTMB")
  
  #Négative binomiale et pas poisson ?  
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    # Obtenir les prédictions du modèle
    ggmd <- as.data.frame(ggpredict(md)$annee_hiver_txt)
    
    # Convertir en data.table pour des manipulations efficaces
    setDT(ggmd)
    
    # Renommer la colonne 'x' en 'year'
    setnames(ggmd, "x", "year")
    
    # Ajouter des colonnes supplémentaires
    ggmd[, `:=`(code = sp)]
    
    #Vérification du modèle : 
    #verif <- simulateResiduals(fittedModel = md, plot = F)
    #testZeroInflation(verif)
    #plot(verif)
    #jpeg(filename = paste(sp, "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
    #dev.print(device = png, file = paste(sp, "png", sep = "."), width = 600)
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      d_out_txt <- ggmd
      out_init <- TRUE
    } else {
      d_out_txt <- rbind(d_out_txt, ggmd, fill = TRUE)
    } } }

summary(md)
d_out_txt$year <- sort(d_out_txt$year)

#L'année en character : 
write.csv2(d_out_txt, "Data/d_out_txt.csv")

#Information importante : on obtiens pas la même chose avec l'année en facteur ou en numérique...
unique(d_out$code)
unique(d_out_fact$code)

#Mod pour avocette élégante 
md_avo <- glmmTMB(abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt), data = subset(data, espece == "vanneau_huppe" & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")
ggmd <- ggpredict(md_avo, terms = c("annee_hiver_txt"))

#Mod pour la sarcelle d'hiver 
md_sar <- glmmTMB(abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt), data = subset(data, espece == "sarcelle_d_hiver" & annee_hiver_txt>2003& site_retenu=="oui"), family = "nbinom2")
ggmd <- ggpredict(md_sar, terms = c("annee_hiver_txt"))

# Vérification du modèle : 
library(DHARMa)

#Test avec l'avocette élégante : 
verif <- simulateResiduals(fittedModel = md, plot = F)
testZeroInflation(verif)
help("DHARMa")
summary(md)
plot(verif)
#Test avec la sarcelle d'hiver
verif <- simulateResiduals(fittedModel = md_sar, plot = F)
testZeroInflation(verif)

#Interprétation avec la ligne rouge ? 

plot(verif)

#Visionner les abondances : il s'agit de valeur d'abondance moyenne par inventaire (une date + site)

print(ggmd)
plot(ggmd)

#Modification pour commencer la série temporelle à l'année 1 : 

ref <- ggmd$predicted[1] 
d_pred <- data.frame(annee = ggmd$x,abondance_var = ggmd$predicted / ref, ICinf = ggmd$conf.low/ref , ICsup = ggmd$conf.high/ref)
print(d_pred)


#Essaie de faire une boucle ? 
#Le faire avec l'année en facteur : (penser à faire la transformation en exponentielle)

out_init <- FALSE
vecsp <- unique(d_out_txt$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle

  data_ref <- subset(d_out_txt, code == sp)
  #data_ref$year <- sort(data_ref$year)
  ref <- data_ref$predicted[1]
  d_pred <- data.frame(annee = data_ref$year, abondance_var =  data_ref$predicted / ref, ICinf =  data_ref$conf.low/ref , ICsup =  data_ref$conf.high/ref)

  setDT(d_pred)
  
  # Ajouter des colonnes supplémentaires
  d_pred[, `:=`(code = sp)]  
  
  if (!out_init) {
    d_tp_txt <- d_pred
    out_init <- TRUE
  } else {
    d_tp_txt <- rbind(d_tp_txt, d_pred, fill = TRUE)
  } } 

#Pour l'année en facteur : 
write.csv2(d_tp_txt,"Data/d_tp_txt.csv")

sort(unique(d_tp_txt$annee))
d_tp_txt$annee <- as.character(d_tp_txt$annee)

d_tp_txt$annee[d_tp_txt$annee=="1.2022"] <- "2022"
d_tp_txt$annee[d_tp_txt$annee=="1.2011"] <- "2011"
d_tp_txt$annee[d_tp_txt$annee=="1.2009"] <- "2009"
d_tp_txt$annee[d_tp_txt$annee=="1.2020"] <- "2020"
d_tp_txt$annee[d_tp_txt$annee=="1.2006"] <- "2006"
d_tp_txt$annee[d_tp_txt$annee=="1.2017"] <- "2017"
d_tp_txt$annee[d_tp_txt$annee=="1.2019"] <- "2019"
d_tp_txt$annee[d_tp_txt$annee=="1.2014"] <- "2014"
d_tp_txt$annee[d_tp_txt$annee=="1.2018"] <- "2018"
d_tp_txt$annee[d_tp_txt$annee=="1.2021"] <- "2021"
d_tp_txt$annee[d_tp_txt$annee=="1.2015"] <- "2015"
d_tp_txt$annee[d_tp_txt$annee=="1.2016"] <- "2016"
d_tp_txt$annee[d_tp_txt$annee=="1.2010"] <- "2010"
d_tp_txt$annee[d_tp_txt$annee=="1.2007"] <- "2007"
d_tp_txt$annee[d_tp_txt$annee=="1.2008"] <- "2008"
d_tp_txt$annee[d_tp_txt$annee=="1.2023"] <- "2023"
d_tp_txt$annee[d_tp_txt$annee=="1.2012"] <- "2012"

sort(unique(d_tp_txt$annee))
d_tp_txt$annee <- as.character(d_tp_txt$annee)
d_tp_txt$annee <- as.factor(d_tp_txt$annee)

#Représentation graphique : 
  # Une boucle ? 

out_init <- FALSE
vecsp <- unique(d_tp_txt$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)

  
gg <- ggplot(data = subset(d_tp_txt, code == sp), mapping=aes(x=annee, y=abondance_var))
gg <- gg + geom_line()
gg <- gg + geom_pointrange(aes(ymin = ICinf, ymax=ICsup)) + geom_hline(yintercept = 1, color = "red")
gg <- gg + labs(y="Variation d'abondance",x="Années", title = sp) 


ggsave(filename = paste(sp,"png", sep= "."), path = "out/Variation_annee_corr", width = 10, height = 10) 

#jpeg(paste(names(setosa)[i], "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)

print(gg)

}


###### Partie 2 : l'année en numérique ###########

class(data$annee_hiver)
data$annee_hiver <- as.numeric(data$annee_hiver)
# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2"))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    # Obtenir les prédictions du modèle
    ggmd <- as.data.frame(ggpredict(md)$annee_hiver)
    
    # Convertir en data.table pour des manipulations efficaces
    setDT(ggmd)
    
    # Renommer la colonne 'x' en 'year'
    setnames(ggmd, "x", "year")
    
    # Ajouter des colonnes supplémentaires
    ggmd[, `:=`(code = sp)]
    
    #Vérification du modèle : 
    #verif <- simulateResiduals(fittedModel = md, plot = F)
    #testZeroInflation(verif)
    #dev.print(device = png, file = paste(sp,"zi","png", sep = "."), width = 600)
    
    #plot(verif)
    #dev.print(device = png, file = paste(sp, "png", sep = "."), width = 600)
    
    #Faire un tableau avec les coefficients : 
    tab_trend_raw <- as.data.frame(coef(summary(md))$cond)
    trend_raw <- tab_trend_raw[2,1]
    trend <- exp(trend_raw)
    mdIC <- as.data.frame(confint(md)[,1:2])
    colnames(mdIC) <- c("ICinf","ICsup")
    IC_inf_raw <- mdIC$ICinf[2]
    IC_sup_raw <- mdIC$ICsup[2]
    IC_inf <- exp(IC_inf_raw)
    IC_sup <- exp(IC_sup_raw)
    tab_trend <- data.frame(trend, IC_inf ,IC_sup, p_val=tab_trend_raw[2,4])
    setDT(tab_trend)
    tab_trend[, `:=` (code = sp)]
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      d_out_num <- ggmd
      tab <- tab_trend
      out_init <- TRUE
    } else {
      d_out_num <- rbind(d_out_num, ggmd, fill = TRUE)
      tab <- rbind(tab, tab_trend, fill = TRUE)
    } } }


write.csv2(tab, "Data/tab.csv")

#DHARMa:testOutliers with type = binomial may have inflated Type I error rates for integer-valued distributions. To get a more exact result, it is recommended to re-run testOutliers with type = 'bootstrap'. See ?testOutliers for details



#Si on veut enregistrer d_Out (pour évitier d'avoir à faire retourner le modèle) 
#L'année en numérique :
write.csv2(d_out_num,"Data/d_out_num.csv")
summary(md)

gg <- ggplot(data = subset(d_out_num, code == "becasseau_variable"), mapping=aes(x=year, y=predicted))
gg <- gg + geom_line()
gg <- gg + geom_pointrange(aes(ymin = conf.low, ymax=conf.high))
gg <- gg + labs(y="Variation d'abondance",x="Années")
print(gg)

write.csv2(d_out_num,"Data/d_out_num.csv")
write.csv2(tab,"Data/tab_mod.csv")






################# Essaie avec Zi ##############


# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2" , ziformula = ~ 1))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  #> Zi => Permet de gérer l'excès de 0 (notamment quand la probabilité de détection est différente de 1, donc génère des 0 parce qu'on voit pas l'espèce, et pas parce qu'elle n'est pas forcément là)
  help("glmmTMB")
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    # Obtenir les prédictions du modèle
    ggmd <- as.data.frame(ggpredict(md)$annee_hiver_txt)
    
    # Convertir en data.table pour des manipulations efficaces
    setDT(ggmd)
    
    # Renommer la colonne 'x' en 'year'
    setnames(ggmd, "x", "year")
    
    # Ajouter des colonnes supplémentaires
    ggmd[, `:=`(code = sp)]
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      d_out_zi <- ggmd
      out_init <- TRUE
    } else {
      d_out_zi <- rbind(d_out_zi, ggmd, fill = TRUE)
    } } }

summary(md)

# Variation d'abondance 
out_init <- FALSE
vecsp <- unique(d_out_zi$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  data_ref <- subset(d_out_zi, code == sp)
  data_ref$year <- sort(data_ref$year)
  ref <- data_ref$predicted[1]
  d_pred <- data.frame(annee = data_ref$year, abondance_var =  data_ref$predicted / ref, ICinf =  data_ref$conf.low/ref , ICsup =  data_ref$conf.high/ref)
  
  setDT(d_pred)
  
  # Ajouter des colonnes supplémentaires
  d_pred[, `:=`(code = sp)]  
  
  if (!out_init) {
    d_tp_txt_zi <- d_pred
    out_init <- TRUE
  } else {
    d_tp_txt_zi <- rbind(d_tp_txt_zi, d_pred, fill = TRUE)
  } } 

#Pour l'année en facteur : 
write.csv2(d_tp_txt_zi,"Data/d_tp_txt_zi.csv")


#Représentation graphique : 
# Une boucle ? 

out_init <- FALSE
vecsp <- unique(d_tp_txt_zi$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)
  
  gg <- ggplot(data = subset(d_tp_txt_zi, code == sp), mapping=aes(x=annee, y=abondance_var))
  gg <- gg + geom_line()
  gg <- gg + geom_pointrange(aes(ymin = ICinf, ymax=ICsup))
  gg <- gg + labs(y="Variation d'abondance",x="Années", title = sp) 
  
  
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/Variation_annee_zi", width = 10, height = 10) 
  
} 


md_van <- glmmTMB(abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt), data = subset(data, espece == "vanneau_huppe" & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")
gg_van <- ggpredict(md_van, terms = c("annee_hiver_txt"))
summary(md_van)

###### Avec l'année en numérique : 

vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE
attach(data)
# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2", ziformula = ~ 1))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  help("glmmTMB")
  
  #Négative binomiale et pas poisson ?  
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    
    #Faire un tableau avec les coefficients : 
    tab_trend_raw <- as.data.frame(coef(summary(md))$cond)
    trend_raw <- tab_trend_raw[2,1]
    trend <- exp(trend_raw)
    
    mdIC <- as.data.frame(confint(md)[,1:2])
    colnames(mdIC) <- c("ICinf","ICsup")
    IC_inf_raw <- mdIC$ICinf[2]
    IC_sup_raw <- mdIC$ICsup[2]
    IC_inf <- exp(IC_inf_raw)
    IC_sup <- exp(IC_sup_raw)
    nb_year <- length(unique(subset(data, espece == sp &  data$annee_hiver > 2003)[,37]))
    first_year <- min(subset(data, espece ==sp &  data$annee_hiver > 2003)[,37])
    last_year <- max(subset(data,espece == sp & data$annee_hiver > 2003)[,37])
    tab_trend <- data.frame(nb_year, 
                            first_year, 
                            last_year,
                            trend, IC_inf, IC_sup, pval = tab_trend_raw[2,4]) 
                            
    setDT(tab_trend)
    #setnames(tab_trend, "trend.2..1.", "estimate")
    tab_trend[, `:=` (code = sp)]
    
    #affectCatEBCC <- function(trend, pVal = p_val, ICinf, ICsup){ 
      #catEBCC <- ifelse(pVal>0.05,
      #ifelse(ICinf < 0.95 | ICsup > 1.05, "Incertain","stable"),
      #ifelse(trend < 1, 
      #ifelse(ICsup < 0.95, "Fort déclin", "Déclin modéré"),
      #ifelse(ICinf > 1.05, "Forte augmentation","Augmentation modérée")))
     # return(catEBCC)
      #}
    #duration <- nb_year-1
    
    #tab_trend[,`:=`(pourcentage_var = round(((trend^duration) -1)*100,2),
                    #pourcentage_IC_inf = round(((IC_inf^duration) -1)*100,2),
                    #pourcentage_IC_sup = round(((IC_sup^duration) -1)*100,2),
                    #catEBCC = affectCatEBCC(trend = trend,pVal = p_val,ICinf=IC_inf,ICsup=IC_sup))]
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      tab <- tab_trend
      out_init <- TRUE
    } else {
      tab <- rbind(tab, tab_trend, fill = TRUE)
    } } }


write.csv2(tab, "Data/tab_mod_zi.csv")

############# Analyse avec le secteur #############

#Centrée réduire la variable année ? 
setDT(data)
data[,`:=`(annee_sc = scale(annee_hiver))]
setDF(data)

#Pour le premier test on va choisir l'avocette élégante : (nb les années > 2003 ont déjà été sélectionnées)

data_avo <- subset(data, espece == "avocette_elegante" & site_retenu == "oui")

#Le modèle : 

md <- glmmTMB(abondance ~ annee_hiver*secteur + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = data_avo, family = nbinom2)
summary(md)

#Problème -> Que des NaN pour l'erreur standard, z value et p value 
# Peut-être que le soucis vient de "secteur/site ? 
md1 <- glmmTMB(abondance ~ annee_hiver*secteur + (1|site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = data_avo, family = "nbinom2")
summary(md1)
#Toujours pas... 

#Test en enlevant un effet aléatoire (au hasard obs)
md2 <- glmmTMB(abondance ~ annee_hiver*secteur + (1|site) + (1|obs) + (1|mois_hiver_txt), data = data_avo, family = "nbinom2")
summary(md2)
#### -> On obtient des valeurs pour l'erreur standard, z value et p value : 
md3 <- glmmTMB(abondance ~ annee_hiver*secteur + (1|site) + (1|mois_hiver_txt) + (1|protocole), data = data_avo, family = "nbinom2")
summary(md3)
####### Problème avec le protocole ? des NaN encore...
md4 <- glmmTMB(abondance ~ annee_hiver*secteur + (1|site) + (1|obs) + (1|protocole), data = data_avo, family = "nbinom2")
summary(md4)

md5 <- glmmTMB(abondance ~ annee_hiver*secteur + (1|mois_hiver_txt) + (1|obs) + (1|protocole), data = data_avo, family = "nbinom2")
summary(md5)



tab <- as.data.frame(coef(summary(md))$cond)
tab$title <- ""


tab$title[1] <- "intercept"
rownames(tab)[1] <- "(Intercept)"

tab_trend <- tab[,1]
trend <- exp(tab_trend)

ind <- as.data.frame(confint(md)[,1:2])
colnames(mdIC) <- c("ICinf","ICsup")
IC_inf_raw <- mdIC$ICinf
IC_sup_raw <- mdIC$ICsup[2]

#+ (1|secteur/site) (1|obs) + (1|mois_hiver_txt) + (1|protocole) +
#A creuser : ne parvient pas à calculer les borne écart-type et pvalue !!!
#Modèle trop complexe ? produite des Na si Secteur/site et s'il y a plus de 3 var aléatoires



vecsp <- unique(data$espece)
# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver * secteur + (1|site) + (1|obs) + (1|mois_hiver_txt)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  # 1| protocole => effet aléatoire du protocole 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2"))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  help("glmmTMB")
  
  #Négative binomiale et pas poisson ?  
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    
    #Le tableau avec les coefficients : 
    tab_trend_raw <-as.data.frame(coef(summary(md))$cond)
    trend_raw <- tab_trend_raw[,1]
    trend <- exp(tab_trend_raw)
    
    mdIC <- as.data.frame(confint(md)[,1:2])
    colnames(mdIC) <- c("ICinf","ICsup")
    mdIC <- mdIC[-c(13:15),]
    IC_inf_raw <- mdIC$ICinf
    IC_sup_raw <- mdIC$ICsup
    IC_inf <- exp(IC_inf_raw)
    IC_sup <- exp(IC_sup_raw)
    #nb_year <- length(unique(subset(data, espece == sp &  data$annee_hiver > 2003)[,37]))
    #first_year <- min(subset(data, espece ==sp &  data$annee_hiver > 2003)[,37])
    #last_year <- max(subset(data,espece == sp & data$annee_hiver > 2003)[,37])
    tab_trend <- data.frame(trend, IC_inf, IC_sup, pval = tab_trend_raw[,4]) 
    
    setDT(tab_trend)
    #setnames(tab_trend, "trend.2..1.", "estimate")
    tab_trend[, `:=` (code = sp)]
    
    #affectCatEBCC <- function(trend, pVal = p_val, ICinf, ICsup){ 
    #catEBCC <- ifelse(pVal>0.05,
    #ifelse(ICinf < 0.95 | ICsup > 1.05, "Incertain","stable"),
    #ifelse(trend < 1, 
    #ifelse(ICsup < 0.95, "Fort déclin", "Déclin modéré"),
    #ifelse(ICinf > 1.05, "Forte augmentation","Augmentation modérée")))
    # return(catEBCC)
    #}
    #duration <- nb_year-1
    
    #tab_trend[,`:=`(pourcentage_var = round(((trend^duration) -1)*100,2),
    #pourcentage_IC_inf = round(((IC_inf^duration) -1)*100,2),
    #pourcentage_IC_sup = round(((IC_sup^duration) -1)*100,2),
    #catEBCC = affectCatEBCC(trend = trend,pVal = p_val,ICinf=IC_inf,ICsup=IC_sup))]
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      tab <- tab_trend
      out_init <- TRUE
    } else {
      tab <- rbind(tab, tab_trend, fill = TRUE)
      write.csv2(tab, file = paste(sp,"csv",sep = "."))
    } } }
 help("write.csv2")             
 