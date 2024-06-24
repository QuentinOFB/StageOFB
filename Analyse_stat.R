#Chargement des package : 

library(ggplot2)
library(data.table)
library(glmmTMB)
library(ggeffects)
library(dplyr)
library(DHARMa)
library(stringr)

#Ouverture du jeu de données : 
data <- read.csv2("Data/data_clean_nonagglo.csv", header = T)

#Réunir les bernaches : 
sort(unique(data$espece))
data[,4] <- gsub("bernache_cravant_du_pacifique","bernache_cravant",data[,4])
data[,4] <- gsub("bernache_cravant_occidentale","bernache_cravant",data[,4])
data[,4] <- gsub("oie_de_la_toundra","oie_des_moissons",data[,4])

#Combien d'espèces intialement ? -> 116 taxons  
sort(unique(data$espece))

#Mettre l'année Txt et le mois txt en facteur : 
data$annee_hiver_txt <- as.character(data$annee_hiver_txt)
data$mois_hiver_txt <- as.character(data$mois_hiver_txt)

#Ne sélectionner que la série temporelle : 2004 - 2024 
data <- subset(data, annee_hiver > 2003)

#Centrer réduire l'année : 
setDT(data)
data[,`:=`(annee_sc = scale(annee_hiver))]
setDF(data)


#Tri des espèces : 
liste <- read.csv("Data/liste_sp.csv",header = T, sep = ";")
data <- merge(data,liste, by.x = "espece", by.y = "espece")
data <- subset(data, data$tri=="Oui")

#Création de l'année de référence : 
d <- aggregate(data, abondance > 0 ~ annee_hiver + espece, sum)
setDT(d)
d[,occ_max := max(`abondance > 0`),by = .(espece)]
d <- subset(d, d$`abondance > 0`== d$occ_max)

d <- d %>% group_by(occ_max,espece) %>% filter(!duplicated(occ_max))

setnames(d,"annee_hiver","annee_hiver_max")

data <- merge(data,d,by=("espece"),all.x = TRUE)
setDT(data)
data[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max, paste0("1.",annee_hiver),as.character(annee_hiver))]
setDF(data)


#Création de l'année de référence pour chaque espèce et par secteur : 
d_sect <- aggregate(data, abondance > 0 ~ annee_hiver + espece + secteur, sum)
setDT(d_sect)
d_sect[,occ_max := max(`abondance > 0`),by = .(espece,secteur)]
d_sect <- subset(d_sect, d_sect$`abondance > 0`== d_sect$occ_max)
d_sect <- d_sect %>% group_by(occ_max,espece,secteur) %>% filter(!duplicated(occ_max))

setnames(d_sect,"annee_hiver","annee_hiver_max")
setnames(d_sect,"secteur","secteur_max")


d_sect$id_max <- paste0(d_sect$espece,d_sect$secteur_max)
data$id_max <- paste0(data$espece,data$secteur)
data <- merge(data,d_sect,by=("id_max"),all.x = TRUE)

setDT(data)
data[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max, paste0("1.",annee_hiver),as.character(annee_hiver))]
setDF(data)

colnames(data) [2] <- "espece"


#  protocole :
data$protocole[data$protocole=="terrestre ?"] <- "terrestre"


    ########### Analyses statistiques #########

### Boucle initiale (exemple) ####

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
  
  # Vérifier si le modèle a été ajusté avec succès
  if (class(md)[1] != "try-error") {
    # Obtenir les prédictions du modèle
    ggmd <- as.data.frame(ggpredict(md)$annee_hiver_txt)
    
    #Création de la valeur d'initiation : 
    init <- 
    
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


#Enlever les espèces pour qui ça fonctionne pas : 
data <- subset(data, !(espece=="becasseau_de_temminck"|espece=="becassine_sourde"
                       |espece=="canard_mandarin"|espece=="chevalier_stagnatile"
                       |espece=="cygne_chanteur"|espece=="cygne_de_bewick"|espece=="harle_bievre"|espece=="cygne_noir"|espece=="eider_a_duvet"
                       |espece=="erismature_rousse"))



####### Analyse des tendances générales sur le long terme  #########
# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)
  
  #Modèle 1 (année en facteur)
  md1 <- glmmTMB(abondance~annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|protocole) + (1|mois_hiver_txt), data = subset(data, espece == sp & site_retenu=="oui"), family = "nbinom2")
  
  #Générer les prédictions pour le modèle 1 : 
  pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
  setDT(pred1)
  pred1[, group := "var"]
  
  #Valeur initiale (correspondant au prédict de l'année de référence)
  init <- pred1 |> filter(x |> str_detect(fixed("1.")))
  init <- init$predicted
  
  #Retirer ensuite le 1. 
  pred1$x <- pred1$x |> 
    str_replace(fixed("1."), "")
  
  #Modèle 2 : année en numérique (la tendance)
  
  md2 <- glmmTMB(abondance~annee_hiver + (1|secteur/site) + (1|obs) + (1|protocole) + (1|annee_hiver_txt/mois_hiver_txt),data = subset(data, espece == sp & site_retenu=="oui"), family = "nbinom2")
  smd2 <- summary(md2)
  
  #Récupérer les estimates : 
  est <- as.data.frame(smd2$coefficients$cond)
  
  #Intervalle de confiance pour le second modèle : 
  est_tot <- as.data.frame(confint(md2))[2,]
  colnames(est_tot) <- c("ICinf", "ICsup", "mean")
  setDT(est_tot)
  
  #Calculer le nombre d'années dans les données 
  data_now <- subset(data, espece==sp & site_retenu=="oui")
  nb_y <- max(data_now[, "annee_hiver"]) - min(data_now[, "annee_hiver"])  
  
  # Calculer les pourcentages de variation et les intervalles de confiance
  est_tot[, `:=`(var = "annee_hiver", nb_year = nb_y, pourcent = round(mean * nb_y * 100, 2), pourcent_inf = round(ICinf * nb_y * 100, 2), pourcent_sup = round(ICsup * nb_y * 100, 2))]
  
  # Générer des prédictions pour le deuxième modèle
  pred2 <- as.data.frame(ggpredict(md2, terms = list(annee_hiver=seq(2004,2024,0.1))))
  setDT(pred2)
  pred2[, group := "trend"]
  
  tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
  trend_raw <- tab_trend_raw[2,1]
  
  mean_year <- mean(data_now[,"annee_hiver"])
  sd_year <- sd(data_now[,"annee_hiver"])
  
  trend <- exp(trend_raw)
  
  mdIC <- as.data.frame(confint(md2)[,1:2])
  colnames(mdIC) <- c("ICinf","ICsup")
  IC_inf_raw <- mdIC$ICinf[2]
  IC_sup_raw <- mdIC$ICsup[2]
  IC_inf <- exp(IC_inf_raw)
  IC_sup <- exp(IC_sup_raw)
  
  tab_trend <- data.frame(nb_year = length(unique(data_now[,"annee_hiver_txt"])), first_year = min(data_now[,"annee_hiver"]), last_year = max(data_now[,"annee_hiver"]), trend, IC_inf ,IC_sup,p_val=tab_trend_raw[2,4])
  setDT(tab_trend)
  
  #Ajouter les catégories EBCC de tendance : 
  affectCatEBCC <- function(trend,pval,ICinf,ICsup){
    
    catEBCC <- ifelse(pval>0.05,
                      ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
                      ifelse(trend<1,
                             ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
                             ifelse(ICinf>1.05,"Forte augmentation","Augmentation modérée")))
    return(catEBCC)
  }
  duration <- length(unique(data_now[,"annee_hiver"]))-1
  
  tab_trend[,`:=`(pourcentage_var = round(((trend^duration) -1)*100,2),
                  pourcentage_IC_inf = round(((IC_inf^duration) -1)*100,2),
                  pourcentage_IC_sup = round(((IC_sup^duration) -1)*100,2),
                  catEBCC = affectCatEBCC(trend = trend, pval = p_val,ICinf=IC_inf,ICsup=IC_sup))]
 

  # Combiner les prédictions des deux modèles
  pred <- rbind(pred1, pred2, fill = TRUE)
  pred$x <- as.numeric(pred$x)
  
  #pred[, x := as.numeric(as.character(x))]
  
  #Récupérer la pvalue 
  pval <- paste("pvalue",tab_trend$p_val,sep = "=")
  
  # Renommer la colonne 'x' en 'year' et normaliser les prédictions
  colnames(pred)[1] <- "year"
  setDT(pred)
  pred[, `:=`(predicted = predicted/init, conf.low = conf.low/init, conf.high = conf.high/init)]
  
  #Vérification des modèles avec DHARMa : 
    #Modèle 1 : 
  #verif1 <- simulateResiduals(fittedModel = md1, plot = F)
  #testZeroInflation(verif1)
  #plot(verif1)
  #jpeg(filename = paste(sp, "verif1_jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
    #Modèle 2 :
  #verif2 <- simulateResiduals(fittedModel = md2, plot = F)
  #testZeroInflation(verif2)
  #plot(verif2)
 # jpeg(filename = paste(sp, "verif2_jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)

  # Définir le titre et le sous-titre du graphique
  title <- paste(sp, pval, sep=" ")

  # Définir les couleurs pour les graphiques
  col_river <- "#2b8cbe"
  col_trend1 <- "#e6550d"
  col_trend2 <- "#fdae6b"
  vec_col <- c(col_river, col_trend1)
  vec_fill <- c(col_river, col_trend2)
  
  # Créer le graphique avec ggplot2
  gg <- ggplot(pred, aes(x = year, y = predicted, colour = group, ymin = conf.low, ymax = conf.high, fill = group, group = group))
  gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
  gg <- gg + labs(x = "Année hiver", y = "Variation abondance", title = title, size = 12)
  gg <- gg + scale_colour_manual(values = vec_col) + scale_fill_manual(values = vec_fill)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                               axis.title = element_text(size = 14, face = "bold"),
                               axis.text = element_text(size = 14, face = "bold"))
  # Afficher le graphique : 
  print(gg)

  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/figure", width = 10, height = 10) 
  
  #Combiner les tableaux de predictions + tendances pour les espèces : 
  setDT(pred)
  pred[, `:=`(code = sp)]  
  
  tab_trend$sp <- sp
  
  if (!out_init) {
    data_pred <- pred
    data_trend <- tab_trend
    out_init <- TRUE
  } else {
    data_pred <- rbind(data_pred, pred, fill = TRUE)
    data_trend <- rbind(data_trend, tab_trend, fill = TRUE)
  } } 


write.csv(data_trend,"Data/data_trend.csv", fileEncoding = "UTF-8")

####### Analyses des tendances sur le long terme en fonction des secteurs ##################  
  # ATTENTION !!! Bien prendre en compte l'année de référence par espèce et secteurs 
  #Enlever le protocole en effet aléatoire car un seul niveau pour la plupart des secteurs (sauf estuaire)

#Ici une boucle dans l'autres par espèce et secteur : 
out_init2 <- FALSE
out_init <- FALSE
vecsp <- unique(data$espece)
  #Boucle espèce : 
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp) 

 #Boucle secteur :  
  data_1 <- subset(data, espece == sp)
  vecsct <- unique(data_1$secteur)
  for (isp in 1:length(vecsct)) {
    sct <- vecsct[isp]  
    cat("\n\n (", isp, "/", length(vecsct), ") ", sct)
  
    #Modèle 1 (avec l'année en facteur) : 
    md1 <- glmmTMB(abondance ~ annee_hiver_txt + (1|site) + (1|obs) + (1|mois_hiver_txt), data = subset(data, secteur == sct & espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")

    #Prediction modèle 1 : 
    pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
    
  #Générer les prédictions pour le modèle 1 : 
  pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
  setDT(pred1)
  pred1[, group := "var"]
  
  #Variable prédite de référence : 
  init <- pred1 |> filter(x |> str_detect(fixed("1.")))
  init <- init$predicted
  
  #Enlever les 1.
  pred1$x <- pred1$x |> 
  str_replace(fixed("1."), "")
  
  #Modèle 2 : Année en numérique : 
  md2 <- glmmTMB(abondance ~ annee_hiver + (1|site) + (1|obs) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(data, secteur == sct & espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")
  summary(md2)

  #Résumer du second modèle : 
  smd2 <- summary(md2)
  est2 <- as.data.frame(smd2$coefficients$cond)[2,]

  #Intervalle de confiance pour le second modèle : 
  est_tot <- as.data.frame(confint(md2))[2,]
  colnames(est_tot) <- c("ICinf", "ICsup", "mean")
  setDT(est_tot)

  #Calculer le nombre d'années dans les données 
  data_now <- subset(data, espece == sp & secteur == sct & site_retenu=="oui")
  nb_y <- max(data_now[, "annee_hiver"]) - min(data_now[, "annee_hiver"])  

  # Calculer les pourcentages de variation et les intervalles de confiance
  est_tot[, `:=`(var = "annee_hiver", nb_year = nb_y, pourcent = round(mean * nb_y * 100, 2), pourcent_inf = round(ICinf * nb_y * 100, 2), pourcent_sup = round(ICsup * nb_y * 100, 2))]

  # Générer des prédictions pour le deuxième modèle
  pred2 <- as.data.frame(ggpredict(md2, terms = list(annee_hiver = seq(2004, 2024, 0.1))))
  setDT(pred2)
  pred2[, group := "trend"]
  
  #Verification des modèles avec DHARMa :
  #verif1 <- simulateResiduals(fittedModel = md1, plot = F)
  #testZeroInflation(verif1)
  #plot(verif1)
  #jpeg(filename = paste(sp, "verif1_jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
  
  #verif2 <- simulateResiduals(fittedModel = md2, plot = F)
  #testZeroInflation(verif1)
  #plot(verif1)
  #jpeg(filename = paste(sp, "verif2_jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)
  
  
  # Combiner les prédictions des deux modèles
  pred <- rbind(pred1, pred2, fill = TRUE)
  pred[, x := as.numeric(as.character(x))]
  
  #Récupérer la pvalue 
  tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
  pval <-tab_trend_raw[2,4]
  pval <- paste("pvalue",pval,sep = "=")

  #Renommer la colonne 'x' en 'year' et normaliser les prédictions
  setnames(pred, "x", "year")
  pred[, `:=`(predicted = predicted/init, conf.low = conf.low/init, conf.high = conf.high/init)]
  pred$secteur <- sct
  pred$sp <- sp
  
  tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
  trend_raw <- tab_trend_raw[2,1]
  
  mean_year <- mean(data_now[,"annee_hiver"])
  sd_year <- sd(data_now[,"annee_hiver"])
  
  trend <- exp(trend_raw)
  
  mdIC <- as.data.frame(confint(md2)[,1:2])
  colnames(mdIC) <- c("ICinf","ICsup")
  IC_inf_raw <- mdIC$ICinf[2]
  IC_sup_raw <- mdIC$ICsup[2]
  IC_inf <- exp(IC_inf_raw)
  IC_sup <- exp(IC_sup_raw)
  
  tab_trend <- data.frame(nb_year = length(unique(data_now[,"annee_hiver_txt"])), first_year = min(data_now[,"annee_hiver"]), last_year = max(data_now[,"annee_hiver"]), trend, IC_inf ,IC_sup,p_val=tab_trend_raw[2,4])
  setDT(tab_trend)
  
  #Ajouter les catégories EBCC de tendance : 
  affectCatEBCC <- function(trend,pval,ICinf,ICsup){
    
  catEBCC <- ifelse(pval>0.05,
                      ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
                      ifelse(trend<1,
                             ifelse(ICsup<0.95,"Fort déclin","Déclin modéré"),
                             ifelse(ICinf>1.05,"Forte augmentation","Augmentation modée")))
    return(catEBCC)
  }
  duration <- length(unique(data_now[,"annee_hiver"]))-1
  
  tab_trend[,`:=`(pourcentage_var = round(((trend^duration) -1)*100,2),
                  pourcentage_IC_inf = round(((IC_inf^duration) -1)*100,2),
                  pourcentage_IC_sup = round(((IC_sup^duration) -1)*100,2),
                  catEBCC = affectCatEBCC(trend = trend,pval = p_val,ICinf=IC_inf,ICsup=IC_sup))]
  
 tab_trend$sp <- sp
 tab_trend$secteur <- sct

  if (!out_init) {
    data_pred <- pred
    data_trend <- tab_trend
    out_init <- TRUE
  } else {
    data_pred <- rbind(data_pred, pred, fill = TRUE)
    data_trend <- rbind(data_trend, tab_trend, fill = TRUE)
  }}
    
#Représentation graphique : 
title <- sp
           
 gg <- ggplot(subset(data_pred, sp==sp), aes(x = year, y = predicted, group = secteur, colour = secteur))
 gg <- gg + geom_line(data = subset(data_pred,group=="trend"), aes(x = year, y = predicted, color = secteur), size = 1) 
 gg <- gg + labs(x = "Années hiver",y = "Variation d'abondance", title = title, size = 12)
 gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                               axis.title = element_text(size = 14, face = "bold"),
                               axis.text = element_text(size = 14, face = "bold"))
 #+ geom_ribbon(alpha = 0.2, colour = NA) +
# Afficher le graphique
print(gg)

#Enregistrer le graphique
ggsave(filename = paste(sp,"png", sep= "."), path = "out/annee_hiver_figures", width = 10, height = 10) 

#Combiner les tableaux de predictions + tendances pour les espèces : 

if (!out_init2) {
  data_pred_all <- data_pred
  data_trend_all <- data_trend
  out_init2 <- TRUE
} else {
  data_pred_all <- rbind(data_pred_all, data_pred, fill = TRUE)
  data_trend_all <- rbind(data_trend_all, data_trend, fill = TRUE)
} } 


write.csv2(data_trend_all,"Data/data_trend_all_1.csv", fileEncoding = "UTF-8")


















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









  

 
    
    
    






vecsp <- unique(data$espece)
# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)

  md <- glmmTMB(abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")
ggmd <- ggpredict(md, terms = c("annee_hiver_txt"))
ref <- ggmd$predicted[1] 
d_pred <- data.frame(annee = ggmd$x,abondance_var = ggmd$predicted / ref)
colnames(d_pred)[2]<- "predicted"
d_pred$group <- "var"

#année numérique : 
md2 <- glmmTMB(abondance ~ annee_hiver + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2")
summary(md2)

ggpredic <- ggpredict(md2, terms = c("annee_hiver"))
colnames(ggpredic)[1] <- "annee"
ggpredic$group <- "trend"
ggpredic <- ggpredic[,-c(3:5)]

tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
trend_raw <-tab_trend_raw[2,1]

trend <- trend_raw
mdIC <- as.data.frame(confint(md2)[,1:2])
colnames(mdIC) <- c("ICinf","ICsup") 
#IC_inf_raw <- mdIC$ICinf[2]
#IC_sup_raw <- mdIC$ICsup[2]
#IC_inf <- IC_inf_raw
#IC_sup <- IC_sup_raw
intercept_raw <- tab_trend_raw[1,1]
intercept <- intercept_raw
tab_trend <- data.frame(trend,p_val=tab_trend_raw[2,4],intercept = intercept)
pval <- tab_trend[1,4]
pval <- paste("pvalue",pval,sep = "=")
d_pred$annee <- as.character(d_pred$annee)
d_pred$annee <- d_pred$annee |> 
  str_replace(fixed("1."), "")


pred <- rbind(ggpredic,d_pred)
setDT(pred)
pred[, annee := as.numeric(as.character(annee))]


 } 



############# Analyse avec le secteur #############








## 1. Secteur x année (étape une par une) : ######
data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)
unique(sort(data$espece))
data1 <- subset(data, espece == "petit_gravelot")
data1 <- subset(data1, !(secteur=="marais_d_orx"))

vecsct <- unique(data1$secteur)
# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsct)) {
  sct <- vecsct[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsct), ") ", sct)

#Premier modèle : 

md1 <- glmmTMB(abondance~annee_hiver_txt + (1|site) + (1|mois_hiver_txt) + (1|obs), data = subset(data1, secteur == sct & site_retenu == "oui"), family = "nbinom2")
summary(md1)

pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
setDT(pred1)
setnames(pred1,"group","secteur")
pred1$secteur <- sct
pred1$group <- "var"

#
init <- pred1 |> filter(x |> str_detect(fixed("1.")))
init <- init$predicted

#Second modèle : 
md2 <- glmmTMB(abondance~annee_hiver + (1|site)  + (1|mois_hiver_txt) + (1|obs) , data = subset(data1, secteur == sct & site_retenu == "oui"), family = "nbinom2")
summary(md2)                       
                       
pred2 <- as.data.frame(ggpredict(md2, terms = c("annee_hiver")))                                                          
setDT(pred2)
setnames(pred2,"group","secteur")
pred2$secteur <- sct
pred2$group <- "trend"
                                  
smd2 <- summary(md2)
est2 <- as.data.frame(smd2$coefficients$cond)[2,]
est2$secteur <- sct

#
pred1$x <- pred1$x |> 
  str_replace(fixed("1."), "")

#Combiner les deux prédictions : 
pred <- rbind(pred1, pred2, fill = TRUE)
pred[, x := as.numeric(as.character(x))]
setDT(pred)

setnames(pred, "x", "year")
pred[, `:=`(predicted = predicted/init)]


if (!out_init) {
  pred_all <- pred
  est_tot <- est2
  out_init <- TRUE
} else {
  pred_all <- rbind(pred_all, pred, fill = TRUE)
  est_tot <- rbind(est_tot, est2)
} } 

write.csv2(est_tot,"Data/estimate_petit_gravelot.csv")


#titre : 
title <- "canard mandarin"


# Créer le graphique avec ggplot2 : 

gg <- ggplot(pred_all, aes(x = year, y = predicted, group = secteur))
gg <- gg + geom_line(data = subset(pred_all,group=="trend"), aes(x = year, y = predicted, color = secteur), size = 1) 
gg <- gg + labs(x = "année",y = "variation d'abondance", title = title, size = 12)
gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                              axis.title = element_text(size = 14, face = "bold"),
                              axis.text = element_text(size = 14, face = "bold"))
                              

print(gg)

ggsave(filename = paste("canard_mandarin","png", sep= "."), path = "out/figure_secteur", width = 10, height = 7) 


#Comparaison tendances avec celles des autres zh : 
#Retirer les espèces qui ne sont pas présentes sur l'estuaire de la Loire : 
data <- subset(data, !(espece =="becasseau_de_temminck"|espece=="becasseau_violet"
                       |espece=="chevalier_stagnatile"|espece=="cygne_de_bewick"
                       |espece=="erismature_rousse"|espece=="garrot_a_oeil_d_or"
                       |espece=="harle_bievre"|espece=="macreuse_brune"
                       |espece=="nette_rousse"|espece=="oie_a_bec_court"
                       |espece=="oie_des_moissons"|espece=="ouette_d_egytpe"
                       |espece=="phalarope_a_bec_etroit"|espece=="phalarope_a_bec_court"
                       |espece=="phalarope_de_wilson"|espece=="pluvier_guignard"
                       |espece=="canard_mandarin"|espece=="becassine_sourde"))

#Transformer la variable estuaire pour qu'elle apparaisse en premier :
data$secteur[data$secteur=="estuaire"] <- "aa_estuaire"
vecsp <- unique(data$espece)
out_init <- FALSE
# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)
  
md <- glmmTMB(abondance~secteur*scale(annee_hiver) + (1|site) + (1|protocole) + (1|mois_hiver_txt) + (1|obs), data = subset(data, espece == "avocette_elegante" & site_retenu=="oui"), family = "nbinom2")
summary(md)

#Récupérer les coefficients : 
smd <- summary(md)
est <- as.data.frame(smd$coefficients$cond)
colnames(est)[4] <- "pvalue"
est$signi <- ifelse(est$pvalue < 0.001, "***", ifelse(est$pvalue < 0.01, "**",
                                                  ifelse(est$pvalue<0.05,"*","NS"))
)
est$code <- sp

#Enregistrer le tableau : 

if (!out_init) {
  tableau_sum <- est
  out_init <- TRUE
} else {
  tableau_sum <- rbind(est, tableau_sum, fill = TRUE)
}
}



vecsct <- unique(data$secteur)
out_init <- FALSE

#Boucle sur chaque secteurs : 
for (isp in 1 : length(vecsct)) {
sct <- vecsct[isp]
    
#data <- subset(data, secteur == "estuaire")
#data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)

vecsp <- unique(data$espece)

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)

#Modèle 1 : 
md1 <- glmmTMB(abondance ~ annee_hiver_txt + (1|site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = subset(data, espece == sp & site_retenu=="oui" & secteur == sct), family = "nbinom2")
summary(md1)

#Modèle 2 : 
md2 <- glmmTMB(abondance ~ scale(annee_hiver) + (1|site) + (1|obs) + (1|mois_hiver_txt) + (1|protocole), data = subset(data, espece == sp & site_retenu== "oui" & secteur == sct), family = "nbinom2")
summary(md2)

#Prediction avec le 1er modèle : 
pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
setDT(pred1)
setnames(pred1,"group","annee_hiver")
pred1[, group := "var"]
#Créer la valeur de référence : 
init <- pred1$predicted[1]
init <- round(init, digits = 7)

#Enlever le 1. 
pred1$x <- pred1$x |> 
  str_replace(fixed("1."), "")

#Prédiction avec le second modèle : 
pred2 <- as.data.frame(ggpredict(md2, terms = list(annee_hiver = seq(min(data$annee_hiver), max(data$annee_hiver), 0.1))))
setDT(pred2)
pred2[, group := "trend"]
smd2 <- summary(md2)
est2 <- as.data.frame(smd2$coefficients$cond)[2,]

#Les intervalles de confiance : 
est_tot <- as.data.frame(confint(md2)) [2,]
colnames(est_tot) <- c("ICinf", "ICsup", "mean")
setDT(est_tot)

# Calculer le nombre d'années dans les données
data_y <- subset(data, espece == sp & site_retenu == "oui" & secteur == sct)

nb_y <- max(data_y[, "annee_hiver"]) - min(data_y[, "annee_hiver"])

# Calculer les pourcentages de variation et les intervalles de confiance
est_tot[, `:=`(var = "year", nb_year = nb_y, pourcent = round(mean * nb_y * 100, 2), pourcent_inf = round(ICinf * nb_y * 100, 2), pourcent_sup = round(ICsup * nb_y * 100, 2))]

#Combiner les deux prédictions : 
pred <- rbind(pred1, pred2, fill = TRUE)
pred[, x := as.numeric(as.character(x))]
setDT(pred)
pred[, `:=`(code = sp, secteur = sct)]

#Renommer la colonne 'x' en 'year' et normaliser les prédictions
setnames(pred, "x", "year")
pred[, `:=`(predicted = predicted/init, conf.low = conf.low/init, conf.high = conf.high/init)]

#pvalue 
pval <- est2[2,4]
pvalue <- paste("pvalue", pval, sep = " = ")

# Définir le titre et le sous-titre du graphique
title <- paste(sct,sp, pvalue, sep = " ")
sub <- paste0(ifelse(est_tot[var == "year", pourcent] > 0, "+ ", "- "), abs(est_tot[var == "year", pourcent]), "%  [", est_tot[var == "year", pourcent_inf], ", ", est_tot[var == "year", pourcent_sup], "] sur ", est_tot[var == "year", nb_year], " ans")

# Définir les couleurs pour les graphiques
col_river <- "#2b8cbe"
col_trend1 <- "#e6550d"
col_trend2 <- "#fdae6b"
vec_col <- c(col_river, col_trend1)
vec_fill <- c(col_river, col_trend2)

# Créer le graphique avec ggplot2
gg <- ggplot(pred, aes(x = year, y = predicted, colour = group, ymin = conf.low, ymax = conf.high, fill = group, group = group))
gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
gg <- gg + labs(x = "", y = "", title = title, subtitle = sub)
gg <- gg + scale_colour_manual(values = vec_col) + scale_fill_manual(values = vec_fill)
gg <- gg + theme_bw() +
  theme(plot.caption = element_text(color = "purple", face = "bold", size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"), legend.position = "none") + guides(fill = "none")

# Afficher le graphique
print(gg)

# Enregistrer le graphique en format PNG et SVG

ggsave(filename = paste(sp,"png", sep= "."), path = "out/figure_secteur", width = 10, height = 10) 


#Enregistrer le tableau avec les tendances : 

tab <- as.data.frame(coef(summary(md2))$cond)
tab$code <- sp
tab$secteur <- sct
if (!out_init) {
  tab_1 <- tab
  out_init <- TRUE
} else {
  tab_all <- rbind(tab, tab_1, fill = TRUE)
}
}}


summary(md2)
    
  
    
   
    
           
 