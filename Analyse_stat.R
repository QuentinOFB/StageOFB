#Chargement des package : 
library(ggplot2)
library(data.table)
library(glmmTMB)
library(ggeffects)
library(dplyr)
library(DHARMa)
library(stringr)
library(devtools)
library(lubridate)
devtools:::install_github("RetoSchmucki/climateExtract", force = TRUE)

#Ouverture du jeu de données : 
data <- read.csv2("Data/data_VF.csv", header = T)
#Combien d'espèces intialement ? -> 119 
sort(unique(data$espece))
sort(unique(data$saison))
#Tous les secteurs présents ?
unique(data$secteur)

#Mettre l'année Txt et le mois txt en facteur : 
data$annee_hiver_txt <- as.character(data$annee_hiver_txt)
data$mois_hiver_txt <- as.character(data$mois_hiver_txt)

  #Choisir des espèces qui sont présentes à la fois sur l'estuaire et dans plus de 3 secteurs
  data <- subset(data, occurence_sp_secteur > 3)  
  unique(data$espece)
 
 #Tri des espèces : 
 tri <- read.csv2("Data/liste_sp.csv", header = TRUE)
 data <- merge(data, tri, by = "espece")
 data <- subset(data, tri == "Oui")

 #selection des espèce en fonction de leur pertinence : 
 crit <- read.csv("Data/crit.csv", header = TRUE, sep = ";")
  unique(crit$pertinence)
 crit$pertinence[crit$pertinence=="Pertinence sur le secteur"] <- "P"
  
crit[,2] <- tolower(crit[,2])
crit[,2] <- gsub(" ","_",crit[,2])
crit[,2] <- iconv(crit[,2], from = "UTF-8", to = "ASCII//TRANSLIT")
crit[,2] <- gsub("'","_", crit[,2]) 
unique(crit$espece)
crit[,2] <- gsub("garrot_a_oil_d_or","garrot_a_oeil_d_or",crit[,2])
crit$id_pertinence <- paste0(crit$espece,crit$secteur)

data$id_pertinence <- paste0(data$espece,data$secteur)
data <- merge(data,crit, by = "id_pertinence", all.x = TRUE)

data$pertinence[is.na(data$pertinence)] <- 0

colnames(data) <- gsub("espece.x","espece",colnames(data))
colnames(data) <- gsub("secteur.x","secteur",colnames(data))

#Sélectionner les espèces en fonction de leur pertinence : 
data <- subset(data, pertinence == "P"|pertinence=="0")

#Création de l'année de référence : 
d <- aggregate(data, abondance > 0 ~ annee_hiver_txt + espece, sum)
setDT(d)
d[,occ_max := max(`abondance > 0`),by = .(espece)]
d <- subset(d, d$`abondance > 0`== d$occ_max)

d <- d %>% group_by(occ_max,espece) %>% filter(!duplicated(occ_max))

setnames(d,"annee_hiver_txt","annee_hiver_max")

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

colnames(data) [3] <- "espece"
    
########### Analyses statistiques #########

hist(data$abondance, breaks = 1000, xlab = "Valeur d'abondance", ylab="Fréquence", main = "")
title(main="Distribution des valeurs d'abondance") 

data_sup0 <- subset(data, abondance > 0)
hist(data_sup0$abondance, breaks = 1000, xlab = "Valeur d'abondance", ylab="Fréquence", main = "Distribution des valeurs d'abondance > 0")
max(data$abondance)
help("hist")

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



class(data$annee_hiver) # -> Integer 
class(data$annee_hiver_txt) # Character 


rm(list = c("crit","tri","d")) 
unique(data$secteur)

####### Analyse des tendances générales sur le long terme  #########
data <- read.csv2("Data/data_final.csv")
data <- subset(data,!(secteur=="golfe_du_morbihan"|secteur=="moeze_oleron"|secteur=="lac_du_der"))
data <- subset(data, saison == "hivernage")
data <- subset(data, !(espece == "cygne_de_bewick"|espece=="eider_a_duvet"|espece=="erismature_rousse"
                       |espece=="fuligule_milouinan"|espece=="oie_a_bec_court"|espece=="oie_des_moissons"
                       |espece=="becasseau_de_temminck"|espece=="becasseau_violet"
                       |espece=="chevalier_guignette"|espece=="cygne_chanteur"|espece=="fuligule_milouin"
                       |espece=="harle_huppe"|espece=="becasseau_minute"|espece=="macreuse_brune"|espece=="nette_rousse"
                       |espece=="ouette_d_egypte"|espece=="becasseau_cocorli"|espece=="chevalier_sylvain"|espece=="oie_rieuse"))

unique(data$secteur)

data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)
data$mois_hiver_txt <- as.factor(data$mois_hiver_txt)

# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

# Boucle sur chaque espèce
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)
  
  #Modèle 1 (année en facteur)
  md1 <- glmmTMB(abondance~annee_hiver_txt + (1|secteur/site) + (1|mois_hiver_txt), data = subset(data, espece == sp  & site_retenu=="oui"), family = "nbinom2")
  summary(md1)
  
  #Générer les prédictions pour le modèle 1 : 
  pred1 <- ggpredict(md1, terms = c("annee_hiver_txt"))
  pred1 <- as.data.frame(pred1)
  setDT(pred1)
  pred1[, group := "var"]
  
  #Valeur initiale (correspondant au prédict de l'année de référence)
  init <- pred1 |> filter(x |> str_detect(fixed("1.")))
  init <- init$predicted
  
  #Retirer ensuite le 1. 
  pred1$x <- pred1$x |> 
    str_replace(fixed("1."), "")
  
  #Modèle 2 : année en numérique (la tendance)
  md2 <- glmmTMB(abondance~annee_hiver + (1|secteur/site) + (1|annee_hiver_txt/mois_hiver_txt) ,data = subset(data, espece == sp & site_retenu=="oui" ), family = "nbinom2")
  smd2 <- summary(md2)
  
  #Récupérer les estimates : 
  est <- as.data.frame(smd2$coefficients$cond)
  
  #Intervalle de confiance pour le second modèle : 
  est_tot <- as.data.frame(confint(md2))[2,]
  colnames(est_tot) <- c("ICinf", "ICsup", "mean")
  setDT(est_tot)
  
  #Calculer le nombre d'années dans les données 
  data_now <- subset(data, espece== sp & site_retenu=="oui")
  nb_y <- max(data_now[, "annee_hiver"]) - min(data_now[, "annee_hiver"])  
  
  # Calculer les pourcentages de variation et les intervalles de confiance
  est_tot[, `:=`(var = "annee_hiver", nb_year = nb_y, pourcent = round(mean * nb_y * 100, 2), pourcent_inf = round(ICinf * nb_y * 100, 2), pourcent_sup = round(ICsup * nb_y * 100, 2))]
  
  # Générer des prédictions pour le deuxième modèle
  #pred2 <- ggpredict(md2, terms = list(annee_hiver=seq(2004,2024,0.1)))
  #pred2 <- as.data.frame(pred2)
  
  pred2 <- as.data.frame(ggpredict(md2, terms = list(annee_hiver=seq(2004,2024,0.1))))
  setDT(pred2)
  pred2[, group := "trend"]
  
  tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
  trend_raw <- tab_trend_raw[2,1]
  std_raw <- tab_trend_raw[2,2]
  mean_year <- mean(data_now[,"annee_hiver"])
  sd_year <- sd(data_now[,"annee_hiver"])
  
  trend <- exp(trend_raw)
  std <- exp(std_raw)
  mdIC <- as.data.frame(confint(md2)[,1:2])
  colnames(mdIC) <- c("ICinf","ICsup")
  IC_inf_raw <- mdIC$ICinf[2]
  IC_sup_raw <- mdIC$ICsup[2]
  IC_inf <- exp(IC_inf_raw)
  IC_sup <- exp(IC_sup_raw)
  
  tab_trend <- data.frame(nb_year = length(unique(data_now[,"annee_hiver_txt"])), first_year = min(data_now[,"annee_hiver"]), last_year = max(data_now[,"annee_hiver"]), trend, IC_inf ,IC_sup,p_val=tab_trend_raw[2,4], trend_raw,std,std_raw)
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
  pval <- paste("pvalue",tab_trend$p_val)
  
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
 #jpeg(filename = paste(sp, "verif2_jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)

  # Définir le titre et le sous-titre du graphique
  title <- sp
  subtitle <- pval
  
  # Définir les couleurs pour les graphiques
  col_river <- "#2b8cbe"
  col_trend1 <- "#e6550d"
  col_trend2 <- "#fdae6b"
  vec_col <- c(col_river, col_trend1)
  vec_fill <- c(col_river, col_trend2)
  
  # Créer le graphique avec ggplot2
  gg <- ggplot(pred, aes(x = year, y = predicted, colour = group, ymin = conf.low, ymax = conf.high, fill = group, group = group))
  gg <- gg + geom_ribbon(alpha = 0.2, colour = NA) + geom_point(size = 0.8) + geom_line(size = 1.5)
  gg <- gg + labs(x = "Année hiver", y = "Variation abondance", title = title, size = 20)
  gg <- gg + scale_colour_manual(values = vec_col) + scale_fill_manual(values = vec_fill)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 20), legend.text = element_text(size = 12),
                               axis.title = element_text(size = 20, face = "bold"),
                               axis.text = element_text(size = 20, face = "bold")) 
  
  #Afficher le graphique : 
  #print(gg)

  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/modele_finaux_15", width = 15, height = 10) 
  
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

  #Enregistrer le tableau : 
write.csv2(data_trend,"Data/prediction_espece_modele_final_15.csv", fileEncoding = "UTF-8")

####### Analyses des tendances sur le long terme en fonction des secteurs ##################  
  # ATTENTION !!! Bien prendre en compte l'année de référence par espèce et secteurs 
  #Enlever le protocole en effet aléatoire car un seul niveau pour la plupart des secteurs (sauf estuaire)

#Retirer les secteurs der + moeze + golfe 
data <- subset(data,!(secteur=="golfe_du_morbihan"|secteur=="moeze_oleron"|secteur=="lac_du_der"))
unique(data$secteur)

data <- subset(data, saison=="hivernage")
data <- subset(data, !(espece=="avocette_elegante"& secteur=="cotentin"))
data <- subset(data,!(espece=="barge_rousse"& secteur=="estuaire"))
data <- subset(data,!(espece=="barge_rousse"& secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="becasseau_cocorli"& secteur=="baie_de_saint_brieuc"))
data <- subset(data,!(espece=="becasseau_minute"& secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="becasseau_sanderling"& secteur=="estuaire"))
data <- subset(data,!(espece=="becasseau_variable"& secteur=="cotentin"))
data <- subset(data,!(espece=="becasseau_violet"))
data <- subset(data,!(espece=="becassine_des_marais"))
data <- subset(data,!(espece=="becassine_sourde"))
data <- subset(data,!(espece=="bernache_cravant"&secteur=="cotentin"))
data <- subset(data,!(espece=="bernache_cravant"&secteur=="estuaire"))
data <- subset(data,!(espece=="bernache_cravant"&secteur=="marais_d_orx"))
data <- subset(data,!(espece=="bernache_du_canada"))
data <- subset(data,!(espece=="bernache_nonnette"))
data <- subset(data,!(espece=="canard_siffleur"&secteur=="marais_d_orx"))
data <- subset(data,!(espece=="chevalier_aboyeur"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="chevalier_culblanc"&secteur=="baie_de_saint_brieuc"))
data <- subset(data,!(espece=="chevalier_gambette"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="chevalier_sylvain"))
data <- subset(data,!(espece=="combattant_varie"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="cygne_chanteur"))
data <- subset(data,!(espece=="cygne_noir"))
data <- subset(data,!(espece=="eider_a_duvet"))
data <- subset(data,!(espece=="echasse_blanche"))
data <- subset(data,!(espece=="fuligule_milouinan"))
data <- subset(data,!(espece=="fuligule_milouin"&secteur=="camargue"))
data <- subset(data,!(espece=="fuligule_milouin"&secteur=="cotentin"))
data <- subset(data,!(espece=="fuligule_milouin"))
data <- subset(data,!(espece=="fuligule_morillon"))
data <- subset(data,!(espece=="garrot_a_oeil_d_or"))
data <- subset(data,!(espece=="grand_gravelot"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="gravelot_a_collier_interrompu"&secteur=="estuaire"))
data <- subset(data,!(espece=="harle_bievre"))
data <- subset(data,!(espece=="harle_huppe"))
data <- subset(data,!(espece=="huitrier_pie"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="macreuse_brune"))
data <- subset(data,!(espece=="macreuse_noire"))
data <- subset(data,!(espece=="nette_rousse"))
data <- subset(data,!(espece=="oie_a_bec_court"))
data <- subset(data,!(espece=="oie_cendree"&secteur=="marais_d_orx"))
data <- subset(data,!(espece=="oie_des_moissons"))
data <- subset(data,!(espece=="oie_rieuse"))
data <- subset(data,!(espece=="ouette_d_egypte"))
data <- subset(data,!(espece=="petit_gravelot"))
data <- subset(data,!(espece=="sarcelle_d_ete"))
data <- subset(data,!(espece=="tadorne_casarca"))
data <- subset(data,!(espece=="sarcelle_d_hiver"&secteur=="baie_de_saint_brieuc"))
data <- subset(data,!(espece=="tadorne_de_belon"&secteur=="cotentin"))
data <- subset(data,!(espece=="tadorne_de_belon"&secteur=="reserve_du_rhin"))
data <- subset(data,!(espece=="tournepierre_a_collier"&secteur=="reserve_du_rhin"))

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
    md1 <- glmmTMB(abondance ~ annee_hiver_txt + (1|site) + (1|mois_hiver_txt), data = subset(data_1, secteur == sct & espece == sp & site_retenu=="oui"), family = "nbinom2")

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
  md2 <- glmmTMB(abondance ~ annee_hiver + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(data_1, secteur == sct & espece == sp & site_retenu=="oui"), family = "nbinom2")
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
#title <- sp
           
 #gg <- ggplot(subset(data_pred, sp==sp), aes(x = year, y = predicted, group = secteur, colour = secteur))
 #gg <- gg + geom_line(data = subset(data_pred,group=="trend"), aes(x = year, y = predicted, color = secteur), size = 1) 
 #gg <- gg + labs(x = "Années hiver",y = "Variation d'abondance", title = title, size = 16)
# gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                              # axis.title = element_text(size = 14, face = "bold"),
                               #axis.text = element_text(size = 14, face = "bold"))
 #+ geom_ribbon(alpha = 0.2, colour = NA) +
# Afficher le graphique
#print(gg)

#Enregistrer le graphique
#ggsave(filename = paste(sp,"png", sep= "."), path = "out/annee_hiver_figures", width = 10, height = 10) 

#Combiner les tableaux de predictions + tendances pour les espèces : 

if (!out_init2) {
  data_pred_all <- data_pred
  data_trend_all <- data_trend
  out_init2 <- TRUE
} else {
  data_pred_all <- rbind(data_pred_all, data_pred, fill = TRUE)
  data_trend_all <- rbind(data_trend_all, data_trend, fill = TRUE)
} } 



write.csv2(data_pred_all,"Data/PREDICTION_SECTEUR.csv", fileEncoding = "UTF-8")
write.csv2(data_trend_all,"Data/TENDANCES_SECTEUR.csv", fileEncoding = "UTF-8")

colnames(data_pred_all)[8] <- "espece"

#Faire une boucle pour enregistrer les graphiques à la suite : 



vecsp <- unique(data_pred_all$espece)

#Boucle espèce : 
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp) 

data_test <- subset(data_pred_all, espece == sp & group == "trend")

#Ajouter un titre avec le nom de l'espèce :
name_sp <- gsub("_"," ",sp)

#Représentation graphique : 
gg <- ggplot(data = data_test, aes(x = year, y = predicted, colour = secteur))
gg <- gg + geom_line(data_test, mapping = aes(x = year, y = predicted, color = secteur), size = 1) 
gg <- gg + labs(x = "Années hiver",y = "Variation d'abondance", size = 32, title = name_sp)
gg <- gg + theme_bw() + theme(legend.title = element_text(size = 32), legend.text = element_text(size = 20),
axis.title = element_text(size = 32, face = "bold"),
axis.text = element_text(size = 32, face = "bold"))

gg <- gg + scale_colour_manual(values = c(baie_aiguillon = "#F16913", baie_de_saint_brieuc = "#A6CEE3", 
                                          arcachon = "#7F0000", camargue = "#F0E442", cotentin = "#762A83",
                                          estuaire = "#1D91C0", marais_d_orx = "#A6D854", reserve_du_rhin = "#003C30"),labels = c ("baie_aiguillon"="Baie de l'Aiguillon",
                                              "baie_de_saint_brieuc" = "Baie de Saint-Brieuc",
                                              "arcachon" = "Bassin d'Arcachon",
                                              "camargue" = "Camargue",
                                              "cotentin" = "Baie du Cotentin",
                                              "estuaire" = "Estuaire de Loire",
                                              "marais_d_orx" = "Marais d'Orx",
                                              "reserve_du_rhin" = "Réserve du Rhin"))  


# Afficher le graphique
print(gg)

#Enregistrer le graphique
ggsave(filename = paste(sp,"png",sep = "."), path = "out/Figure_secteur_final", width = 20, height = 15)

}


# Les tendances en fonction des macro_secteur :######### 

    #Aggrégation par secteur/mois/année/sp/max et nb de sortie :
    #Ajouter un "site" pour le golfe du Morbihan 
    data$site[data$secteur=="golfe_du_morbihan"] <- "gof"
    data$site_retenu[data$secteur=="golfe_du_morbihan"] <- "oui"
    data$date[data$secteur=="golfe_du_morbihan"] <- "2015-01-15"
  
    #Choisir la saison d'hivernage : 
    data <- subset(data, saison == "hivernage")
    
    #Ne choisir que les sites retenus : 
    data <- subset(data, site_retenu == "oui")
    #Extraire la valeur la plus proche du 15
    data <- data %>%
    group_by(annee, mois) %>%
    mutate(mid_month = as.Date(paste(annee, mois, 15, sep = "-")))
  
    #Calculer la différence en jours avec le milieu du mois
    data <- data %>%
    mutate(diff_mid = abs(as.numeric(difftime(date, mid_month, units = "days"))))
  
    #Sélectionner l'inventaire le plus proche du milieu du mois pour chaque mois
    tab <- data %>%
    group_by(annee, mois, espece, site, secteur) %>%
    filter(diff_mid == min(diff_mid))
  
    #Aggrégé les données pour le golfe et le Der : 
    der <-subset(tab, secteur == "lac_du_der"|secteur=="moeze_oleron")
    der <- aggregate(der, abondance ~ espece + secteur + annee_hiver_txt + mois_hiver_txt, sum)
  
    espece <- read.csv("Data/espece.csv", header = T)
    espece <- espece[-c(98),]
    
    espece[,5] <- tolower(espece[,5])
    espece[,5] <- gsub(" ","_",espece[,5])
    espece[,5] <- gsub("'","_",espece[,5])
    espece[,5] <- iconv(espece[,5],from = "UTF-8",to = "ASCII//TRANSLIT")
    der <- merge(der, espece, by.x = "espece", by.y = "french_name", all.x = T)
    der$site <- der$secteur
    der$annee_hiver <- as.numeric(der$annee_hiver_txt)
    der$mois_hiver <- as.numeric(der$mois_hiver_txt)
  
  #
  tab2 <- subset(tab, !(secteur=="lac_du_der"|secteur=="moeze_oleron"))
  tab3 <- rbind(tab2,der)
    
  #Rajouter la colonne macro secteur : 
  tab3$macro_secteur <- with(tab3, ifelse(secteur=="camargue","sud_est",
                                    ifelse(secteur=="lac_du_der","nord_est",
                                    ifelse(secteur=="reserve_du_rhin","nord_est",
                                    ifelse(secteur=="marais_d_orx","sud_ouest",
                                    ifelse(secteur=="arcachon","sud_ouest",
                                    ifelse(secteur=="baie_aiguillon", "sud_ouest",
                                    ifelse(secteur=="moeze_oleron","sud_ouest","nord_ouest"))))))))
  
  data_zh <- st_read("C:/Users/quentin.petit/Documents/Git/StageOFB/Carto_europe/Localisation_ZH.shp")
  
  data_zh$Nom_site[data_zh$Nom_site=="Estuaire de la Loire"] <- "estuaire"
  data_zh$Nom_site[data_zh$Nom_site=="Golfe du Morbihan"] <- "golfe_du_morbihan"
  data_zh$Nom_site[data_zh$Nom_site=="Baie de Saint Brieuc"] <- "baie_de_saint_brieuc"
  data_zh$Nom_site[data_zh$Nom_site=="Baie du Cotentin"] <- "cotentin"
  data_zh$Nom_site[data_zh$Nom_site=="Baie de l'Aiguillon"] <- "baie_aiguillon"
  data_zh$Nom_site[data_zh$Nom_site=="Moeze oleron"] <- "moeze_oleron"
  data_zh$Nom_site[data_zh$Nom_site=="Bassin d'Arcachon"] <- "arcachon"
  data_zh$Nom_site[data_zh$Nom_site=="Marais d'Orx"] <- "marais_d_orx"
  data_zh$Nom_site[data_zh$Nom_site=="Camargue"] <- "camargue"
  data_zh$Nom_site[data_zh$Nom_site=="Lac du Der"] <- "lac_du_der"
  data_zh$Nom_site[data_zh$Nom_site=="Reserve du Rhin"] <- "reserve_du_rhin"
  
  #Mettre les coordonnées en WGS 84 : 
  
  data_zh <- st_as_sf(data_zh, coords = c("x_L93","y_L93"), crs = 2154)
  data_zh <- st_transform(data_zh, crs = 4326)
  
  data_zh <- data_zh[c(2,10)]
  data_term <- merge(tab3,data_zh, by.x = "secteur", by.y="Nom_site")
  write.csv2(data_term,"Data/data_final.csv")
  
  #Création de l'année de référence : 
  d_sect <- aggregate(tab3, abondance > 0 ~ annee_hiver + espece + macro_secteur, sum)
  setDT(d_sect)
  d_sect[,occ_max := max(`abondance > 0`),by = .(espece,macro_secteur)]
  d_sect <- subset(d_sect, d_sect$`abondance > 0`== d_sect$occ_max)
  d_sect <- d_sect %>% group_by(occ_max,espece,macro_secteur) %>% filter(!duplicated(occ_max))
  
  setnames(d_sect,"annee_hiver","annee_hiver_max")
  setnames(d_sect,"macro_secteur","macro_secteur_max")
  
  
  d_sect$id_max <- paste0(d_sect$espece,d_sect$macro_secteur_max)
  tab3$id_max <- paste0(tab3$espece,tab3$macro_secteur)
  tab3 <- merge(tab3,d_sect,by=("id_max"),all.x = TRUE)
  
  setDT(tab3)
  tab3[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max, paste0("1.",annee_hiver),as.character(annee_hiver))]
  setDF(tab3)
  
  colnames(tab3) <- gsub("espece.x","espece",colnames(tab3))
  
  rm(list = c("crit","d_sect","der","tab","tab2","tri"))
  
  tab3 <- subset(tab3, !(espece=="avocette_elegante" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="barge_a_queue_noire" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="becasseau_variable" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="becasseau_violet"))
  tab3 <- subset(tab3, !(espece=="becasseau_maubeche"& macro_secteur=="nord_est"))
  tab3 <- subset(tab3, ! (espece=="bernache_cravant" & macro_secteur == "nord_est"))
  tab3 <- subset(tab3, !(espece=="bernache_du_canada"))
  tab3 <- subset(tab3, !(espece=="bernache_nonnette"))
  tab3 <- subset(tab3, !(espece=="chevalier_arlequin"))
  tab3 <- subset(tab3, !(espece=="chevalier_gambette"&macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="chevalier_sylvain"))
  tab3 <- subset(tab3, !(espece=="courlis_corlieu" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="cygne_chanteur"))
  tab3 <- subset(tab3, !(espece=="cygne_noir"))
  tab3 <- subset(tab3, !(espece=="eider_a_duvet"))
  tab3 <- subset(tab3, !(espece=="fuligule_milouinan"))
  tab3 <- subset(tab3, !(espece=="fuligule_milouin" & macro_secteur =="sud_est"))
  tab3 <- subset(tab3, !(espece=="fuligule_morillon" & macro_secteur=="sud_est"))
  tab3 <- subset(tab3, !(espece=="grand_gravelot" & macro_secteur == "nord_est"))
  tab3 <- subset(tab3, !(espece=="harle_huppe"))
  tab3 <- subset(tab3, !(espece=="huitrier_pie" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="oie_cendree" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="oie_rieuse" & macro_secteur=="sud_ouest"))
  tab3 <- subset(tab3, !(espece=="petit_gravelot"))
  tab3 <- subset(tab3, !(espece=="pluvier_argente" & macro_secteur=="nord_est"))
  tab3 <- subset(tab3, !(espece=="tadorne_casarca"))
  
  #Création de l'année de référence pour chaque espèce et par macro-secteur : 
  d_sect <- aggregate(tab3, abondance > 0 ~ annee_hiver_txt + espece + macro_secteur, sum)
  setDT(d_sect)
  d_sect[,occ_max := max(`abondance > 0`),by = .(espece,macro_secteur)]
  d_sect <- subset(d_sect, d_sect$`abondance > 0`== d_sect$occ_max)
  d_sect <- d_sect %>% group_by(occ_max,espece,macro_secteur) %>% filter(!duplicated(occ_max))
  
  setnames(d_sect,"annee_hiver_txt","annee_hiver_max")
  setnames(d_sect,"macro_secteur","macro_secteur_max")
  
  
  d_sect$id_max <- paste0(d_sect$espece,d_sect$macro_secteur_max)
  tab3$id_max <- paste0(tab3$espece,tab3$macro_secteur)
  tab3 <- merge(tab3,d_sect,by=("id_max"),all.x = TRUE)
  
  setDT(tab3)
  tab3[,annee_hiver_txt:=ifelse(annee_hiver==annee_hiver_max, paste0("1.",annee_hiver),as.character(annee_hiver))]
  setDF(tab3)
  
  colnames(tab3) [9] <- "espece"

  #Ouvrir le jeu de données : 
  tab3 <- read.csv2("Data/data_final.csv")
  tab3$mois_hiver_txt <- as.factor(tab3$mois_hiver_txt)
  tab3$annee_hiver_txt <- as.factor(tab3$annee_hiver_txt)
  
   #Obtenir la liste d'espèces :
  vecsp <- unique(tab3$espece)
  
  #Initier la sortie du modèle 
  out_init <- FALSE
  out_init2 <- FALSE
  #Faire une boucle sur chaque espèce : 
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")",sp)
    
  #Faire une boucle sur chaque macro secteur : 
  data_1 <- subset(tab3, espece == sp)
  vecsct <- unique(data_1$macro_secteur)
  
  for (isp in 1:length(vecsct)) {
      sct <- vecsct[isp]  
      cat("\n\n (", isp, "/", length(vecsct), ") ", sct)
    
    #Modèle 1 : Variation internanunuelle en fonction des macro_secteur : 
    md1 <- glmmTMB(abondance ~ annee_hiver_txt + (1|mois_hiver_txt) + (1|site) , data = subset(data_1, espece == sp & macro_secteur== sct), family = "nbinom2")
    summary(md1)
    
    pred1 <- as.data.frame(ggpredict(md1, terms = c("annee_hiver_txt")))
    pred1$group <- "var"
    
    #Ajouter la colonne espèce au tableau des predictions :
    pred1$sp <- sp 
    pred1$macro_secteur <- sct
    
    #Variable prédite de référence : 
    init <- pred1 |> filter(x |> str_detect(fixed("1.")))
    init <- init$predicted
    
    #Enlever les 1.
    pred1$x <- pred1$x |> 
    str_replace(fixed("1."), "")
    
    #Diviser par le nb max d'abondance :
    #init <- pred1 %>% group_by(macro_secteur, annee_hiver_txt)%>% summarise(init = max(predicted))
    #pred <- merge(pred1,init, by = "macro_secteur")

    #Modèle de la tendance : 
    md2 <- glmmTMB(abondance~annee_hiver + (1|site) + (1|annee_hiver_txt/mois_hiver_txt) ,data = subset(data_1, espece == sp & macro_secteur == sct), family = "nbinom2")
    smd2 <- summary(md2)
    #Récupérer les estimates : 
    est <- as.data.frame(smd2$coefficients$cond)
    
    #Intervalle de confiance pour le second modèle : 
    est_tot <- as.data.frame(confint(md2))[2,]
    colnames(est_tot) <- c("ICinf", "ICsup", "mean")
    setDT(est_tot)
    
    #Calculer le nombre d'années dans les données 
    data_now <- subset(data_1, espece== sp & macro_secteur==sct)
    nb_y <- max(data_now[, "annee_hiver"]) - min(data_now[, "annee_hiver"])  
    
    # Calculer les pourcentages de variation et les intervalles de confiance
    est_tot[, `:=`(var = "annee_hiver", nb_year = nb_y, pourcent = round(mean * nb_y * 100, 2), pourcent_inf = round(ICinf * nb_y * 100, 2), pourcent_sup = round(ICsup * nb_y * 100, 2))]
    
    # Générer des prédictions pour le deuxième modèle
    #pred2 <- ggpredict(md2, terms = list(annee_hiver=seq(2004,2024,0.1)))
    #pred2 <- as.data.frame(pred2)
    
    pred2 <- as.data.frame(ggpredict(md2, terms = list(annee_hiver=seq(2004,2024,0.1))))
    setDT(pred2)
    pred2[, group := "trend"]
    pred2$sp <- sp
    pred2$macro_secteur <- sct
    
    tab_trend_raw <- as.data.frame(coef(summary(md2))$cond)
    trend_raw <- tab_trend_raw[2,1]
    std_raw <- tab_trend_raw[2,2]
    mean_year <- mean(data_now[,"annee_hiver"])
    sd_year <- sd(data_now[,"annee_hiver"])
    
    trend <- exp(trend_raw)
    std <- exp(std_raw)
    mdIC <- as.data.frame(confint(md2)[,1:2])
    colnames(mdIC) <- c("ICinf","ICsup")
    IC_inf_raw <- mdIC$ICinf[2]
    IC_sup_raw <- mdIC$ICsup[2]
    IC_inf <- exp(IC_inf_raw)
    IC_sup <- exp(IC_sup_raw)
    
    tab_trend <- data.frame(nb_year = length(unique(data_now[,"annee_hiver_txt"])), first_year = min(data_now[,"annee_hiver"]), last_year = max(data_now[,"annee_hiver"]), trend, IC_inf ,IC_sup,p_val=tab_trend_raw[2,4], trend_raw,std,std_raw)
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
    help("rbind")
    
    # Combiner les prédictions des deux modèles
    pred1$x <- as.numeric(pred1$x)
    pred <- bind_rows(pred1, pred2)
    pred$x <- as.numeric(pred$x)
    
    # Renommer la colonne 'x' en 'year' et normaliser les prédictions
    colnames(pred)[1] <- "year"
    setDT(pred)
    pred[, `:=`(predicted = predicted/init, conf.low = conf.low/init, conf.high = conf.high/init)]
    
    tab_trend$sp <- sp
    tab_trend$macro_secteur <- sct 
    
    if (!out_init) {
      data_pred <- pred
      data_trend <- tab_trend
      out_init <- TRUE
    } else {
      data_pred <- rbind(data_pred, pred, fill = TRUE)
      data_trend <- rbind(data_trend, tab_trend, fill = TRUE)
    }}} 
  
  write.csv2(data_trend,"Data/TENDANCE_MACRO_SECTEUR.csv")
  write.csv2(data_pred,"Data/PREDICTION_MACRO_SECTEUR.csv")
  
  #renommer les colonnes : 
  colnames(data_pred)[7] <- "espece"
  colnames(data_pred) [4] <- "IC_INF"
  colnames(data_pred) [5] <- "IC_SUP"
  
  #Transformation en data.frame 
  ggplot_data <- data.frame(year = as.numeric(data_pred$year),
                            predicted = as.numeric(data_pred$predicted),
                            IC_SUP = as.numeric(data_pred$IC_SUP),
                            IC_INF = as.numeric(data_pred$IC_INF),
                            data_pred$macro_secteur)
  
  #Ne prendre que les valeurs des tendances : 
  data_pred2 <- subset(data_pred, group == "trend")
  
  #Boucle pour faire la représentation graphique :  
  vecsp <- unique(data_pred2$espece)
  
  #Initier la sortie du modèle 
  out_init <- FALSE

  #Faire une boucle sur chaque espèce : 
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")",sp)
  
    dt <- subset(data_pred2, espece == sp)
    
    #Représentation graphique : 
    gg <- ggplot(data = dt, aes(x = year, y = predicted, colour = macro_secteur, group = macro_secteur))
    #gg <- gg + geom_ribbon(alpha = 0.4) 
    gg <- gg + geom_line(size = 1.5)
    gg <- gg + scale_colour_manual(name = "Macro-secteur", values = c(
      "nord_est"="#009E73",
      "nord_ouest"="#0072B2",
      "sud_ouest"="#D55E00",
      "sud_est"="#F0E442"), labels = c ("nord_est"="Nord-Est",
                                        "nord_ouest"="Nord-Ouest",
                                        "sud_ouest"="Sud-Ouest",
                                        "sud_est"="Sud-Est"))
  
    #gg <- gg + scale_fill_manual(values = c("nord_est"="#009E73",
                                           # "nord_ouest"="#0072B2",
                                            #"sud_ouest"="#D55E00",
                                            #"sud_est"="#F0E442"), labels = NULL, guide = "none")
    
                      
  gg <- gg + labs(x = "Année hiver", y = "Variation d'abondance", size = 32, title = sp)
  gg <- gg + theme_classic() + theme(legend.title = element_text(size = 32), legend.text = element_text(size = 28),
                                       axis.title = element_text(size = 32, face = "bold"),
                                       axis.text = element_text(size = 32, face = "bold")) 
        
    print(gg)
    
    #Sauvergarder les figures enregistrer : 
    ggsave(filename = paste(sp,"png", sep= "."), path = "out/MACRO_SECTEUR_F", width = 20, height = 15) 
    
  }

  write.csv2(data_pred,"Data/prediection_macro_secteur.csv")
  
  
  
  
    #Somme des abondances pour agréger au secteur : 
    tab <- subset(tab, site_retenu=="oui")
    data_s <- aggregate(tab, abondance ~ espece + secteur + mois_hiver_txt + annee_hiver_txt, sum)
    
    #Rajouter la période + le macro secteur 
    data_s$annee_hiver <- as.numeric(data_s$annee_hiver_txt)
    data_s$mois_hiver <- as.numeric(data_s$mois_hiver_txt)
    
    
    

    
   data_s$saison <- with(data_s, ifelse(mois_hiver >= 1 & mois_hiver < 10, "hivernage","reproduction"))
    
    #Enregistrer jeu de données 
    write.csv2(data_s, "Data/data_15.csv")
    data <- read.csv2("Data/data_15.csv") 
  
    
    
    
    
    
    
    
    data <- subset(data, !(espece=="becasseau_cocorli"|espece=="barge_a_queue_noire"|espece=="barge_rousse"
                           |espece=="echasse_blanche"|espece=="grand_gravelot"
                           |espece=="macreuse_noire"|espece=="becasseau_sanderling"|espece=="becasseau_minute"
                           |espece=="petit_gravelot"|espece=="cygne_chanteur"|espece=="cygne_noir"|espece=="fuligule_milouinan"|espece=="tadorne_casarca"))
    
    #Obtenir la liste d'espèces :
    vecsp <- unique(data$espece)

  #Initier la sortie du modèle 
  out_init <- FALSE

#Faire une boucle sur chaque espèce : 
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]
  cat("\n\n(",isp, "/", length(vecsp),")",sp)
  
  #Modèle 1 : Evolution des abondances en fonction de la période temporelle : 
  md1 <- glmmTMB(abondance ~ periode*macro_secteur + (1|annee_hiver_txt/mois_hiver_txt) + (1|secteur), data = subset(data, espece == sp & saison == "hivernage"), family = "nbinom2")
  summary(md1)
  
  pred1 <- as.data.frame(ggpredict(md1, terms = c("periode","macro_secteur")))

  #Ajouter la colonne espèce au tableau des predictions :
  pred1$sp <- sp 
  colnames(pred1) <- gsub("group","macro_secteur",colnames(pred1))
  
  #Diviser par le nb max d'abondance :
  init <- pred1 %>% group_by(macro_secteur)%>% summarise(init = max(predicted))
  pred <- merge(pred1,init, by = "macro_secteur")
  colnames(pred) <- gsub("x","periode",colnames(pred))
  
  #Divison par la valeur de référence : 
  pred$predicted <- pred$predicted/pred$init
  pred$conf.low <- pred$conf.low/pred$init
  pred$conf.high <- pred$conf.high/pred$init
  
  title <- sp 
  
    if (!out_init) {
    data_pred <- pred
    data_trend <- tab_trend
    out_init <- TRUE
  } else {
    data_pred <- rbind(data_pred, pred, fill = TRUE)
    data_trend <- rbind(data_trend, tab_trend, fill = TRUE)
  } } 
  
  
  gg <- ggplot(pred, mapping = aes(x = periode, y=predicted, colour = macro_secteur))
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_jitterdodge(jitter.width = 0, jitter.height = 0))
  gg <- gg + scale_colour_manual(values = c("est" = "springgreen4",
                                            "ouest" = "red2"))
  gg <- gg + labs(x = "Période", y = "Abondance prédite", title = title, size = 16)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                                axis.title = element_text(size = 14, face = "bold"),
                                axis.text = element_text(size = 14, face = "bold")) 
  
  
  print(gg)
  
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/periodeXmacro_secteur", width = 10, height = 10) 
 
   if (!out_init) {
    data_pred <- pred
  } else {
    data_pred <- rbind(data_pred, pred, fill = TRUE)
  } }
  
  data$mois_hiver_txt <- as.factor(data$mois_hiver_txt)
  data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)
  #Obtenir la liste d'espèces :
  vecsp <- unique(data$espece)
  
  #Initier la sortie du modèle 
  out_init <- FALSE
  
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")",sp)
  #Second modèle 
  md2 <- glmmTMB(abondance ~ periode*secteur + (1|annee_hiver_txt/mois_hiver_txt), data = subset(data, espece == sp & saison == "hivernage"), family = "nbinom2")
  summary(md2)
  #Faire des prédictions :
  pred2 <- as.data.frame(ggpredict(md2, terms = c("secteur","periode")))
  colnames(pred2) <- gsub("x","secteur",colnames(pred2))
  
  colnames(pred2) <- gsub("group","periode",colnames(pred2))
  title <- sp
  #Diviser par le nb max d'abondance :
  init <- pred2 %>% group_by(secteur)%>% summarise(init = max(predicted))
  pred2 <- merge(pred2,init, by = "secteur")
  pred2$sp <- sp
  #Divison par la valeur de référence : 
  pred2$predicted <- pred2$predicted/pred2$init
  pred2$conf.low <- pred2$conf.low/pred2$init
  pred2$conf.high <- pred2$conf.high/pred2$init
  
  gg <- ggplot(pred2, mapping = aes(x = periode, y=predicted, color = secteur))
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_jitterdodge(jitter.width = 0, jitter.height = 0))
  gg <- gg + scale_colour_manual(values = c("arcachon" = "olivedrab2",
                                            "baie_aiguillon" = "darkred",
                                            "baie_de_saint_brieuc" = "darkgoldenrod2",
                                            "camargue" = "lightpink2",
                                            "cotentin" = "royalblue4",
                                            "estuaire" = "deepskyblue2",
                                            "golfe_du_morbihan"="grey",
                                            "lac_du_der"="forestgreen",
                                            "moeze_oleron"="firebrick1",
                                            "reserve_du_rhin"="orangered",
                                            "marais_d_orx"="violetred"))
  gg <- gg + labs(x = "Période", y = "Abondance prédite", title = title, size = 16)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                                axis.title = element_text(size = 14, face = "bold"),
                                axis.text = element_text(size = 14, face = "bold")) 
  
  
  print(gg)
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/periodeXsecteur", width = 10, height = 10) 
  
  if (!out_init) {
    data_pred2 <- pred2
    out_init <- TRUE
  } else {
    data_pred2_f <- rbind(data_pred2, pred2, fill = TRUE)
  } } 

  
  
  
####### Données climatiques ##########  
library(sf)
library(raster)
  
#Créer un identifiant année_mois : 
data$id_an_mois <- paste(data$annee_hiver_txt,data$mois_hiver_txt, sep = "_")
    
#Données climatiques pour le Nord de l'Europe : 
#Ouverture fichier shapefile : 
polygone <- st_read("C:/Users/quentin.petit/Documents/Git/StageOFB/Carto_europe/Couche_Europe_nord_WGS_4326.shp")
plot(polygone)
file
#Chemin vers le fichier
d_dossier <- "Carto/T_min_2020_2021/"

#Liste des fichiers raster 
fichier_tif <- list.files(d_dossier, pattern = "\\.tif$", full.names = TRUE)

#Faire une boucle :
results <- list() 

for (file in fichier_tif) {
  cat(file,"\n")
  #Charge le raster 
  rast <- raster(file)
  
  #extraire les valeurs à l'intérieur du polygone 
  values <- raster::extract(rast, polygone)
  
  #Ajouter les résultats à la liste avec le nom du fichier 
  results[[basename(file)]] <- values
}

#Création d'un data.frame 
results_df <- do.call(rbind.data.frame,results)
results_df <- na.omit(results_df)
results_df$id <- rownames(results_df)
colnames(results_df)[1] <- "temp_min_euN"

#extraire le nom du mois et de l'année 
results_df$match <- str_extract(results_df$id, "\\d{4}-\\d{2}")

results_df <- results_df %>% mutate(annee = as.numeric(sub("-.*","",match)),
                                    mois = as.numeric(sub(".*-","",match)))

setDT(results_df)
results_df[,annee_hiver := ifelse(mois > 7, annee,annee - 1)]
results_df[,annee_hiver_txt := as.character(annee_hiver)]
results_df[,mois_hiver := ifelse(annee == annee_hiver, mois - 7, mois + 5)]
results_df[,mois_hiver_txt := as.character(mois_hiver)]
setDF(results_df)

#Faire la moyenne par mois et année :
results_df$id_annee_mois <- paste(results_df$annee_hiver_txt,results_df$mois_hiver_txt, sep = "_")
dt <- results_df %>% group_by(annee_hiver_txt,mois_hiver_txt) %>% summarise(mean_t_ne = mean(temp_min_euN),
                                                                            sd_t_ne = sd(temp_min_euN))
  
write.csv2(dt,"Data/temp_EuN_2020_2021.csv")

#
dt1 <- read.csv2("Data/temp_EuN_2020_2021.csv")
dt2 <- read.csv2("Data/temp_EuN_2010_2019.csv")
dt3 <- read.csv2("Data/temp_EuN_2000_2009.csv")

dt <- rbind(dt1,dt2,dt3)
dt$id_annee_mois <- paste(dt$annee_hiver_txt,dt$mois_hiver_txt,sep = "_")
write.csv2(dt,"Data/temp_moy_EuN.csv")

#Données climatiques locales :  
library(climateExtract)

data_zh <- st_read("C:/Users/quentin.petit/Documents/Git/StageOFB/Carto_europe/Localisation_ZH.shp")


# Extraire les coordonnées
data_zh$coord <- sub("POINT \\(([^ ]+) ([^ ]+)\\)", "\\1 \\2", data_zh$geometry)

# Séparer les coordonnées en un vecteur numérique
data_zh$x[data_zh$Nom_site=="Estuaire de la Loire"] <- "-2.12337427776802"
data_zh$y[data_zh$Nom_site=="Estuaire de la Loire"] <- "47.2906597639907"

data_zh$x[data_zh$Nom_site=="Golfe du Morbihan"] <- "-2.51893480997207"
data_zh$y[data_zh$Nom_site=="Golfe du Morbihan"] <- "47.5030831172372"

data_zh$x[data_zh$Nom_site=="Baie de Saint Brieuc"] <- "-2.67960628902096"
data_zh$y[data_zh$Nom_site=="Baie de Saint Brieuc"] <- "48.5403310325744"

data_zh$x[data_zh$Nom_site=="Baie du Cotentin"] <- "-1.15464626802122"
data_zh$y[data_zh$Nom_site=="Baie du Cotentin"] <- "49.3994212514426"

data_zh$x[data_zh$Nom_site=="Baie de l'Aiguillon"] <- "-1.17243553517462"
data_zh$y[data_zh$Nom_site=="Baie de l'Aiguillon"] <- "46.2725358701069"

data_zh$x[data_zh$Nom_site=="Moeze oleron"] <- "-1.16377498416456"
data_zh$y[data_zh$Nom_site=="Moeze oleron"] <- "45.9139436120514"

data_zh$x[data_zh$Nom_site=="Bassin d'Arcachon"] <- "-1.18626986302376"
data_zh$y[data_zh$Nom_site=="Bassin d'Arcachon"] <- "44.6745907727148"

data_zh$x[data_zh$Nom_site=="Marais d'Orx"] <- "-1.39718431238666"
data_zh$y[data_zh$Nom_site=="Marais d'Orx"] <- "43.5956835573635"

data_zh$x[data_zh$Nom_site=="Camargue"] <- "-4.554542964878"
data_zh$y[data_zh$Nom_site=="Camargue"] <- "43.5060855593449"

data_zh$x[data_zh$Nom_site=="Lac du Der"] <- "4.74708613098175"
data_zh$y[data_zh$Nom_site=="Lac du Der"] <- "48.5796630886404"

data_zh$x[data_zh$Nom_site=="Reserve du Rhin"] <- "7.7397807285887"
data_zh$y[data_zh$Nom_site=="Reserve du Rhin"] <- "48.4201351464056"

data_zh <- subset(data_zh,!(Nom_site=="Estuaire de la Seine"|Nom_site=="Baie de Somme"|Nom_site=="Baie de Canche"))

data_zh$clim_tmin <- climate_extract(longitude = data_zh$x,
                                     lattitude = data_zh$y,
                                     var = "tmin",
                                     start_date ="2004-09-01",
                                     end_date ="2024-04-30",
                                     output = "data.frame")
library(terra)
help("temporal_aggregate")
fr_boundaries <- st_read("C:/Users/quentin.petit/Documents/Git/StageOFB/Carto_europe/France_wgs_4326.shp")
plot(fr_boundaries)
clim_min_1995_2010 <- extract_nc_value(first_year = 2004,
                                 last_year = 2024,
                                 spatial_extent = fr_boundaries,
                                 sml_chunk = "1995-2010",
                                 clim_variable = "min temp",
                                 statistic = "mean",
                                 grid_size = 0.25,
                                 ecad_v = NULL,
                                 write_raster = TRUE,
                                 out = "raster_min_temp2.tiff",
                                 return_data = TRUE,
                             local_file = FALSE
                              )

point <- data_zh[,c(2,11,12)]
points_sf <- st_as_sf(point, coords = c("x", "y"), crs = 2154)
points_sf <- st_transform(points_sf, crs = 4326)
write.csv2(points_sf,"Data/pointsWGS84.csv")

rbk2 <- terra::rast("raster_tp/raster_min_temp2.tiff")

clim_tmin <- temporal_aggregate(x = rbk2,
                                        y = points_sf,
                                        variable_name = "min temp",
                                        time_step = "monthly",
                                        site_col = "Nom_site")


write.csv2(clim_tmin,"Data/data_locale_tmin_2004_2010.csv")

#Combinaison des deux tableaux de données climatiques : 
clim1 <- read.csv2("Data/data_locale_tmin_2011_2023.csv")
clim2 <- read.csv2("Data/data_locale_tmin_2004_2010.csv")

t_min_locale <- rbind(clim1,clim2)
#Changer les noms des secteurs : 
t_min_locale$site[t_min_locale$site=="Estuaire de la Loire"] <- "estuaire"
t_min_locale$site[t_min_locale$site=="Camargue"] <- "camargue"
t_min_locale$site[t_min_locale$site=="Lac du Der"] <- "lac_du_der"
t_min_locale$site[t_min_locale$site=="Golfe du Morbihan"] <- "golfe_du_morbihan"
t_min_locale$site[t_min_locale$site=="Moeze oleron"] <- "moeze_oleron"
t_min_locale$site[t_min_locale$site=="Reserve du Rhin"] <- "reserve_du_rhin"
t_min_locale$site[t_min_locale$site=="Marais d'Orx"] <- "marais_d_orx"
t_min_locale$site[t_min_locale$site=="Baie de Saint Brieuc"] <- "baie_de_saint_brieuc"
t_min_locale$site[t_min_locale$site=="Bassin d'Arcachon"] <- "arcachon"
t_min_locale$site[t_min_locale$site=="Baie du Cotentin"] <- "cotentin"
t_min_locale$site[t_min_locale$site=="Baie de l'Aiguillon"] <- "baie_aiguillon"

setDT(t_min_locale)
t_min_locale[,annee_hiver := ifelse(month > 7, year,year - 1)]
t_min_locale[,annee_hiver_txt := as.character(annee_hiver)]
t_min_locale[,mois_hiver := ifelse(year == annee_hiver, month - 7, month + 5)]
t_min_locale[,mois_hiver_txt := as.character(mois_hiver)]
setDF(t_min_locale)

t_min_locale$id_sect_temp <- paste(t_min_locale$site,t_min_locale$annee_hiver_txt,t_min_locale$mois_hiver_txt, sep = "_")


write.csv2(t_min_locale,"Data/data_tmin_local.csv")

#Données du NAO : 
NAO <- read.table("Data/norm.nao.monthly.txt")
colnames(NAO)[1] <- "annee"
colnames(NAO)[2] <- "mois"
colnames(NAO)[3] <- "NAO_Index"

#Recréer l'année hiver et le mois hiver pour le jeu de données du NAO :
setDT(NAO)
NAO[,annee_hiver := ifelse(mois > 7, annee,annee - 1)]
NAO[,annee_hiver_txt := as.character(annee_hiver)]
NAO[,mois_hiver := ifelse(annee == annee_hiver, mois - 7, mois + 5)]
NAO[,mois_hiver_txt := as.character(mois_hiver)]
setDF(NAO)

#Créer un identifiant année_mois
NAO$id_annee_mois <- paste(NAO$annee_hiver_txt,NAO$mois_hiver_txt, sep = "_")

#Enregistrer le fichier  
write.csv2(NAO,"Data/NAO.csv")

#Ajout des varibles climatiques dans le jeu de données : 
data <- read.csv2("Data/data_final.csv")
data$id_annee_mois <- paste(data$annee_hiver_txt,data$mois_hiver_txt,sep = "_")
data$id_sect_temp <- paste(data$secteur,data$annee_hiver_txt,data$mois_hiver_txt,sep = "_")

#Ajouter la température locale des secteurs : 
data <- merge(data,t_min_locale, by = "id_sect_temp", all.x = TRUE)
setDT(data)
data[, c('annee_hiver.y','annee_hiver_txt.y','mois_hiver.y','mois_hiver_txt.y','espece.y','secteur.y',
         'X.y','site.y','annee.y','mois.y','macro_secteur.y'):=NULL]
data[, c('X.1.y','X.x','X.1.x'):=NULL]
setDF(data)

colnames(data) <- gsub("macro_secteur.x","macro_secteur",colnames(data))
colnames(data) <- gsub("mois.x","mois",colnames(data))
colnames(data) <- gsub("annee.x","annee",colnames(data))
colnames(data) <- gsub("site.x","site",colnames(data))
colnames(data) <- gsub("annee_hiver.x","annee_hiver",colnames(data))
colnames(data) <- gsub("annee_hiver_txt.x","annee_hiver_txt",colnames(data))
colnames(data) <- gsub("mois_hiver.x","mois_hiver",colnames(data))
colnames(data) <- gsub("mois_hiver_txt.x","mois_hiver_txt",colnames(data))

#Ajouter le NAO : 
NAO <- read.csv2("Data/NAO.csv")
data <- merge(data,NAO,by="id_annee_mois")

#Ajouter les valeurs pour le Nord de l'Europe : 
dt <- read.csv2("Data/temp_moy_EuN.csv")
data <- merge(data,dt,by="id_annee_mois",all.x = TRUE)

#Valeur par macro_secteur :
data$macro_secteur <- with(data, ifelse(secteur=="camargue","sud_est",
                                                              ifelse(secteur=="lac_du_der","nord_est",
                                                                     ifelse(secteur=="reserve_du_rhin","nord_est",
                                                                            ifelse(secteur=="marais_d_orx","sud_ouest",
                                                                                   ifelse(secteur=="arcachon","sud_ouest",
                                                                                          ifelse(secteur=="baie_aiguillon", "sud_ouest",
                                                                                                 ifelse(secteur=="moeze_oleron","sud_ouest","nord_ouest"))))))))

#Calculer la moyenne par Macro_secteur : 
colnames(data)[72] <- "min_temp"

tab <- data %>% group_by(macro_secteur,annee_hiver_txt,mois_hiver_txt) %>% summarise(mean_min_temp = mean(min_temp))

data$id_MS <- paste(data$macro_secteur,data$annee_hiver_txt,data$mois_hiver_txt,sep = "_")
tab$id_MS <- paste(tab$macro_secteur,tab$annee_hiver_txt,tab$mois_hiver_txt,sep = "_")

data <- merge(data,tab,by = "id_MS",all.x = TRUE)

write.csv2(data,"Data/data_final.csv")

######## Modèle avec les données climatiques ########
library(margins)
library(broom.mixed)
library(emmeans)  
library(lsmeans)
library(effects)
library(car)
library(MuMin)

  #Ouvrir le jeu de données final : 
  data <- read.csv2("Data/data_final.csv")
  
  #S'assurer qu'on a bien l'année hiver txt et le mois hiver txt en facteur :
  data$annee_hiver_txt <- as.character(data$annee_hiver_txt)
  data$mois_hiver_txt <- as.character(data$mois_hiver_txt)
  
  #tester la corrélation entre deux variables explicatives : 
  #Coefficient de corrélation : 
  corr <- cor.test(data$mean_min_temp,data$NAO_Index)
  print(corr)
  plot(mean_min_temp~NAO_Index,data, xlab = "NAO", ylab = "Température minimale locale")

  #Tester la corrélation : 
  coor_test <- cor.test(data$NAO_Index,data$mean_t_ne)
  print(coor_test)
  par(mar=c(5,6,4,2))
  plot(mean_t_ne~NAO_Index, data, xlab = "NAO", ylab = "Température minimale moyenne 
       en Europe du Nord")
 
  #Sélectionner uniquement les données sans NA pour les températures minimales : 
  dt1 <- subset(data, !(mean_min_temp=="NA"))

  #Appliquer la fonction Scale 
  setDT(dt1)
  dt1 <- dt1[,`:=`(NAO_sc = scale(NAO_Index), tmin_sc = scale(mean_min_temp))]  
  setDF(dt1)       
         
  ######### MODELE climato #####
  
  #Réaliser une boucle sur plusieurs espèces et sur chaque macro_secteur :
  vecsp <- unique(dt1$espece)
  out_init <- FALSE
  
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")",sp)
  
 
    
  #Modèle 1 : Avec le NAO et température minimale ## 
  md1 <- glmmTMB(abondance ~ NAO_sc*macro_secteur + tmin_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt1, espece == "canard_colvert"), family = "nbinom2")
  sm1 <- summary(md1)
  
  #Extraire les pentes avec emtrends : 
  trend_nao <- emtrends(md1,~macro_secteur, var = "NAO_sc", mode = "response")
  trend_t_min <- emtrends(md1,~macro_secteur,var="tmin_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trends_combined <- as.data.frame(trend_nao)
  trend_combined_tmin <- as.data.frame(trend_t_min)
  
  #Ajouter une colonne pour le type de pente : 
  trends_combined$type <- "NAO"
  trend_combined_tmin$type <- "tmin"
  
  #Ajouter la colonne interaction : 
  trends_combined$interaction <- "macro_secteur:NAO"
  trend_combined_tmin$interaction <- "macro_secteur:tmin"
  
  #renommer la colonne trend : 
  colnames(trends_combined) <- gsub("NAO_sc.trend","trend", colnames(trends_combined))
  colnames(trend_combined_tmin) <- gsub("tmin_sc.trend","trend", colnames(trend_combined_tmin))
  
  #Combiner les deux dataframe : 
  trend_f <- rbind(trends_combined, trend_combined_tmin)
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_f <- sp
  
  #Visualisation des résultats : 
  gg <- ggplot(trend_f, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0), size =2)
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.5, size = 1, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + scale_x_discrete(labels = c("macro_secteur:NAO"="NAO","macro_secteur:tmin"="température minimale hivernale locale"))
  gg <- gg + labs(x = "Variable climatique", y = "Coefficient de pente")
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                           values = c("nord_est"="#009E73",
                                      "nord_ouest"="#0072B2",
                                      "sud_ouest"="#D55E00",
                                       "sud_est"="#F0E442"), 
                       labels = c ("nord_est"="Nord-Est",
                      "nord_ouest"="Nord-Ouest",
                      "sud_ouest"="Sud-Ouest",
                      "sud_est"="Sud-Est"))
   
  print(gg)
  
  #Enregistrer le graphique : 
  ggsave(filename = paste("sp","png",sep = "."), path = "out/md1_NAO_Tmin", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result1 <- emtrends(md1,~macro_secteur,var = "NAO_sc", interaction = TRUE)
  pair <- pairs(result)
  
  result2 <- emtrends(md1,~macro_secteur,var="tmin_sc",interaction = TRUE)
  pair2 <- pairs(result2)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "NAO"
  
  tab_result2 <- as.data.frame(pair2)
  tab_result2$type <- "tp_min_hiv"
  
  #Fusion des deux tableaux : 
  tab_result <- rbind(tab_result1,tab_result2)
  
  #Ajouter l'espèce en question :
  tab_result$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd1 <- as.data.frame(ggpredict(md1, terms = list("NAO_sc" = seq(min(dt1$NAO_sc),max(dt1$NAO_sc), by = 0.1),
                                                    "macro_secteur" = unique(dt1$macro_secteur))))
  
  colnames(predmd1) <- gsub("x","value",colnames(predmd1))
  predmd1$type <- "NAO_sc"
  
  pred2md1 <- as.data.frame(ggpredict(md1, terms = list("tmin_sc" = seq(min(dt1$tmin_sc),max(dt1$tmin_sc), by = 0.1),
                                                         "macro_secteur" = unique(dt1$macro_secteur))))
  colnames(pred2md1) <- gsub("x","value",colnames(pred2md1))
  pred2md1$type <- "tmin_sc"
  pred_MD1 <- rbind(predmd1,pred2md1)
  pred_MD1$modele <- "md1"
  
  #Représentation graphique : 
  gg <- ggplot(data = subset(pred_MD1,type=="NAO_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "NAO",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
  
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md1_NAO", width = 5, height = 3)
  
  gg <- ggplot(data = subset(pred_MD1,type=="tmin_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale locale",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
   
print(gg)
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md1_tmin", width = 5, height = 3)
  
  #Modèle 2 : Température minimale moyenne sur les sites de reproduction et température minimales sur les sites d'hivernage
  
  #Faire un jeu de données ne contenant pas de NA pour les températures : 
  dt2 <- subset(dt1,!(mean_t_ne == "NA"))
  
  #Centrer réduire la variable : 
  setDT(dt2)
  dt2 <- dt2[,`:=`(tmin_NE_sc = scale(mean_t_ne))]
  setDF(dt2)
  
  #Ajustement du modèle 
  md2 <- glmmTMB(abondance ~ tmin_NE_sc*macro_secteur + tmin_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt2, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_tm_NE <- emtrends(md2,~macro_secteur, var = "tmin_NE_sc", mode = "response")
  trend_t_min <- emtrends(md2,~macro_secteur,var="tmin_sc", mode = "response")
 
  #Combiner les résultats des tendances 
  trends_combined_tm_NE <- as.data.frame(trend_tm_NE)
  trend_combined_tmin <- as.data.frame(trend_t_min)
  
  #Ajouter une colonne pour le type de pente : 
  trends_combined_tm_NE$type <- "tp_min_hiv_NE"
  trend_combined_tmin$type <- "tmin"
  
  #Ajouter la colonne interaction : 
  trends_combined_tm_NE$interaction <- "macro_secteur:tp_min_hiv_NE"
  trend_combined_tmin$interaction <- "macro_secteur:tmin"
  
  #renommer la colonne trend : 
  colnames(trends_combined_tm_NE) <- gsub("t_min_NE_sc.trend","trend", colnames(trends_combined_tm_NE))
  colnames(trend_combined_tmin) <- gsub("tmin_sc.trend","trend", colnames(trend_combined_tmin))
  
  #Combiner les deux dataframe : 
  trend_md2 <- rbind(trends_combined_tm_NE, trend_combined_tmin)
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md2 <- sp
  
  #Visualisation des résultats : 
  gg <- ggplot(trend_md2, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "Variable climatique", y = "Coefficient de pente")
  gg <- gg + scale_x_discrete(labels = c("macro_secteur:tp_min_hiv_NE"="Température minimale nord Europe","macro_secteur:tmin"="température minimale hivernale locale"))
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md2_TNE_Tmin", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md2_1 <- emtrends(md2,~macro_secteur,var = "tmin_NE_sc", interaction = TRUE)
  pair <- pairs(result_md2_1)
  
  result_md2_2 <- emtrends(md2,~macro_secteur,var="tmin_sc",interaction = TRUE)
  pair2 <- pairs(result_md2_2)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "tp_min_hiv_NE"
  
  tab_result2 <- as.data.frame(pair2)
  tab_result2$type <- "tp_min_hiv"
  
  #Fusion des deux tableaux : 
  tab_result_md2 <- rbind(tab_result1,tab_result2)
  
  #Ajouter l'espèce en question :
  tab_result_md2$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd2 <- as.data.frame(ggpredict(md2, terms = list("tmin_NE_sc" = seq(min(dt2$tmin_NE_sc),max(dt2$tmin_NE_sc), by = 0.1),
                                                       "macro_secteur" = unique(dt2$macro_secteur))))
  
  colnames(predmd2) <- gsub("x","value",colnames(predmd2))
  predmd1$type <- "tmin_NE_sc"
  
  pred2md2 <- as.data.frame(ggpredict(md2, terms = list("tmin_sc" = seq(min(dt2$tmin_sc),max(dt2$tmin_sc), by = 0.1),
                                                        "macro_secteur" = unique(dt2$macro_secteur))))
  colnames(pred2md2) <- gsub("x","value",colnames(pred2md2))
  pred2md2$type <- "tmin_sc"
  pred_MD2 <- rbind(predmd1,pred2md1)
  pred_MD2$modele <- "md2"
  
  #Représentation graphique : 
  gg <- ggplot(data = subset(pred_MD2,type=="tmin_NE_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale hivernale moyenne dans le Nord de l'Europe",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
   
  print(gg)
  
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md2_tmin_NE", width = 5, height = 3)
  
  gg <- ggplot(data = subset(pred_MD2,type=="tmin_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale locale",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
   
  print(gg)
  
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md2_tmin", width = 5, height = 3)
  
  #modèle 3 : Température minimale dans le nord de l'Europe et NAO
  
  #Faire un jeu de données ne contenant pas de NA pour les températures : 
  dt3 <- subset(data,!(mean_t_ne == "NA"))
  
  #Centrer réduire la variable : 
  setDT(dt3)
  dt3 <- dt3[,`:=`(tmin_NE_sc = scale(mean_t_ne), NAO_sc = scale(NAO_Index))]
  setDF(dt3)
  
  #Ajustement du modèle 
  md3 <- glmmTMB(abondance ~ tmin_NE_sc*macro_secteur + NAO_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt3, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_tm_NE <- emtrends(md3,~macro_secteur, var = "tmin_NE_sc", mode = "response")
  trend_NAO <- emtrends(md3,~macro_secteur,var="NAO_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trend_combined_tm_NE <- as.data.frame(trend_tm_NE)
  trend_combined_NAO <- as.data.frame(trend_NAO)
  
  #Ajouter une colonne pour le type de pente : 
  trend_combined_tm_NE$type <- "tp_min_hiv_NE"
  trend_combined_NAO$type <- "NAO"
  
  #Ajouter la colonne interaction : 
  trends_combined_tm_NE$interaction <- "macro_secteur:tp_min_hiv_NE"
  trend_combined_NAO$interaction <- "macro_secteur:NAO"
  
  #renommer la colonne trend : 
  colnames(trends_combined_tm_NE) <- gsub("t_min_NE_sc.trend","trend", colnames(trends_combined_tm_NE))
  colnames(trend_combined_NAO) <- gsub("NAO_sc.trend","trend", colnames(trend_combined_NAO))
  
  #Combiner les deux dataframe : 
  trend_md3 <- rbind(trends_combined_tm_NE, trend_combined_NAO)
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md3 <- sp
  
  gg <- ggplot(trend_md3, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "Variable climatique", y = "Coefficient de pente")
  gg <- gg + scale_x_discrete(labels = c("macro_secteur:tp_min_hiv_NE"="Température minimale nord Europe","macro_secteur:NAO"="NAO"))
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md3_TNE_NAO", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md3_1 <- emtrends(md3,~macro_secteur,var = "tmin_NE_sc", interaction = TRUE)
  pair <- pairs(result_md3_1)
  
  result_md3_2 <- emtrends(md3,~macro_secteur,var="NAO_sc",interaction = TRUE)
  pair2 <- pairs(result_md3_2)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "tp_min_hiv_NE"
  
  tab_result2 <- as.data.frame(pair2)
  tab_result2$type <- "NAO"
  
  #Fusion des deux tableaux : 
  tab_result_md3 <- rbind(tab_result1,tab_result2)
  
  #Ajouter l'espèce en question :
  tab_result_md3$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd3 <- as.data.frame(ggpredict(md3, terms = list("tmin_NE_sc" = seq(min(dt3$tmin_NE_sc),max(dt3$tmin_NE_sc), by = 0.1),
                                                       "macro_secteur" = unique(dt3$macro_secteur))))
  
  colnames(predmd3) <- gsub("x","value",colnames(predmd3))
  predmd3$type <- "tmin_NE_sc"
  
  pred2md3 <- as.data.frame(ggpredict(md3, terms = list("NAO_sc" = seq(min(dt3$NAO_sc),max(dt3$NAO_sc), by = 0.1),
                                                        "macro_secteur" = unique(dt3$macro_secteur))))
  colnames(pred2md3) <- gsub("x","value",colnames(pred2md3))
  pred2md3$type <- "NAO_sc"
  pred_MD3 <- rbind(predmd3,pred2md3)
  pred_MD3$modele <- "md3"
  
  #Représentation graphique : 
  gg <- ggplot(data = subset(pred_MD3,type=="tmin_NE_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale hivernale moyenne dans le Nord de l'Europe",y = "Valeur prédites", size = 32)
  gg <- gg + theme_minimal() 
  
  print(gg)
  
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md3_tmin_NE", width = 5, height = 3)
  
  gg <- ggplot(data = subset(pred_MD2,type=="NAO_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "NAO",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
   
  #Enregistrer le graphique  
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md3_NAO", width = 5, height = 3)
  
  
  #Modèle 4 : Température minimale locale + Température minimale Nord de l'Europe + NAO
  
  #Ne pas prendre les NA pour les variables climatiques : 
  dt4 <- subset(data, !(mean_t_ne == "NA"& mean_min_temp=="NA"))
  
  #Centrée réduire les variables : 
  setDT(dt4)
  dt4 <- dt4[,`:=`(tmin_NE_sc = scale(mean_t_ne), NAO_sc = scale(NAO_Index), tmin_sc = scale(mean_min_temp))]
  setDF(dt4)
  
  #Ajustement du modèle 
  md4 <- glmmTMB(abondance ~ tmin_NE_sc*macro_secteur + NAO_sc*macro_secteur + tmin_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt4, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_tm_NE <- emtrends(md4,~macro_secteur, var = "tmin_NE_sc", mode = "response")
  trend_t_min <- emtrends(md4,~macro_secteur,var="tmin_sc", mode = "response")
  trend_NAO <- emtrends(md4,~macro_secteur,var="NAO_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trend_combined_tm_NE <- as.data.frame(trend_tm_NE)
  trend_combined_NAO <- as.data.frame(trend_NAO)
  trend_combined_tmin <- as.data.frame(trend_t_min)
  
  #Ajouter une colonne pour le type de pente : 
  trend_combined_tm_NE$type <- "tp_min_hiv_NE"
  trend_combined_NAO$type <- "NAO"
  trend_combined_tmin$type <- "tmin"
  
  #Ajouter la colonne interaction : 
  trends_combined_tm_NE$interaction <- "macro_secteur:tp_min_hiv_NE"
  trend_combined_NAO$interaction <- "macro_secteur:NAO"
  trend_combined_tmin$interaction <- "macro_secteur:tmin"
  
  #renommer la colonne trend : 
  colnames(trends_combined_tm_NE) <- gsub("t_min_NE_sc.trend","trend", colnames(trends_combined_tm_NE))
  colnames(trend_combined_NAO) <- gsub("NAO_sc.trend","trend", colnames(trend_combined_NAO))
  colnames(trend_combined_tmin) <- gsub("tmin_sc.trend","trend", colnames(trend_combined_tmin))
  
  #Combiner les trois dataframe : 
  trend_md4 <- rbind(trends_combined_tm_NE, trend_combined_NAO, trend_combined_tmin)
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md4 <- sp
  
  #Faire la représentation graphique : 
  gg <- ggplot(trend_md4, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "Variable climatique", y = "coefficient de tendance (pente)")
  gg <- gg + scale_x_discrete(labels = c("macro_secteur:tp_min_hiv_NE"="Température minimale nord Europe","macro_secteur:tmin"="température minimale hivernale locale", "macro_secteur:NAO"="NAO"))
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md4_TNE_NAO_tmin", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md4_1 <- emtrends(md4,~macro_secteur,var = "tmin_NE_sc", interaction = TRUE)
  pair <- pairs(result_md4_1)
  
  result_md4_2 <- emtrends(md4,~macro_secteur,var="NAO_sc",interaction = TRUE)
  pair2 <- pairs(result_md4_2)
  
  result_md4_3 <- emtrends(md4,~macro_secteur,var="tmin_sc",interaction = TRUE)
  pair3 <- pairs(result_md4_3)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "tp_min_hiv_NE"
  
  tab_result2 <- as.data.frame(pair2)
  tab_result2$type <- "NAO"
  
  tab_result3 <- as.data.frame(pair3)
  tab_result3$type <- "tmin"
  
  #Fusion des deux tableaux : 
  tab_result_md4 <- rbind(tab_result1,tab_result2,tab_result3)
  
  #Ajouter l'espèce en question :
  tab_result_md4$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd4 <- as.data.frame(ggpredict(md4, terms = list("tmin_NE_sc" = seq(min(dt4$tmin_NE_sc),max(dt4$tmin_NE_sc), by = 0.1),
                                                       "macro_secteur" = unique(dt4$macro_secteur))))
  
  colnames(predmd4) <- gsub("x","value",colnames(predmd4))
  predmd4$type <- "tmin_NE_sc"
  
  pred2md4 <- as.data.frame(ggpredict(md3, terms = list("NAO_sc" = seq(min(dt4$NAO_sc),max(dt4$NAO_sc), by = 0.1),
                                                        "macro_secteur" = unique(dt4$macro_secteur))))
  colnames(pred2md4) <- gsub("x","value",colnames(pred2md4))
  pred2md4$type <- "NAO_sc"
  
  pred3md4 <- as.data.frame(ggpredict(md4, terms = list("tmin_sc" = seq(min(dt4$tmin_sc),max(dt4$tmin_sc), by = 0.1),
                                                        "macro_secteur" = unique(dt4$macro_secteur))))
  colnames(pred3md4) <- gsub("x","value",colnames(pred3md4))
  pred3md4$type <- "NAO_sc"
  
  pred_MD3 <- rbind(predmd3,pred2md3,pred3md4)
  pred_MD3$modele <- "md4"
  
  #Représentation graphique : 
  gg <- ggplot(data = subset(pred_MD4,type=="tmin_NE_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale hivernale moyenne dans le Nord de l'Europe",y = "Valeur prédites")
  gg <- gg + theme_minimal() 
  
  print(gg)
  
  #Enregistrer la figure : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md4_tmin_NE", width = 5, height = 3)
  
  gg <- ggplot(data = subset(pred_MD4,type=="NAO_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "NAO",y = "Valeur prédites")
  gg <- gg + theme_minimal() 
  
  
  #Enregistrer le graphique  
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md4_NAO", width = 5, height = 3)
  
  gg <- ggplot(data = subset(pred_MD4,type=="tmin_sc"), aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "température minimale locale",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
  
  
  #Enregistrer le graphique  
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md4_tmin", width = 30, height = 30)
  
  #Modèle 0 : 
  md0 <- glmmTMB(abondance ~1 + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt2, espece == "canard_colvert"), family = "nbinom2")
  
  #modèle 5 : NAO
  md5 <- glmmTMB(abondance ~ NAO_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(data, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_NAO <- emtrends(md5,~macro_secteur,var="NAO_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trend_combined_NAO <- as.data.frame(trend_NAO)
  
  #Ajouter une colonne pour le type de pente : 
  trend_combined_NAO$type <- "NAO"
  
  #Ajouter la colonne interaction : 
  trend_combined_NAO$interaction <- "macro_secteur:NAO"
  
  #renommer la colonne trend : 
  colnames(trend_combined_NAO) <- gsub("NAO_sc.trend","trend", colnames(trend_combined_NAO))
  
  #Combiner les deux dataframe : 
  trend_md5 <- trend_combined_NAO
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md5 <- sp
  
  gg <- ggplot(trend_md5, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "NAO", y = "Coefficient de pente")
  gg <- gg + scale_x_discrete(labels = c("macro_secteur:NAO"="NAO"))
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md5_NAO", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md5 <- emtrends(md5,~macro_secteur,var = "NAO_sc", interaction = TRUE)
  pair <- pairs(result_md5)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "NAO"
  
  #Fusion des deux tableaux : 
  tab_result_md5 <- tab_result1
  
  #Ajouter l'espèce en question :
  tab_result_md5$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd5 <- as.data.frame(ggpredict(md5, terms = list("NAO_sc" = seq(min(data$NAO_sc),max(data$NAO_sc), by = 0.1),
                                                       "macro_secteur" = unique(data$macro_secteur))))
  
  colnames(predmd5) <- gsub("x","value",colnames(predmd5))
  predmd5$type <- "NAO_sc"
  
  pred_MD5 <- predmd5
  pred_MD5$modele <- "md5"
  
  #Représentation graphique : 
  gg <- ggplot(data = pred_MD5, aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "NAO",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md5_NAO_pred", width = 5, height = 3)
  
  #Modèle 6 : température minimale locale : 
  md6 <- glmmTMB(abondance ~ tmin_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt1, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_t_min <- emtrends(md6,~macro_secteur,var="tmin_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trend_combined_tmin <- as.data.frame(trend_t_min)
  
  #Ajouter une colonne pour le type de pente : 
  trend_combined_t_min$type <- "t_min"
  
  #Ajouter la colonne interaction : 
  trend_combined_NAO$interaction <- "macro_secteur:t_min"
  
  #renommer la colonne trend : 
  colnames(trend_combined_tmin) <- gsub("tmin_sc.trend","trend", colnames(trend_combined_tmin))
  
  #Combiner les deux dataframe : 
  trend_md6 <- trend_combined_tmin
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md6 <- sp
  
  gg <- ggplot(trend_md6, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "Température minimale locale", y = "Coefficient de pente)")
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md6_Tmin", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md6 <- emtrends(md6,~macro_secteur,var = "tmin_sc", interaction = TRUE)
  pair <- pairs(result_md6)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "tmin"
  
  #Fusion des deux tableaux : 
  tab_result_md6 <- tab_result1
  
  #Ajouter l'espèce en question :
  tab_result_md6$esp <- sp 
  
  #Faire une représentation graphique avec les courbes : 
  predmd6 <- as.data.frame(ggpredict(md6, terms = list("tmin_sc" = seq(min(dt1$tmin_sc),max(dt1$tmin_sc), by = 0.1),
                                                       "macro_secteur" = unique(dt1$macro_secteur))))
  
  colnames(predmd6) <- gsub("x","value",colnames(predmd6))
  predmd6$type <- "tmin_sc"
  
  pred_MD6 <- predmd6
  pred_MD6$modele <- "md6"
  
  #Représentation graphique : 
  gg <- ggplot(data = pred_MD6, aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température minimale locale",y = "Abondance prédite")
  gg <- gg + theme_minimal() 
  

  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md6_tmin_pred", width = 5, height = 3)
  
  #Modèle 7 : 
  dt7 <- subset(data, !(mean_t_ne == "NA"))
  
  #Centrée réduire les variables : 
  setDT(dt7)
  dt7 <- dt7[,`:=`(tmin_NE_sc = scale(mean_t_ne))]
  setDF(dt7)
  
  #Ajustement du modèle : 
  md7 <- glmmTMB(abondance ~ tmin_NE_sc*macro_secteur + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt7, espece == "canard_colvert"), family = "nbinom2")
  
  #Extraire les pentes avec emtrends : 
  trend_tmin_NE <- emtrends(md7,~macro_secteur,var="tmin_NE_sc", mode = "response")
  
  #Combiner les résultats des tendances 
  trend_combined_tmin_NE <- as.data.frame(trend_tmin_NE)
  
  #Ajouter une colonne pour le type de pente : 
  trend_combined_tmin_NE$type <- "tmin_NE"
  
  #Ajouter la colonne interaction : 
  trend_combined_tmin_NE$interaction <- "macro_secteur:tmin_NE"
  
  #renommer la colonne trend : 
  colnames(trend_combined_timin_NE) <- gsub("tmin_NE_sc.trend","trend", colnames(trend_combined_tmin_NE))
  
  #Combiner les deux dataframe : 
  trend_md7 <- trend_combined_tmin_NE
  
  #Ajouter une colonne avec le nom de l'espèce : 
  trend_md7 <- sp
  
  gg <- ggplot(trend_md7, aes(x = interaction, y = trend, color = macro_secteur)) 
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0,jitter.height = 0))
  gg <- gg + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + labs(x = "Température minimale moyenne dans le nord de l'Europe", y = "Coefficient de pente")
  gg <- gg + theme_minimal() 
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md7_Tmin_NE", width = 5, height = 3)
  
  #Différence deux à deux des tendances : 
  result_md7 <- emtrends(md7,~macro_secteur,var = "tmin_NE_sc", interaction = TRUE)
  pair <- pairs(result_md7)
  
  #Enregistrer les résultats sous la forme d'un data.frame : 
  tab_result1 <- as.data.frame(pair)
  tab_result1$type <- "tmin_NE"
  
  #Fusion des deux tableaux : 
  tab_result_md7 <- tab_result1
  #Faire une représentation graphique avec les courbes : 
  predmd7 <- as.data.frame(ggpredict(md7, terms = list("tmin_NE_sc" = seq(min(dt7$tmin_sc),max(dt7$tmin_sc), by = 0.1),
                                                       "macro_secteur" = unique(dt7$macro_secteur))))
  
  colnames(predmd7) <- gsub("x","value",colnames(predmd7))
  predmd7$type <- "tmin_NE_sc"
  
  pred_MD7 <- predmd7
  pred_MD7$modele <- "md7"
  
  #Représentation graphique : 
  gg <- ggplot(data = pred_MD7, aes(x = value, y = predicted, color = group)) 
  gg <- gg + geom_line(size = 1)
  gg <- gg + scale_colour_manual(name = "Macro-secteur", 
                                 values = c("nord_est"="#009E73",
                                            "nord_ouest"="#0072B2",
                                            "sud_ouest"="#D55E00",
                                            "sud_est"="#F0E442"), 
                                 labels = c ("nord_est"="Nord-Est",
                                             "nord_ouest"="Nord-Ouest",
                                             "sud_ouest"="Sud-Ouest",
                                             "sud_est"="Sud-Est"))
  gg <- gg + labs(x = "Température hivernale minimale moyenne dans le nord de l'Europe",y = "Valeur prédites")
  gg <- gg + theme_minimal() 
  
  
  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png",sep = "."), path = "out/md7_tmin_NE_pred", width = 5, height = 3)
  
  #Ajouter l'espèce en question :
  tab_result_md7$esp <- sp 
  
  ## Fusion des tableaux de pentes : 
  trend_MDF <- rbind(trend_md1,trend_md2,trend_md3,trend_md4,trend_md5,trend_md6,trend_md7)
  
  ##Fusion des tableaux de comparaisons : 
  result_pair_F <- rbind(tab_result_md1,tab_result_md2,tab_result_md3,tab_result_md4,tab_result_md5,tab_result_md6,tab_result_md7)
  
  ## Comparaison des AIC : 
  AIC_value <- as.data.frame(AIC(md1,md2,md3,md4,md5,md6,md7,md0))
 
  AIC_value$esp <- sp 
  
  #Fusion des prédictions : 
  pred_MDF <- rbind(pred_MD1,pred_MD2,pred_MD3,pred_MD4,pred_MD5,pred_MD6,pred_MD7)
  
  
  
  
  #Calcul des estimates : 
  coeff <- tidy(md1, effects = "fixed")
  
  pente_NAO <-  coeff%>% filter(grepl("NAO_sc",term)) 
  pente_timin <- coeff%>%filter(grepl("tmin_sc",term))
  pente_macro <- coeff%>%filter(grepl("macro_secteur",term))
  
  NAO_interaction <- pente_NAO[2,4]
  NAO_interaction <- NAO_interaction$estimate
  t_min_interaction <- pente_timin[2,4]
  t_min_interaction <- t_min_interaction$estimate
  macro_secteur <- pente_macro[1,4]
  macro_secteur <- macro_secteur$estimate
  
  Intercept <- coeff[1,4]
  Intercept <- Intercept$estimate
  
  ic <- as.data.frame(confint(md1)[,1:2])
  term <- row.names(ic)
  ic_NAO <-  ic%>% filter(grepl("NAO_sc",term)) 
  ic_tmin <- ic%>%filter(grepl("tmin_sc",term))
  ic_macro <- ic%>%filter(grepl("macro_secteur",term))
  
  ic_ref_inf <- ic[1,1]
  ic_ref_sup <- ic[1,2]
  
  #NAO 
  nao_ref <- Intercept - ((Intercept-t_min_interaction)+(Intercept-macro_secteur))
  
  pente_NAO <- pente_NAO %>% 
    mutate(p = ifelse(grepl(":",term), estimate + nao_ref,estimate))
  
    #Ajouter la valeur de référence : 
    mc_ref <- head(sort(unique(dt1$macro_secteur)),1)  
  
    nao_tab_raw <- data.frame(macro_sect = mc_ref,
                                 p = nao_ref, 
                               term = paste0("NAO_sc:macro_secteur",mc_ref))
 
      pente_NAO <- pente_NAO %>% 
     mutate(macro_sect = str_extract(term, "(?<=:).*"))
  
      #Intervale de confiance : 
      ic <- as.data.frame(confint(md1)[,1:2])
      nao_inf <- ic_ref_inf - ((ic_ref_inf-ic_tmin[2,1])+(ic_ref_inf-ic_macro[2,1]))
      nao_sup <- ic_ref_sup - ((ic_ref_sup-ic_NAO[2,2])+(ic_ref_sup-ic_macro[2,2]))
   
      colnames(ic_NAO)[1] <- "ic_inf"
      colnames(ic_NAO)[2] <- "ic_sup"
      ic_NAO$term <- row.names(ic_NAO)
      tab_NAO <- merge(pente_NAO,ic_NAO, by = "term",all.x = TRUE)
      
      #Calcul des IC 
      tab_NAO <- tab_NAO %>% 
        mutate(IC_SUP = ifelse(grepl(":",term), ic_sup + nao_sup, ic_sup),
              IC_INF = ifelse(grepl(":",term), ic_inf + nao_inf, ic_inf))
      
      
      nao_tab_raw <- data.frame(nao_tab_raw,IC_SUP=nao_sup,IC_INF=nao_inf)
      
      #Fusion des deux tableaux : 
      tab_NAO <- bind_rows(nao_tab_raw,tab_NAO)
      
      #Rajouter la colonne valeurs : 
      tab_NAO$valeur <- "NAO"
   
      #Rajouter ensuite la colonne espèce : 
      tab_NAO$espece <- sp 
      
      #Rajouter s'il s'agit ou non de l'interaction : 
      tab_nao$inter <- ifelse(grepl(":",term),"macrosecteur:NAO_sc","NAO_sc")
      
      
      
      
      
      
      
      
      
      
      #Utiliser emmeans pour effectuer des comparaisons de pentes :
    #Pour le NAO : 
  emm_NAO <- emmeans(md1, ~ macro_secteur * NAO_sc + macro_secteur * tmin_sc)
  emm_NAO_df <- as.data.frame(emm_NAO)
  pairs(emm_results_NAO)
  
  
  
    #Pour les températures minimales : 
  emm_t_min <- emmeans(md1, ~ macro_secteur * tmin_sc)
  emm_tmin_df <- as.data.frame(emm_t_min)
  
  pairs(emm_results_t_min)
  
  #Extraire les coefficients du modèle : 
  library(broom.mixed)
  
  coef_results <- tidy(md1, effects = "fixed")
  

  
  
  #Extraire les valeurs de pente dans l'intercept :
  coefficients <- fixef(md1)$cond  
  
    #Les valeurs de NAO : 
  naos <- as.data.frame(coefficients[grep("^NAO_sc",names(coefficients))])
      #Valeur de NAO à soustraire (celle dans la première interaction)
        inter_nao <- naos[2,1] 
 
    #Les valeurs de températures minimales : 
  tmin <- as.data.frame(coefficients[grep("tmin_sc",names(coefficients))])      
        #Valeur de tmin à soustraire (celle de la première interaction)
          inter_tmin <- tmin[2,1]
        
    #Les valeurs des macro_secteurs :
  mac <- as.data.frame(coefficients[grep("macro_secteur",names(coefficients))])        
    #Valeur de l'estimate à soustraire :
        inter_mac <- mac[1,1]
          
    #Calcul des estimates pour les interactions macro_secteur*NAO et macro_secteur*tmin
        # interaction = Intercept - ((intercept - inter_mac) + (intercept - inter_tmin))
        
        mc_nao <- intercept - ((intercept - inter_mac) + (intercept - inter_tmin))
        
        
        
        sum1 <- as.data.frame(coef(summary(md1))$cond)
  
  coefficients <- fixef(md1)$cond
  
  #Coefficient de l'intercept : 
  intercept <- coefficients["(Intercept)"]
  
  naos <- coefficients[grep("^NAO_sc",names(coefficients))]
  macro <- coefficients[grep("macro_secteur",names(coefficients))]
  
  ic <- as.data.frame(confint(md1)[,1:2])
    
  #Calcul des estimates :
  
  
  #Comparaisons des tendances : 
  library(emmeans)
  library(lmeans)
  em <- emmeans(md1, ~ macro_secteur|NAO_sc)
  comp <- pairs(em)
  summary(comp)
  
  

  summmd1 <- as.data.frame(coef(summary(md1))$cond)
  intercept <- summmd1[1,1]
  macro_sct <- summmd1[3,1]
  NAO_macro_sct <- summmd1
  
  #Prediction du modèle : 
  pred1 <- data.frame(ggpredict(md1))
  plot(ggpredict(md1))
  
  #Résumé du modele :
  md1_sum <- summary(md1)
  md1_coef <- coef(summary(md1))
  
  #Sortir l'AIC du modèle : 
  AIC_tab <- data.frame(md1_sum$AICtab)
  AIC_tab <- AIC_tab[1,]
  
  #Créer un data.frame renseignant l'AIC, le nom du modèle, de l'espèce et du macro_secteur
  AIC_tab <- data.frame(AIC_tab)
  AIC_tab$modele <- "md1"
  AIC_tab$espece <- sp
  AIC_tab$macro_secteur <- ms
  
  #Créer un data frame avec les valeurs de pentes 
  tab_trend <- as.data.frame(coef(summary(md1))$cond)
  tab_trend <- tab_trend[c(2,3),]
  tab_trend$var_explicative <- rownames(tab_trend)
  tab_trend$sp <- sp 
  tab_trend$macro_secteur <- ms
  
  #Prédiction du modèle
  pred1 <- data.frame(ggpredict(md1, terms = list(NAO_sc=seq(min(dt1$NAO_sc),max(dt1$NAO_sc),by = 0.01))))
  pred2 <- data.frame(ggpredict(md1, terms = list(tmin_sc=seq(min(dt1$tmin_sc),max(dt1$tmin_sc),by = 0.01))))

  colnames(pred1)[1] <- "NAO_sc" 
  colnames(pred2)[1] <- "tmin_sc" 

  #Ajouter colonne espèce 
  pred1$sp <- sp
  pred2$sp <- sp

  #Ajouter colonne macro_secteur 
  pred1$macro_secteur <- ms
  pred2$macro_secteur <- ms

  #Décentrer réduire les variables explicatives : 
  sd_NAO <- sd(dt1$NAO_Index)
  sd_tmin <- sd(dt1$mean_min_temp)
  
  Estimate_origin_NAO <- exp(tab_trend[1,1])^(1/sd_NAO)
  Estimate_origin_tmin <- exp(tab_trend[2,1])^(1/sd_tmin)
  
  std_origin_NAO <- exp(tab_trend[1,2])^(1/sd_NAO)
  std_origin_tmin <- exp(tab_trend[2,2])^(1/sd_tmin)
    
  #Ajouter les pentes non standardisés au tableau : 
  tab_trend$pente_original <- ""
  tab_trend$pente_original[tab_trend$var_explicative=="NAO_sc"] <- Estimate_origin_NAO
  tab_trend$pente_original[tab_trend$var_explicative=="tmin_sc"] <- Estimate_origin_tmin
  
  tab_trend$std.error_original <- ""
  tab_trend$std.error_original[tab_trend$var_explicative=="NAO_sc"] <- std_origin_NAO
  tab_trend$std.error_original[tab_trend$var_explicative=="tmin_sc"] <- std_origin_tmin
  
  #Fusionner les jeux de données : 
  if(!out_init){
    data_NAO <- pred1
    data_tmin <- pred2
    trend_data <- tab_trend
    Tab_AIC <- AIC_tab
    out_init <- TRUE
  } else { 
    data_NAO <- bind_rows(data_NAO,pred1)
    data_tmin <- bind_rows(data_tmin, pred2)
    trend_data <- bind_rows(trend_data, tab_trend)
    Tab_AIC <- bind_rows(Tab_AIC, AIC_tab) 
    }}} 
  
  
  #Enregistrement des tableaux de données : 
  write.csv2(data_NAO,"Data/data_NAO_md1.csv")
  write.csv2(data_tmin, "Data/data_tmin_md1.cvs")
  write.csv2(Tab_AIC, "Data/data_AIC_md1.csv")
  write.csv2(trend_data, "Data/trend_md1")
  
  palette.colors(palette = "Okabe-Ito")
  #Palette de couleur à utiliser qui soient colorblind friendly :
  nord_est <- "#009E73"
  nord_ouest <- "#0072B2"
  sud_est <- "#F0E442"
  sud_ouest <- "#D55E00"
  
  colnames(data_NAO)[7] <- "espece"
  colnames(trend_data)[6] <- "espece"
  colnames(data_tmin)[7] <- "espece"
  
  #Faire une boucle : 
  vecsp <- unique(data_NAO$espece)
  
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")", sp)
  
  #Paramètres graphiques : 
  name_sp <- gsub("_"," ",sp)
  title <- name_sp   
  
  dgg <- subset(data_NAO, espece == sp)
    
  #Représentation graphique : 
  gg <- ggplot(data = dgg, mapping = aes(x = NAO_sc, y = predicted, color = macro_secteur, group = macro_secteur))                      
  gg <- gg + geom_line(data = dgg, mapping = aes(x = NAO_sc, y = predicted, group = macro_secteur), size = 1) 
  gg <- gg + labs(x = "Indince d'oscilation", y = "Variation d'abondance", size = 16, color = "Macro secteur", title = title)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 25), legend.text = element_text(size = 12),
                                axis.title = element_text(size = 20, face = "bold"),
                                axis.text = element_text(size = 14, face = "bold"))
  gg <- gg + scale_colour_manual(values = c(nord_est = "#009E73", nord_ouest = "#0072B2",
                                                       sud_est = "#F0E442", sud_ouest = "#D55E00"))
  
  print(gg)
  
  #Enregistrer les figures dans un dossier : 
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/md1_NAO_var_ab", width = 20, height = 20) 
  
  }
  
 
  ############### MODELE 2 ##############
  
  #Sélectionner uniquement les données sans NA pour les températures minimales : 
  dt2 <- subset(data, !(mean_t_ne=="NA"))
  
  #Appliquer la fonction Scale 
  setDT(dt2)
  dt2 <- dt2[,`:=`(t_ne_sc = scale(mean_t_ne), tmin_sc = scale(mean_min_temp))]  
  setDF(dt2)
  
  vecsp <- unique(dt2$espece)
  out_init <- FALSE
  
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")",sp)
    
    #Boucle sur chaque macro_secteur : 
    dt_ms <- subset(dt2, espece == sp)
    vecms <- unique(dt_ms$macro_secteur)
    for (ims in 1:length(vecms)) {
      ms <- vecms[ims]
      
    #Modèle 2 : Avec les températures minimales moyenne dans le nord de l'Europe : 
    md2 <- glmmTMB(abondance ~ t_ne_sc + tmin_sc + (1|site) + (1|annee_hiver_txt/mois_hiver_txt), data = subset(dt2, espece == sp & macro_secteur== ms), family = "nbinom2")
    summary(md2)
      
      #Résumé du modele :
      md2_sum <- summary(md2)
      md2_coef <- coef(summary(md2))
      
      #Sortir l'AIC du modèle : 
      AIC_tab <- data.frame(md2_sum$AICtab)
      AIC_tab <- AIC_tab[1,]
      
      #Créer un data.frame renseignant l'AIC, le nom du modèle, de l'espèce et du macro_secteur
      AIC_tab <- data.frame(AIC_tab)
      AIC_tab$modele <- "md2"
      AIC_tab$espece <- sp
      AIC_tab$macro_secteur <- ms
      
      #Créer un data frame avec les valeurs de pentes 
      tab_trend <- as.data.frame(coef(summary(md2))$cond)
      tab_trend <- tab_trend[c(2,3),]
      tab_trend$var_explicative <- rownames(tab_trend)
      tab_trend$sp <- sp 
      tab_trend$macro_secteur <- ms
      
      #Prédiction du modèle
      pred1 <- data.frame(ggpredict(md2, terms = list(t_ne_sc=seq(min(dt2$t_ne_sc),max(dt2$t_ne_sc),by = 0.01))))
      pred2 <- data.frame(ggpredict(md2, terms = list(tmin_sc=seq(min(dt2$tmin_sc),max(dt2$tmin_sc),by = 0.01))))
      
      colnames(pred1)[1] <- "t_ne_sc" 
      colnames(pred2)[1] <- "tmin_sc" 
      
      #Ajouter colonne espèce 
      pred1$espece <- sp
      pred2$espece <- sp
      
      #Ajouter colonne macro_secteur 
      pred1$macro_secteur <- ms
      pred2$macro_secteur <- ms
      
      #Décentrer réduire les variables explicatives : 
      sd_t_ne <- sd(dt2$t_ne_sc)
      sd_tmin <- sd(dt2$mean_min_temp)
      
      Estimate_origin_t_ne <- exp(tab_trend[1,1])^(1/sd_t_ne)
      Estimate_origin_tmin <- exp(tab_trend[2,1])^(1/sd_tmin)
      
      std_origin_t_ne <- exp(tab_trend[1,2])^(1/sd_t_ne)
      std_origin_tmin <- exp(tab_trend[2,2])^(1/sd_tmin)
      
      #Ajouter les pentes non standardisés au tableau : 
      tab_trend$pente_original <- ""
      tab_trend$pente_original[tab_trend$var_explicative=="t_n_sc"] <- Estimate_origin_t_ne
      tab_trend$pente_original[tab_trend$var_explicative=="tmin_sc"] <- Estimate_origin_tmin
      
      tab_trend$std.error_original <- ""
      tab_trend$std.error_original[tab_trend$var_explicative=="t-n_sc"] <- std_origin_t_ne
      tab_trend$std.error_original[tab_trend$var_explicative=="tmin_sc"] <- std_origin_tmin
      
      #Fusionner les jeux de données : 
      if(!out_init){
        data_t_ne <- pred1
        data_tmin <- pred2
        trend_data <- tab_trend
        Tab_AIC <- AIC_tab
        out_init <- TRUE
      } else { 
        data_t_ne <- bind_rows(data_t_ne,pred1)
        data_tmin <- bind_rows(data_tmin, pred2)
        trend_data <- bind_rows(trend_data, tab_trend)
        Tab_AIC <- bind_rows(Tab_AIC, AIC_tab)
      }}} 
  
  #Enregistrement des tableaux de données : 
  write.csv2(data_t_ne,"Data/data_t_ne_md2.csv")
  write.csv2(data_tmin, "Data/data_tmin_md2.cvs")
  write.csv2(Tab_AIC, "Data/data_AIC_md2.csv")
  write.csv2(trend_data, "Data/trend_md2") 
 
  #Faire une boucle : 
  vecsp <- unique(data_tmin$espece)
  
  for (isp in 1:length(vecsp)) {
    sp <- vecsp[isp]
    cat("\n\n(",isp, "/", length(vecsp),")", sp)
    
    #Paramètres graphiques : 
    name_sp <- gsub("_"," ",sp)
    title <- name_sp   
    
    dgg <- subset(data_tmin, espece == sp)
    
    #Représentation graphique : 
    gg <- ggplot(data = dgg, mapping = aes(x = tmin_sc, y = predicted, color = macro_secteur, group = macro_secteur))                      
    gg <- gg + geom_line(data = dgg, mapping = aes(x = tmin_sc, y = predicted, group = macro_secteur), size = 1) 
    gg <- gg + labs(x = "Température minimale locale", y = "Variation d'abondance", size = 16, color = "Macro secteur", title = title)
    gg <- gg + theme_bw() + theme(legend.title = element_text(size = 25), legend.text = element_text(size = 12),
                                  axis.title = element_text(size = 20, face = "bold"),
                                  axis.text = element_text(size = 14, face = "bold"))
    gg <- gg + scale_colour_manual(values = c(nord_est = "#009E73", nord_ouest = "#0072B2",
                                              sud_est = "#F0E442", sud_ouest = "#D55E00"))
    
    print(gg)
    
    #Enregistrer les figures dans un dossier : 
    ggsave(filename = paste(sp,"png", sep= "."), path = "out/md2_tmin_var_ab", width = 20, height = 20) 
    
  }
