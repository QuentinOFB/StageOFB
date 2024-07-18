#Chargement des package : 
library(ggplot2)
library(data.table)
library(glmmTMB)
library(ggeffects)
library(dplyr)
library(DHARMa)
library(stringr)

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
  data <- subset(data, present_loire =="oui" & occurence_sp_secteur > 3)  
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

data <- select(data, -c(espece.y,secteur.y))

#Sélectionner les espèces en fonction de leur pertinence : 
data <- subset(data, pertinence == "P"|pertinence=="0")

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

colnames(data) [3] <- "espece"
    
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



class(data$annee_hiver) # -> Integer 
class(data$annee_hiver_txt) # Character 

#Enlever les espèces pour qui ça ne fonctionne pas : 
#data <- subset(data, !(espece == "becasseau_sanderling"|espece=="barge_rousse"|espece=="gravelot_a_collier_interrompu"))
                       #|espece=="becasseau_maubeche"|espece=="canard_chipeau"
                       #|espece=="chevalier_gambette"|espece=="chevalier_sylvain"
                       #|espece=="combattant_varie"|espece=="courlis_corlieu"|espece=="cygne_chanteur"
                       #|espece=="fuligule_milouinan"|espece=="fuligule_morillon"
                       #|espece=="gravelot_a_collier_interrompu"|espece=="harle_huppe"|espece=="huitrier_pie"
                       #|espece=="oie_rieuse"|espece=="petit_gravelot"|espece=="sarcelle_d_ete"))

rm(list = c("crit","tri","d")) 
unique(data$secteur)
data <- subset(data, !(espece=="barge_rousse"|espece=="bernache_cravant"))
               
               #|espece=="cygne_chanteur"|espece=="fuligule_milouinan"|espece=="sarcelle_d_ete"
                       #|espece=="tadorne_casarca"|espece=="barge_rousse"|espece=="becasseau_sanderling"
                       #|espece=="canard_souchet"|espece=="bernache_cravant"|espece=="gravelot_a_collier_interrompu"))

data <- subset(data, !(secteur=="golfe_du_morbihan"|secteur=="moeze_oleron"|secteur=="lac_du_der"))
data <- subset(data, saison == "hivernage")
data <- subset(data, !(espece=="becasseau_minute"|espece =="cygne_chanteur"|espece== "fuligule_milouinan"|espece=="eider_a_duvet"
                       |espece=="fuligule_milouin"|espece=="fuligule_morillon"|espece=="grand_gravelot"|espece=="gravelot_a_collier_interrompu"
                       |espece=="harle_huppe"|espece=="oie_cendree"|espece=="oie_rieuse"|espece=="petit_gravelot"))
data <- subset(data, secteur == "estuaire")
unique(data$espece)
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
  md1 <- glmmTMB(abondance~annee_hiver_txt + (1|secteur/site) + (1|mois_hiver_txt) , data = subset(data, espece == sp & site_retenu=="oui"), family = "nbinom2")
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
  gg <- gg + labs(x = "Année hiver", y = "Variation abondance", title = title, size = 16, subtitle = subtitle)
  gg <- gg + scale_colour_manual(values = vec_col) + scale_fill_manual(values = vec_fill)
  gg <- gg + theme_bw() + theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12),plot.caption = element_text(color = "purple", face = "bold", size = 14),
                               axis.title = element_text(size = 14, face = "bold"),
                               axis.text = element_text(size = 14, face = "bold")) 
  
  #Afficher le graphique : 
  #print(gg)

  #Enregistrer le graphique : 
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/modele_finaux_estuaire", width = 10, height = 10) 
  
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
write.csv2(data_trend,"Data/prediction_espece_modele_final.csv", fileEncoding = "UTF-8")

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
 gg <- gg + labs(x = "Années hiver",y = "Variation d'abondance", title = title, size = 16)
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


write.csv2(pred,"Data/predictionavo2", fileEncoding = "UTF-8")


# Analyse par période pour comparer entre les secteurs :######### 

#Aggrégation par secteur/mois/année/sp/max et nb de sortie :
  #Ajouter un "site" pour le golfe du Morbihan 
    data$site[data$secteur=="golfe_du_morbihan"] <- "gof"
    data$site_retenu[data$secteur=="golfe_du_morbihan"] <- "oui"
    data$date[data$secteur=="golfe_du_morbihan"] <- "2015-01-15"
  
    #Extraire la valeur la plus proche du 15
  data <- data %>%
    group_by(annee, mois) %>%
    mutate(mid_month = as.Date(paste(annee, mois, 15, sep = "-")))
  
  # Calculer la différence en jours avec le milieu du mois
  data <- data %>%
    mutate(diff_mid = abs(as.numeric(difftime(date, mid_month, units = "days"))))
  
  # Sélectionner l'inventaire le plus proche du milieu du mois pour chaque mois
  tab <- data %>%
    group_by(annee, mois, espece, site, secteur) %>%
    filter(diff_mid == min(diff_mid))
    
    verif <- tab %>% 
      group_by(annee, mois, espece, site, secteur) %>% 
      count(mois_hiver_txt)
  
    rm(list = c('crit','data','tri'))
    
    #Somme des abondances pour agréger au secteur : 
    tab <- subset(tab, site_retenu=="oui")
    data_s <- aggregate(tab, abondance ~ espece + secteur + mois_hiver_txt + annee_hiver_txt, sum)
    
    #Rajouter la période + le macro secteur 
    data_s$annee_hiver <- as.numeric(data_s$annee_hiver_txt)
    data_s$mois_hiver <- as.numeric(data_s$mois_hiver_txt)
    
    data_s$periode <- with(data_s, ifelse(annee_hiver >= 2004 & annee_hiver < 2011, "P1",
                                    ifelse(annee_hiver >= 2011 & annee_hiver <2017, "P2","P3")))
    
    #Ajouter le macro_secteur : 
    data_s$macro_secteur <- with(data_s, ifelse(secteur=="camargue","est",
                                          ifelse(secteur=="reserve_du_rhin","est",
                                          ifelse(secteur=="lac_du_der","est","ouest"))))
    
   data_s$saison <- with(data_s, ifelse(mois_hiver >= 1 & mois_hiver < 10, "hivernage","reproduction"))
    
    #Enregistrer jeu de données 
    write.csv2(data_s, "Data/data_15.csv")
    data <- read.csv2("Data/data_15.csv") 
  
    #Obtenir la liste d'espèces :
    vecsp <- unique(data$espece)

  #Initier la sortie du modèle 
  out_init <- FALSE

#Faire une boucle sur chaque espèce : 
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]
  cat("\n\n(",isp, "/", length(vecsp),")",sp)
  
  #Modèle 1 : Evolution des abondances en fonction de la période temporelle : 
  md1 <- glmmTMB(abondance ~ periode*macro_secteur + (1|annee_hiver_txt/mois_hiver_txt) + (1|secteur), data = subset(data_s, espece == sp & saison == "hivernage"), family = "nbinom2")
  summary(md1)
  
  pred1 <- as.data.frame(ggpredict(md1, terms = c("periode","macro_secteur")))

  #Ajouter la colonne espèce au tableau des predictions :
  pred1$sp <- sp 
  colnames(pred1) <- gsub("group","macro_secteur",colnames(pred1))
  
  #Diviser par le nb max d'abondance :
  init <- pred1 %>% group_by(macro_secteur)%>% summarise(init = max(predicted))
  pred <- merge(pred1,init, by = "macro_secteur")
  colnames(pred)[7] <-"init"
  colnames(pred) <- gsub("x","periode",colnames(pred1))
  
  #Divison par la valeur de référence : 
  pred$predicted <- pred$predicted/pred$init
  pred$conf.low <- pred$conf.low/pred$init
  pred$conf.high <- pred$conf.high/pred$init
  
  title <- sp 
  
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
  
  #Second modèle 
  md2 <- glmmTMB(abondance ~ periode*secteur + (1|annee_hiver_txt/mois_hiver_txt), data = subset(data_s, espece == sp & saison == "hivernage"), family = "nbinom2")
  summary(md2)
  #Faire des prédictions :
  pred2 <- as.data.frame(ggpredict(md2, terms = c("secteur","periode")))
  colnames(pred2) <- gsub("x","secteur",colnames(pred2))
  
  colnames(pred2) <- gsub("group","periode",colnames(pred2))
  
  #Diviser par le nb max d'abondance :
  init <- pred2 %>% group_by(secteur)%>% summarise(init = max(predicted))
  pred <- merge(pred2,init, by = "secteur")
  colnames(pred_bis)[7] <-"init"
  pred$sp <- sp
  #Divison par la valeur de référence : 
  pred$predicted <- pred$predicted/pred$init
  pred$conf.low <- pred$conf.low/pred$init
  pred$conf.high <- pred$conf.high/pred$init
  
  gg <- ggplot(pred, mapping = aes(x = periode, y=predicted, colour = secteur))
  gg <- gg + geom_point(position = position_jitterdodge(jitter.width = 0, jitter.height = 0)) 
  gg <- gg + geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_jitterdodge(jitter.width = 0, jitter.height = 0))
  gg <- gg + scale_colour_manual(values = c("arcachon" = "olivedrab2",
                                            "baie_aiguillon" = "darkred",
                                            "baie_de_saint_brieuc" = "darkgoldenrod2",
                                            "camargue" = "lightpink2",
                                            "cotentin" = "royalblue4",
                                            "estuaire" = "deepskyblue2",
                                            "golfe_du_morbihan"="turquoise3",
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
  
  
  }











