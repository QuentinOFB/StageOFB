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

data <- read.csv2("Data/data_clean.csv", header = T)

#Données non aggrégate : 
data <- read.csv2("Data/data_clean_nonagglo.csv", header = T)

class(data$annee_hiver)
class(data$annee_hiver_txt)


####### 1. L'année en facteur : 
data$annee_hiver_txt <- as.factor(data$annee_hiver_txt)

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
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2"))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  help("glmmTMB")
  
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
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      d_out_txt <- ggmd
      out_init <- TRUE
    } else {
      d_out_txt <- rbind(d_out_txt, ggmd, fill = TRUE)
    } } }

summary(md)

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
verif <- simulateResiduals(fittedModel = md_avo, plot = F)
testZeroInflation(verif)

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
  data_ref$year <- sort(data_ref$year)
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


#Représentation graphique : 
  # Une boucle ? 

out_init <- FALSE
vecsp <- unique(d_tp_txt$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)

gg <- ggplot(data = subset(d_tp_txt, code == sp), mapping=aes(x=annee, y=abondance_var))
gg <- gg + geom_line()
gg <- gg + geom_pointrange(aes(ymin = ICinf, ymax=ICsup))
gg <- gg + labs(y="Variation d'abondance",x="Années", title = sp) 


ggsave(filename = paste(sp,"png", sep= "."), path = "out/Variation_annee", width = 10, height = 10) 

#jpeg(paste(names(setosa)[i], "jpeg", sep = "."), width = 15, height =12, units="cm", quality=75, res=300)

print(gg)

}

#Borne intervalle de confiance qui sont énormes ? 

###### Partie 2 : l'année en numérique ###########

class(data$annee_hiver)

# Obtenir une liste unique des espèces dans la colonne 'espece' du dataframe 'data'
vecsp <- unique(data$espece)

# Initialiser la variable de sortie
out_init <- FALSE

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
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver > 2003 & site_retenu=="oui"), family = "nbinom2"))
  
  #> Sélection les années à partir de 2004 
  #> Sélection des sites retenus (+ de 3 saisons suivies)
  
  help("glmmTMB")
  
  #Négative binomiale et pas poisson ?  
  
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
  
  
  ggsave(filename = paste(sp,"png", sep= "."), path = "out/Variation_annee", width = 10, height = 10) 
  
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

              