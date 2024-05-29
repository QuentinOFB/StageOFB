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

data$annee_hiver_txt <- as.character(data$annee_hiver_txt)

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
  md <- try(glmmTMB(form, subset(data, espece == sp & annee_hiver_txt > 2003 & site_retenu=="oui", family = "nbinom2")))
  
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
      d_out_fact <- ggmd
      out_init <- TRUE
    } else {
      d_out_fact <- rbind(d_out, ggmd, fill = TRUE)
    } } }

#Si on veut enregistrer d_Out (pour évitier d'avoir à faire retourner le modèle) 
#L'année en numérique :
write.csv2(d_out,"Data/d_out.csv")
#L'année en character : 
write.csv2(d_out_fact, "Data/d_out_fact.csv")

#Mod pour avocette élégante 
md_avo <- glmmTMB(abondance ~ annee_hiver_txt + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt), data = subset(data, espece == "avocette_elegante"& annee_hiver_txt>2003& site_retenu=="oui"), family = "nbinom2")
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


out_init <- FALSE
vecsp <- unique(d_out_fact$code)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle

  data_ref <- subset(d_out_fact, code == sp)
  ref <- data_ref$predicted[1]
  d_pred <- data.frame(annee = data_ref$year, abondance_var =  data_ref$predicted / ref, ICinf =  data_ref$conf.low/ref , ICsup =  data_ref$conf.high/ref)

  setDT(d_pred)
  
  # Ajouter des colonnes supplémentaires
  d_pred[, `:=`(code = sp)]  
  
  if (!out_init) {
    d_out_tempo_fact <- d_pred
    out_init <- TRUE
  } else {
    d_out_tempo_fact <- rbind(d_out_tempo_fact, d_pred, fill = TRUE)
  } } 
  
#Pour l'année en numérique :
write.csv2(d_out_tempo_num,"Data/d_out_tempo_num.csv")

#Pour l'année en facteur : 
write.csv2(d_out_tempo_fact,"Data/d_out_tempo_fact.csv")








