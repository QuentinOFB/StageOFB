######## Error in fitTMB(TMBStruc) : negative log-likelihood is Nan at starting parameter values
# -> Apparement problème avec le package Matrix 
  # Il faut le réinstaller (v 1.6-2)

#1. Réinstaller Matrix 

#2. Réinstaller TMB 

#3. Réinstaller GLMMTMB


library(data.table)
library(glmmTMB)
library(ggeffects)

data <- read.csv2("Data/Data_clean.csv", header = T)

#Ajout colonnes année hiver + mois hiver 
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
attach(data)
for (isp in 1:length(vecsp)) {
  sp <- vecsp[isp]  # Sélectionner l'espèce courante
  cat("\n\n (", isp, "/", length(vecsp), ") ", sp)  # Afficher l'état de la boucle
  
  # Définir la formule du modèle
  form <- as.formula("abondance ~ annee_hiver + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt)")
  
  #(1|secteur/site) => effet imbriqué secteur/site (prend en compte effet aléatoire du site)
  # 1| observateur => effet aléatoire observateur 
  # 1| mois_hiver_txt => effet aléatoire mois d'hiver 
  
  # Ajuster le modèle glmmTMB pour l'espèce courante, en utilisant la famille de distribution 'nbinom2' (binomiale négative)
  md <- try(glmmTMB(form, subset(data, espece == sp, annee_hiver>2004), family = "nbinom2"))
  
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
    
    # Initialiser ou ajouter les données prédictives à la sortie finale
    if (!out_init) {
      d_out <- ggmd
      out_init <- TRUE
    } else {
      d_out <- rbind(d_out, ggmd, fill = TRUE)
    } } }


#test avec une seule espèce : 
form <- as.formula("abondance ~ annee_hiver + (1|secteur/site) + (1|obs) + (1|mois_hiver_txt)")
md <- try(glmmTMB(form, subset(data, data$espece == "avocette_elegante", data$annee_hiver>2004), family = "nbinom2"))



