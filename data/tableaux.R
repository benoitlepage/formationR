### exemple pour créer des tableaux
rm(list=ls())

# Import des données
df_1 <- read.csv2("data/df_1.csv")
meta_df_1 <- read.csv2("data/meta_df_1.csv")

# on va créer quelques données manquantes dans la base pour plus de réalisme :
# pour chaque variable, 10% des valeurs sont remplacées au hasard par des manquants
set.seed((6543))
df_1miss <- df_1
for (i in 2:ncol(df_1miss)) {
  df_1miss[[i]] <- ifelse(rbinom(n = nrow(df_1miss), size = 1, prob = 0.10) == 1, NA, df_1miss[[i]])
}
summary(df_1miss)

# On souhaite présenter sous forme de tableau mise en forme pour être inclus dans un rapport d'analyse : 
# - 1 table descriptive univariée des 4 variables (sex, imc, traitement et PAS)
# - 1 table bivariée comparant les distribution de sex, imc et PAS en fonction des groupes de traitement

# ---------------------------------------------------------------------------- #
# 1) Création d'une base de données correspondant au tableau à présenter ----
# ---------------------------------------------------------------------------- #
# 1.1) variable quantitative ----
# ---------------------------------------------------------------------------- #

# on va faire une fonction qui crée une table où : 
# les noms des colonnes sont "var" et "stat", 
# la 1ère ligne donne le label de la variable,
# la 2ème ligne indique les effectifs observés et manquants
# la 3ème ligne indique la moyenne et l'écart type (arrondis à 1 chiffre après la virgule)
# Pour les variables quantitatives : n / n missing et mean (SD)
N <- length(which(!is.na(df_1miss$imc)))
N_miss <- length(which(is.na(df_1miss$imc)))
moy <- mean(df_1miss$imc, na.rm = TRUE)
std <- sd(df_1miss$imc, na.rm = TRUE)


tab_imc <- data.frame(var = c(meta_df_1$label[meta_df_1$var == "imc"],
                              "n / n missing", 
                              "moyenne (DS)"),
                      stat= c("", # la première ligne de la colonne stat est vide
                              paste0(N, " / ", N_miss),
                              paste0(round(moy, digits = 1), " (",
                                     round(std, digits = 1), ")")))
tab_imc

# on peut automatiser la création de cette table avec une nouvelle fonction
# qui prendra 3 arguments : 
tab_univ_quanti <- function(data, # le data frame
                            metadata, # la base de méta-données
                            variables) { # vecteur de noms de variables quanti
  # la table associée à chaque variable sera stockée dans une liste
  tab_list <- list() # créée une liste vide
  
  for (i in variables) { # boucle pour chaque variable du vecteur var
    N <- length(which(!is.na(data[[i]])))
    N_miss <- length(which(is.na(data[[i]])))
    moyenne <- mean(data[[i]], na.rm = TRUE)
    std <- sd(data[[i]], na.rm = TRUE)
    
    tab_list[[i]] <- data.frame(var = c(metadata$label[metadata$var == i],
                                  "n / n missing", 
                                  "moyenne (DS)"),
                          stat= c("", # la première ligne de la colonne stat est vide
                                  paste0(N, " / ", N_miss),
                                  paste0(round(moyenne, digits = 1), " (",
                                         round(std, digits = 1), ")")))
  }
  
  # on commence par créer une table commune vide
  tab_pooled <- data.frame(matrix("", ncol = 2, nrow = 0))
  # on empile chaque table stockée dans la liste les unes après les autres
  for(j in 1:length(tab_list)) {
    tab_pooled <- rbind(tab_pooled, tab_list[[j]])
  }
  
  return(tab_pooled)
}

tab_quanti <- tab_univ_quanti(data = df_1miss, # le data frame
                              metadata = meta_df_1, # la base de méta-données
                              variables = c("imc", "pas"))
tab_quanti

# ---------------------------------------------------------------------------- #
# 1.2) variable qualitative ----
# ---------------------------------------------------------------------------- #
# pour compter les cases avec des effectifs nuls, le format "factor" est utile
# on va donc d'abord créer des variables qualitatives au format factor
df_1miss$sexL <- factor(df_1miss$sex,
                        labels = meta_df_1$labs[meta_df_1$var == "sex"])
df_1miss$traitL <- factor(df_1miss$trait,
                          labels = meta_df_1$labs[meta_df_1$var == "trait"])

# pensez à vérifier que le recodage est correct
table(df_1miss$sexL, df_1miss$sex)
table(df_1miss$traitL, df_1miss$trait)

# mise à jour de la base de méta données
meta_df_1 <- rbind(meta_df_1, 
                   data.frame(var = "sexL", 
                              label = meta_df_1$label[meta_df_1$var == "sex"],
                              id_labs = names(table(as.numeric(df_1miss$sexL))),
                              code_labs = as.numeric(names(table(as.numeric(df_1miss$sexL)))),
                              labs = levels(df_1miss$sexL)))
meta_df_1 <- rbind(meta_df_1, 
                   data.frame(var = "traitL", 
                              label = meta_df_1$label[meta_df_1$var == "trait"],
                              id_labs = names(table(as.numeric(df_1miss$traitL))),
                              code_labs = as.numeric(names(table(as.numeric(df_1miss$traitL)))),
                              labs = levels(df_1miss$traitL)))



# on va faire une table indiquant sur chaque ligne l'effectif et le pourcentage
# en cas de manquants, 
# la dernière ligne contient les effectifs observés et manquants
# comme précédemment, la première colonne contiendra le nom de la variable
# et le nom des labels

# on peut commencer par combiner en deux colonnes les effectifs et les pourcentages
# les pourcentages seront calculés en excluant les données manquantes 
# (comme sous une hypothèse "missing completely at random" MCAR)
tab_sex_temp <- cbind(table(df_1miss$sexL, useNA = "no"), 
                      prop.table(table(df_1miss$sexL, useNA = "no"))) 
tab_sex_temp

# la fonction paste0 peut être utilisée pour concaténer les 2 colonnes en un seul 
# vecteur, avec une mise en forme des pourcentages entre parenthèse et 1 chiffre
# après la virgule
paste0(tab_sex_temp[,1], " (", round(tab_sex_temp[,2] * 100, digits = 1), "%)")

tab_sex <- data.frame(var = c(meta_df_1$label[meta_df_1$var == "sexL"][1],
                              levels(df_1miss$sexL)),
                      stat= c("", # la première ligne de la colonne stat est vide
                              paste0(tab_sex_temp[,1], 
                                     " (", 
                                     round(tab_sex_temp[,2] * 100, digits = 1), 
                                     "%)")))
tab_sex <- rbind(tab_sex, 
                 c("n / n missing", 
                   paste0(length(which(!is.na(df_1miss$sexL))), 
                          " / ", 
                          length(which(is.na(df_1miss$sexL))))))


# On créé une fonction pour automatiser la création de tables pour les variables
# univaritées qualitatives
tab_univ_quali <- function(data, # le data frame
                           metadata, # la base de méta-données
                           variables) { # vecteur de noms de variables quanti
  # la table associée à chaque variable sera stockée dans une liste
  tab_list <- list() # créée une liste vide
  
  for (i in variables) { # boucle pour chaque variable du vecteur variables
    N <- length(which(!is.na(data[[i]])))
    N_miss <- length(which(is.na(data[[i]])))
    
    tab_temp <- cbind(table(data[[i]], useNA = "no"), 
                          prop.table(table(data[[i]], useNA = "no"))) 
    
    
    tab_list[[i]] <- data.frame(var = c(paste0(metadata$label[metadata$var == i][1],
                                               ", n (%)"),
                                        levels(data[[i]])),
                                stat= c("", # la première ligne de la colonne stat est vide
                                        paste0(tab_temp[,1], 
                                               " (", 
                                               round(tab_temp[,2] * 100, digits = 1), 
                                               "%)")))
    if (N_miss >= 1) { # s'il y a des  manquants, on ajoute les effectifs observés 
      tab_list[[i]] <- rbind(tab_list[[i]], 
                             c("n / n missing", 
                               paste0(length(which(!is.na(data[[i]]))), 
                                      " / ", 
                                      length(which(is.na(data[[i]]))))))
    }
  }
  
  # on commence par créer une table commune vide
  tab_pooled <- data.frame(matrix("", ncol = 2, nrow = 0))
  # on empile chaque table stockée dans la liste les unes après les autres
  for(j in 1:length(tab_list)) {
    tab_pooled <- rbind(tab_pooled, tab_list[[j]])
  }
  
  return(tab_pooled)
}

tab_quali <- tab_univ_quali(data = df_1miss, # le data frame
                            metadata = meta_df_1, # la base de méta-données
                            variables = c("sexL", "traitL"))
tab_quali


# ---------------------------------------------------------------------------- #
# 2) Présentation de la table avec le package tinytable ----
# ---------------------------------------------------------------------------- #
## faire une table mise en forme pour copier-coller dans un rapport d'analyse

tab_desc <- rbind(tab_quanti, tab_quali)

# on va changer les noms de colonnes
# en indiquant les effectifs totaux dans la colonne des résultats statistiques
names(tab_desc) <- c("Variables", 
                     paste0("N = ", nrow(df_1miss))) 

library(tinytable)
tt(tab_desc)

# pour afficher les noms de variables en gras
# et ajouter une indentation vers la droite pour les statistiques et catégories
tt(tab_desc) |>
  style_tt(
    i = which(tab_desc[,2] == ""), # sélectionner les lignes sans statistiques
    j = 1, # sélectionne la 1ère colonne
    bold = TRUE) |>
  style_tt(
    i = which(tab_desc[,2] != ""), # sélectionner les lignes avec statistiques
    j = 1,
    indent = 1 # ajoute une indentation vers la droite d'une unité
  )
# voir les options de style ?style_tt


# ---------------------------------------------------------------------------- #
# 3) Préparation d'un tableau pour des analyses bivariées avec p-value ----
# ---------------------------------------------------------------------------- #
table(df_1miss$traitL, useNA = "ifany")
# Placebo Traitement A Traitement B         <NA> 
#     104           84           79           33
# pour cette analyse, on va se restreindre à la base de 267 patients
# ayant des données observées pour le traitement,
# et on excluera les 33 données manquantes pour le traitement

# Pour croiser l'IMC et la PAS en fonction du traitement, 
# on va créer une fonction qui récupére sous forme de vecteur :
# - les effectifs observés et manquants (n / n missing)
# - la moyenne et l'écart type (moyenne (DS))
mean_sd_fct <- function(x, dig = 1, remove_miss = TRUE) { # data = data, 
  # v <- data[[x]]
  n_n_miss <- paste0(length(which(!is.na(x))), 
                     " / ",
                     length(which(is.na(x))))
  mean_sd <- paste0(round(mean(x, na.rm = remove_miss), digits = dig), 
                    " (",
                    round(sd(x, na.rm = remove_miss), digits = dig),
                    ")")
  return(c(n_n_miss, mean_sd))
}


mean_sd_fct(x = df_1miss[!is.na(df_1miss$traitL), "imc"],  
            dig = 1, 
            remove_miss = TRUE)
# aggregate(x = df_1miss$imc,                                 moins utile
#           by = list(df_1miss$traitL),
#           FUN = mean_sd_fct, # fonction à utiliser sur X
#           dig = 1, remove_miss = TRUE) # arguments de la fonction     
tapply(X = df_1miss$imc, 
       INDEX = df_1miss$traitL,
       FUN = mean_sd_fct, # fonction à utiliser sur X
       dig = 1, remove_miss = TRUE) # arguments de la fonction     
# pour que tapply fonctionne, la longueur des vecteurs X et INDEX doit être la même
# on remarque que la fonction tapply a automatiquement exclu de l'analyse les données
# manquantes sur l'INDEX (la variable traitL)

# préparation d'une table croisant le traitement avec : 
# - l'IMC
# - la PAS
# - le sexe
# les effectifs totaux seront affichés sur la première ligne
df_results <- data.frame(var = c("N total", 
                                 meta_df_1$label[meta_df_1$var == "imc"],
                                 "n / n missing", 
                                 "moyenne (DS)",
                                 meta_df_1$label[meta_df_1$var == "pas"],
                                 "n / n missing", 
                                 "moyenne (DS)",
                                 paste0(meta_df_1$label[meta_df_1$var == "sexL"][1], ", n (%)"),
                                 levels(df_1miss$sexL),
                                 "n / n missing"))
df_results <- data.frame(df_results, 
                         matrix("", ncol = length(levels(df_1miss$traitL)) + 2,
                                nrow = nrow(df_results)))
names(df_results) <- c("Variable", levels(df_1miss$traitL), "p-value", "Total")

## on remplit le contenu du tableau: 
# à noter que pour ce tableau, on va exclure de l'analyse les données manquantes
# concernant le traitement

## Ligne N total 
n_obs_tot <- table(df_1miss$traitL, useNA = "no")
df_results[1,c("Placebo", 
               "Traitement A", 
               "Traitement B")] <- paste0("N = ", n_obs_tot)
df_results[1, "Total"] <- paste0("N = ", sum(n_obs_tot))

## Lignes de l'IMC en fonction du traitement
imc_by_trait <- tapply(X = df_1miss$imc, 
                       INDEX = df_1miss$traitL,
                       FUN = mean_sd_fct, # fonction à utiliser sur X
                       dig = 1, remove_miss = TRUE)
      
df_results[3:4, c("Placebo")] <- imc_by_trait$Placebo
df_results[3:4, c("Traitement A")] <- imc_by_trait$`Traitement A`
df_results[3:4, c("Traitement B")] <- imc_by_trait$`Traitement B`
# note : pour avoir des effectifs cohérents dans la colonne total, 
# les analyses doivent être réalisée dans le sous-ensemble de données 
# sans manquants concernant le traitement +++
df_results[3:4, c("Total")] <- mean_sd_fct(x = subset(df_1miss,
                                                      subset = !is.na(traitL))$imc,  
                                           dig = 1, 
                                           remove_miss = TRUE)
anova_imc_by_traitL <- anova(aov(imc ~ traitL, data = df_1miss))
anova_imc_by_traitL$`Pr(>F)` # [1] 0.5259026        NA
df_results[2,"p-value"] <- paste0(round(anova_imc_by_traitL$`Pr(>F)`[1], digits = 2),
                                  "<sup>a")

# lignes de la PAS en fonction du traitement
pas_by_trait <- tapply(X = df_1miss$pas, 
                       INDEX = df_1miss$traitL,
                       FUN = mean_sd_fct, # fonction à utiliser sur X
                       dig = 1, remove_miss = TRUE)
df_results[6:7, c("Placebo")] <- pas_by_trait$Placebo
df_results[6:7, c("Traitement A")] <- pas_by_trait$`Traitement A`
df_results[6:7, c("Traitement B")] <- pas_by_trait$`Traitement B`
df_results[6:7, c("Total")] <- mean_sd_fct(x = subset(df_1miss, 
                                                      subset = !is.na(traitL))$pas,  
                                           dig = 1, 
                                           remove_miss = TRUE)
anova_pas_by_traitL <- anova(aov(pas ~ traitL, data = df_1miss))
anova_pas_by_traitL$`Pr(>F)` # [1] 1.079829e-05           NA
df_results[5,"p-value"] <- "<0.0001<sup>a"

## on complète la table pour le sexe
## on commence par décrire les effectifs avec les manquants 
## pour calculer les effectifs par groupe de traitement
table(df_1miss$sexL, df_1miss$traitL, useNA = "ifany")
#          Placebo Traitement A Traitement B <NA>
# Féminin       40           40           37   20
# Masculin      58           40           30   12
# <NA>           6            4           12    1

# total des 3 premières colonnes, en comptant les manquants
tot_trait <- colSums(table(df_1miss$sexL, df_1miss$traitL, useNA = "ifany")[,1:3])
# nombre de manquants
n_miss_sex <- table(df_1miss$sexL, df_1miss$traitL, useNA = "ifany")[3,1:3]
# total des effectifs non-manquants
n_obs_sex <- colSums(table(df_1miss$sexL, df_1miss$traitL, useNA = "no"))

# effectifs et pourcentages calculés en excluant les manquants (hypothèse MCAR)
tab_sex_n <- table(df_1miss$sexL, df_1miss$traitL, useNA = "no")
tab_sex_pct <- prop.table(table(df_1miss$sexL, df_1miss$traitL, useNA = "no"),
                          margin = 2) # pour les pourcentages en colonnes
tab_sexL_by_traitL <- paste0(tab_sex_n, 
                             " (", 
                             round(tab_sex_pct * 100, digits = 1),
                             "%)")
dim(tab_sexL_by_traitL) <- dim(tab_sex_n)
tab_sexL_by_traitL

# on complète la table bivariée
df_results[9:10,2:4] <- tab_sexL_by_traitL
df_results[11, 2:4] <- paste0(n_obs_sex, " / ", n_miss_sex)
# colonne Total
df_results[9:10,"Total"] <- paste0(rowSums(tab_sex_n), 
                                  " (",
                                  round(prop.table(rowSums(tab_sex_n)) * 100, 
                                        digits = 1),
                                  "%)")
df_results[11,"Total"] <- paste0(sum(n_obs_sex), " / ", sum(n_miss_sex))
# colonne p-value
chi2_sex_trait <- chisq.test(table(df_1miss$sexL, df_1miss$traitL, useNA = "no"))
df_results[8, "p-value"] <- paste0(round(chi2_sex_trait$p.value, digits = 2), 
                                   "<sup>b")

## présentation de la table bivariée ----
tt(df_results, 
   cap = "Table 1 : Analyses bivariées", 
   notes = list(a = "Anova", b = "Chi-2")) |>
  style_tt( # affiche les noms de variables en gras
    i = which(df_results[,2] == ""),
    j = 1,
    bold = TRUE) |> 
  style_tt( # ajoute une indentation vers la droite d'une unité
    i = which(df_results[,2] != ""), 
    j = 1,
    indent = 1) |>
  group_tt( # ajoute un nom de variable pour les 3 colonnes de traitements
    j = list("Traitements" = 2:4))
  



### création d'une fonction qui décrit la distribution en analyse bivariée ----
# pour les variables quanti

# data = df_1miss
# metadata = meta_df_1
# by = "traitL"
# variables = c("imc", "pas")
# dig = 1
tab_biv_quanti <- function(data, # le data frame
                           metadata, # la base de méta-données
                           by, # variable facteur en colonnes
                           variables, # vecteur de noms de variables quanti
                           dig = dig) { # nb de chiffres après la virgule
  
  # on intègre la fonction "mean_sd_fct" définie plus haut
  mean_sd_fct <- function(x, dig = dig, remove_miss = TRUE) { # data = data, 
    # v <- data[[x]]
    n_n_miss <- paste0(length(which(!is.na(x))), 
                       " / ",
                       length(which(is.na(x))))
    mean_sd <- paste0(round(mean(x, na.rm = remove_miss), digits = dig), 
                      " (",
                      round(sd(x, na.rm = remove_miss), digits = dig),
                      ")")
    return(c(n_n_miss, mean_sd))
  }
  
  # la variable by_var sera la variable à croiser
  by_var <- data[[by]]
  
  # la table associée à chaque variable sera stockée dans une liste
  tab_list <- list() # créée une liste vide
  
  for (i in variables) { # boucle pour chaque variable du vecteur var
    # on va indiquer dans une matrice : les résultats par groupe, le total, la p-value
    results <- matrix("", ncol = length(levels(by_var)) + 2,
                      nrow = 3)
    colnames(results) <- c(levels(by_var),"p-value","Total")
    res_list <- tapply(X = data[[i]], 
                       INDEX = by_var,
                       FUN = mean_sd_fct, # fonction à utiliser sur X
                       dig = 1, remove_miss = TRUE)
    for (j in 1:length(levels(by_var))) {
      results[2:3,j] <- res_list[[levels(by_var)[j]]]
    }
    results[2:3,"Total"] <- mean_sd_fct(x = data[[i]],
                                        dig = dig)
    
    # Anova pour récupérer la p-value
    anova_table <- aov(formula(paste0(i, "~", by)), 
                       data = data)
    pval <- anova(anova_table)$`Pr(>F)`[1]
    results[1 ,"p-value"] <- round(pval, digits = 2)            
        
    
    tab_list[[i]] <- data.frame(var = c(metadata$label[metadata$var == i],
                                        "n / n missing", 
                                        "moyenne (DS)"),
                                results)
  }
  # on commence par créer une table commune vide
  tab_pooled <- data.frame(matrix("", ncol = 2, nrow = 0))
  # on empile chaque table stockée dans la liste les unes après les autres
  for(j in 1:length(tab_list)) {
    tab_pooled <- rbind(tab_pooled, tab_list[[j]])
  }
  
  return(tab_pooled)
} 
  
table_quanti_by_trait <- tab_biv_quanti(data = df_1miss, # le data frame
                                        metadata = meta_df_1, # la base de méta-données
                                        by = "traitL", # variable facteur en colonnes
                                        variables = c("imc", "pas"), # vecteur de noms de variables quanti
                                        dig = 1) # nb de chiffres après la virgule
table_quanti_by_trait








# + exemple d'utilisation de tinytable avec une sortie de modèle de régression ++



# 1) library gt ---- 
## voir https://gt.rstudio.com/index.html
# puis, "Get started" pour avoir une vignette de présentation générale
#       ou "Articles > Case study: clinical table" pour des exemples de tables
#           utilisées pour présenter des données de santé.

## Installer la library gt, puis la charger
library(gt)

# gt permet de transformer des bases de données (format data.frame ou tibble) 
# en tableaux avec une mise en page propre.

# On va se servir de ce package pour créer des tableaux qui peuvent être inclus
# dans des rapports d'analyse ou des articles

## Importer la base df_1 et les méta-données
rm(list = ls())
df_1 <- read.csv2("data/df_1.csv")
meta_df_1 <- read.csv2("data/meta_df_1.csv")

## créer les facteurs traitement et sex
df_1 <- within(df_1, {
  sexL <- factor(sex, 
                 labels = meta_df_1$labs[meta_df_1$var == "sex"])
  traitL <- factor(trait,
                   labels = meta_df_1$labs[meta_df_1$var == "trait"])
  })

## On met à jour la base de méta-données pour ces deux nouvelles variables
# (on garde les mêmes label, id_labs, cod_labs et labs que pour les variables
#  originales)
meta_df_1 <- rbind(meta_df_1, 
                   data.frame(var = "sexL", 
                              label = meta_df_1$label[meta_df_1$var == "sex"],
                              id_labs = names(table(as.numeric(df_1$sexL))),
                              code_labs = as.numeric(names(table(as.numeric(df_1$sexL)))),
                              labs = levels(df_1$sexL)))
meta_df_1 <- rbind(meta_df_1, 
                   data.frame(var = "traitL", 
                              label = meta_df_1$label[meta_df_1$var == "trait"],
                              id_labs = names(table(as.numeric(df_1$traitL))),
                              code_labs = as.numeric(names(table(as.numeric(df_1$traitL)))),
                              labs = levels(df_1$traitL)))

## Exemple simple pour décrire la distribution du traitement (au format "factor")
df_1$traitL |> table()

df_1$traitL |> 
  table() |>
  prop.table() |>
  round(digits = 1)

# On veut créer un objet data.frame indiquant : 
#  - les noms des variables, 
#  - les noms des levels, 
#  - les effectifs 
#  - les % (1 chiffre après la virgule) pour la variable sex
tb_sex <- data.frame(var_name = meta_df_1$label[meta_df_1$var == "sexL"],
                     levels = df_1$sexL |> 
                       table() |>
                       names(),
                     n = df_1$sexL |> 
                       table() |>
                       as.integer(), # supprime les attributs "names()" du vecteur
                     p = df_1$sexL |> 
                       table() |>
                       prop.table() |>
                       as.double() * 100) # supprime les attributs "names()"
                    
# Ajouter au dessous la table décrivant le traitement
tb_trait <- data.frame(var_name = meta_df_1$label[meta_df_1$var == "traitL"],
                       levels = df_1$traitL |> 
                         table() |>
                         names(),
                       n = df_1$traitL |> 
                         table() |>
                         as.integer(), # supprime les attributs "names()"
                       p = df_1$traitL |> 
                         table() |>
                         prop.table() |>
                         as.double() * 100)  # supprime les attributs "names()"

tb1 <- rbind(tb_sex, tb_trait)

# on va présenter les pourcentages arrondis à 1 chiffre après la virgule
tb1$p <- with(tb1, round(p, digits = 1))
tb1

# mise en forme de la table avec la fonction gt()
# l'objet doit être au format data.frame ou tibble
t1 <- gt(tb1)
t1
class(t1)
# l'objet t1 obtenu est à la fois une liste et une table de format "gt_tbl"
# cette liste est constituée de plusieurs objets que vous pouvez récupérer 
# en commançant par saisir t1$... et voir les propositions de RStudio
t1$`_data` # données initiales
t1$`_heading`$title # pour l'instant, aucun titre de table n'est associé

## On peut ajouter un certain nombre d'arguments voir l'aide ?gt
# "rowname_col" défini la colonne levels comme "nom de lignes"
# "groupname_col" défini la colonne var_name comme "nom de variable"
t1 <- gt(tb1,
         rowname_col = "levels",
         groupname_col = "var_name") 
t1

# ajouter un nom de colonne au dessus des noms de lignes
t1 <- t1 |>
  tab_stubhead(label = "Variables")
t1

# changer le nom des colonnes
t1 <- t1 |>
  cols_label(n = "N", # change les noms de colonnes
             p = "%")
t1

## ajouter des titres et sous-titres
# à noter que l'on peut utiliser des format markdown ou html pour changer
# la police. 
# Par exemple, en markdown, 
#  - texte en italique = encadrer le texte par une astérisque ou un underscore 
#  - texte en gras = encadrer le texte par 2 astérisques

t1 <- t1 |>
  tab_header(
    title = "Indiquer votre titre ici",
    subtitle = md("Texte en **gras** ou en *italique*")
  )
t1
# Question : comment ajouter des caractères d'équation ou des lettres grecques ? ++++++++++


