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
summary(df_1miss)


# On souhaite présenter sous forme de tableau mise en forme pour être inclus dans un rapport d'analyse : 
# - 1 table descriptive univariée des 4 variables (sex, imc, traitement et PAS)
# - 1 table bivariée comparant les distribution de sex, imc et PAS en fonction des groupes de traitement

# ---------------------------------------------------------------------------- #
# 1) Package table1 ----
# ---------------------------------------------------------------------------- #
library(table1)
# pour réaliser une analyse descriptive univariée
table1(~ imc + pas + traitL + sexL, data = df_1miss)

# le package table1 permet d'ajouter un nom de variable plus explicites aux variables
# le label est ajouté en "attribute" de la variable
label(df_1miss$sexL) <- meta_df_1$label[meta_df_1$var == "sexL"][1]
label(df_1miss$traitL) <- meta_df_1$label[meta_df_1$var == "traitL"][1]
label(df_1miss$imc) <- meta_df_1$label[meta_df_1$var == "imc"]
label(df_1miss$pas) <- meta_df_1$label[meta_df_1$var == "pas"]

attributes(df_1miss$sexL)
attributes(df_1miss$imc)

table1(~ imc + pas + traitL + sexL, data = df_1miss)
# le label apparaît à présent 

# il est possible de choisir quels paramètres afficher : 
# pour plus d'information sur les paramètres des distributions statistiques disponibles
# avec table1 : 
?stats.default
# par défaut, les variables continues sont présentés avec cet argument
render.continuous.default
# function (x, ...) 
# {
#   with(stats.apply.rounding(stats.default(x, ...), ...), 
#        c("", 
#          `Mean (SD)` = sprintf("%s (%s)", MEAN, SD), 
#          `Median [Min, Max]` = sprintf("%s [%s, %s]", MEDIAN, MIN, MAX)))
# }

# les variables en classes sont présentés avec cet argument
render.categorical.default
# function (x, ..., na.is.category = TRUE) 
# {
#   c("", 
#     sapply(stats.apply.rounding(stats.default(x, ...), ...), 
#            function(y) with(y, 
#                             sprintf("%s (%s%%)", 
#                                     FREQ, 
#                                     if (na.is.category) PCT else PCTnoNA))))
# }

# on peut créer de nouvelles fonctions en s'inspirant de ces deux fonctions 
# utilisées par défaut pour afficher les paramètres que l'on souhaite ++
render_var_quanti <- function(x) {   # fonction permettant de customiser les paramètres à récupérer et le format d'affichage
  with(stats.default(x), 
       c("", 
         "Mean &plusmn SD" = sprintf("%0.1f &plusmn %0.1f", MEAN, SD)))
}

# Ci dessous on créé une nouvelle fonction de rendu des variables 
render_var_quali <- function (x, ..., na.is.category = FALSE) { # TRUE passe en FALSE
  # en cas de données manuantes, les pourcentages sont calculés en excluants les manquants
    c("",
      sapply(stats.apply.rounding(stats.default(x, ...), ...),
             function(y) with(y, sprintf("%s (%s%%)", FREQ,
                                         if (na.is.category) PCT else PCTnoNA)))) 
  }

table1(~ imc + pas + traitL + sexL, 
       data = df_1miss, 
       render.continuous = render_var_quanti, 
       render.categorical = render_var_quali)

table1(~ imc + pas + traitL + sexL, 
       data = df_1miss, 
       render.continuous = render_var_quanti, 
       render.categorical = render.categorical.default(df_1miss, na.is.category = FALSE))

# table bivariée
table1(~ imc + pas + sexL | traitL, 
       data = df_1miss)
# cela ne fonctionne pas car la variable de stratification (le traitement)
# ne doit pas contenir de données manquantes +++
table1(~ imc + pas + sexL | traitL, 
       data = subset(df_1miss, subset = !is.na(traitL))) 
# à noter que les attributs de labels ont été supprimés lors de la sélection 
# des non-manquants par la fonction subset. Si on veut voir les labels de 
# variables dans la table, il faut créer une nouvelle base de données 
# et réattribuer les labels ou bien utiliser les fonctions du tidyverse
# qui permet de sélectionner des sous-ensemble sans supprimer les attributs
table1(~ imc + pas + sexL | traitL, 
       data = dplyr::filter(df_1miss, !is.na(traitL))) 

# Comme pour l'analyse bivariée, on peut choisir de n'afficher que la moyenne 
# et l'écart type pour les variables quanti et calculer les pourcentages
# en excluant les manquants pour les variables qualitatives
table1(~ imc + pas + sexL | traitL, 
       data = dplyr::filter(df_1miss, !is.na(traitL)),
       render.continuous = render_var_quanti, 
       render.categorical = render_var_quali) 
# comme pour les tables bivariées, 

# Une des limites du package table1 est qu'il ne permet pas d'ajouter
# les résultats de test de comparaison dans la sortie de la table
# bivariée


# ---------------------------------------------------------------------------- #
# 2) Création d'une base de données correspondant au tableau à présenter ----
# ---------------------------------------------------------------------------- #
# 2.1) variable quantitative ----
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
# 2.2) variable qualitative ----
# ---------------------------------------------------------------------------- #
# # pour compter les cases avec des effectifs nuls, le format "factor" est utile
# # on va donc d'abord créer des variables qualitatives au format factor
# df_1miss$sexL <- factor(df_1miss$sex,
#                         labels = meta_df_1$labs[meta_df_1$var == "sex"])
# df_1miss$traitL <- factor(df_1miss$trait,
#                           labels = meta_df_1$labs[meta_df_1$var == "trait"])
# 
# # pensez à vérifier que le recodage est correct
# table(df_1miss$sexL, df_1miss$sex)
# table(df_1miss$traitL, df_1miss$trait)
# 
# # mise à jour de la base de méta données
# meta_df_1 <- rbind(meta_df_1, 
#                    data.frame(var = "sexL", 
#                               label = meta_df_1$label[meta_df_1$var == "sex"],
#                               id_labs = names(table(as.numeric(df_1miss$sexL))),
#                               code_labs = as.numeric(names(table(as.numeric(df_1miss$sexL)))),
#                               labs = levels(df_1miss$sexL)))
# meta_df_1 <- rbind(meta_df_1, 
#                    data.frame(var = "traitL", 
#                               label = meta_df_1$label[meta_df_1$var == "trait"],
#                               id_labs = names(table(as.numeric(df_1miss$traitL))),
#                               code_labs = as.numeric(names(table(as.numeric(df_1miss$traitL)))),
#                               labs = levels(df_1miss$traitL)))



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
# 3) Présentation de la table avec le package tinytable ----
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
# 4) Préparation d'un tableau pour des analyses bivariées avec p-value ----
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
# manquantes sur l'INDEX (la variable traitL) ??

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
# A COMPLETER MAIS PEUT ETRE PAS POUR LA FORMATION, TROP COMPLEXE +++
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
reg_results <- lm(pas ~ traitL + sexL + imc, data = df_1miss)
coef_table <- summary(reg_results)$coefficients
IC95 <- confint.lm(reg_results)

results <- data.frame(Coefficients = rownames(coef_table), 
                      Estimate = round(coef_table[,"Estimate"], digits = 1), 
                      lb = round(IC95[,1], digits = 3), 
                      ub = round(IC95[,2], digits = 3), 
                      p = round(coef_table[,"Pr(>|t|)"], digits = 4))
names(results)[3:4] <- c("2.5 %", "97.5 %")
tt(results)


# ---------------------------------------------------------------------------- #
# 5) library gt ---- 
# ---------------------------------------------------------------------------- #
## voir https://gt.rstudio.com/index.html
# puis, "Get started" pour avoir une vignette de présentation générale
#       ou "Articles > Case study: clinical table" pour des exemples de tables
#           utilisées pour présenter des données de santé.

## Installer la library gt, puis la charger
library(gt)

# on récupère la table descriptive créée pour le package tinytable
tab_desc
df_results

# le principe du package gt est d'afficher le corps de la table
# - pour la table tab_desc, le corps correspond à la colonne 2
#   sans lignes vides associées aux noms de variables
tab_desc[which(tab_desc[,2] != ""), 2] # c'est un vecteur dans notre cas
corps <- gt(data.frame(stat = tab_desc[which(tab_desc[,2] != ""), 2]))
corps

# on peut ensuite ajouter différents éléments à ce corps de table

# ajouter un titre et un sous-titre avec la fonction tab_header()
corps |>
  tab_header(title = "Titre", 
             subtitle = "Sous-titre")

# ajouter des sources avec la fonction tab_source_note()
corps |>
  tab_header(title = "Titre", 
             subtitle = "Sous-titre") |>
  tab_source_note(
    source_note = "sources que l'on souhaite citer"
  )

# ajouter une note de bas de tableau avec la fonction tab_footnote()
# à noter qu'elle se positionne automatiquement avant les sources
corps |>
  tab_header(title = "Titre", 
             subtitle = "Sous-titre") |>
  tab_source_note(
    source_note = "sources que l'on souhaite citer"
  ) |>
  tab_footnote(
    footnote = "Note de bas de tableau"
  )

# on peut ensuite ajouter le "stub", à droite du tableau 
# le stub décrit les paramètres du corps du tableau
# l'argument "rowname_col" peut être utilisé pour cela

# on va se servir du contenu de la table descriptive 
# en supprimant les lignes indiquant les noms de variables
corps_df <- tab_desc[which(tab_desc$`N = 300` != ""),]
corps_df

gt(corps_df, 
   rowname_col = "Variables")

# on peut donner un nom de colonne au "stub" avec la fonction tab_stubhead()
gt(corps_df, 
   rowname_col = "Variables") |>
  tab_stubhead("Variables")

# on peut finalement regrouper les lignes par variable en ajoutant un nom
# de variable avec la fonction tab_row_group
gt(corps_df, 
   rowname_col = "Variables") |>
  tab_stubhead("Variables") |>
  tab_row_group(
    label = meta_df_1$label[meta_df_1$var == "imc"], 
    rows = 1:2 # indiquer les rangs correspondant à cette variable
  ) |>
  tab_row_group(
    label = meta_df_1$label[meta_df_1$var == "pas"], 
    rows = 3:4 # indiquer les rangs correspondant à cette variable
  ) |>
  tab_row_group(
    label = meta_df_1$label[meta_df_1$var == "sexL"][1], 
    rows = 5:7 # indiquer les rangs correspondant à cette variable
  ) |>
  tab_row_group(
    label = meta_df_1$label[meta_df_1$var == "traitL"][1], 
    rows = 8:11 # indiquer les rangs correspondant à cette variable
  ) 

# Une autre façon de faire est d'ajouter une colonne avec les labels de variables
# dans le data frame utilisé pour le corps de la table
corps_df <- tab_desc[which(tab_desc$`N = 300` != ""),]
corps_df$label <- c(rep(meta_df_1$label[meta_df_1$var == "imc"], 2), 
                    rep(meta_df_1$label[meta_df_1$var == "pas"], 2), 
                    rep(meta_df_1$label[meta_df_1$var == "sexL"][1], 3),
                    rep(meta_df_1$label[meta_df_1$var == "traitL"][1], 4))

# la colonne label peut alors être déclarée dans l'argument groupname_col
gt(corps_df, 
   rowname_col = "Variables", 
   groupname_col = "label") 

gt(corps_df, 
   rowname_col = "Variables", 
   groupname_col = "label") |>
  tab_stubhead("Variables") |>
  tab_header(title = "Titre", 
             subtitle = "Sous-titre") |>
  tab_source_note(
    source_note = "sources que l'on souhaite citer"
  ) |>
  tab_footnote(
    footnote = "Note de bas de tableau"
  )
  
## table bivariée
corps_biv <- df_results
# on va décaler d'une ligne vers le bas les p-values
corps_biv$`p-value`[2:11] <- corps_biv$`p-value`[1:10]
# pour l'instant, on supprime les indicateurs <sup>a et <sup>b des p-values
# c'est à dire qu'on supprime les 6 derniers caractères
corps_biv$`p-value` <- substr(corps_biv$`p-value`, 1, nchar(corps_biv$`p-value`) - 6)

corps_biv <- corps_biv[which(!corps_biv$Placebo == ""), ]
corps_biv$label <- c("", 
                     rep(meta_df_1$label[meta_df_1$var == "imc"], 2), 
                     rep(meta_df_1$label[meta_df_1$var == "pas"], 2), 
                     rep(meta_df_1$label[meta_df_1$var == "sexL"][1], 3))

gt(corps_biv, 
   rowname_col = "Variable", 
   groupname_col = "label") 

# on peut ajouter un groupe de colonne avec la fonction tab_spanner()
gt(corps_biv, 
   rowname_col = "Variable", 
   groupname_col = "label") |>
  tab_spanner(
    label = "Traitement", 
    columns = c("Placebo","Traitement A","Traitement B")
  )
# les notes de bas de tableau peuvent être utilisées pour indiquer le type
# de test statistique utilisé associé aux différentes p-values
gt(corps_biv, 
   rowname_col = "Variable", 
   groupname_col = "label") |>
  tab_spanner(                          # colonne de groupe de traitements
    label = "Traitement", 
    columns = c("Placebo","Traitement A","Traitement B")
  ) |>
  tab_footnote(                         # Anova en note de bas de table
    footnote = "Anova",
    locations = cells_body(columns = "p-value", rows = c(2,4))
  ) |>
  tab_footnote(                         # chi2 en note de bas de table
    footnote = "Chi-2",
    locations = cells_body(columns = "p-value", rows = 6)
  ) |> 
  tab_header(title = "Titre",           # ajouter un titre et un sous-titre
             subtitle = md("Texte en **gras** ou en *italique*")) |>
  tab_source_note(
    source_note = md("$$\\mathbb{E}(Y) = \\beta_0 + \\theta_1 X$$")
  )

# on peut appliquer les format markdown avec du texte dans la fonction md()
# (exemple dans le sous-titre)
# ainsi que des notations mathématiques 
# (exemple dans la source)

# il est possible également de nommer les colonnes directement.
# dans l'exemple ci-dessous, les effectifs sont ajoutés dans les noms de colonne
# après un saut à la ligne (en format markdown, indiqué par \n)
gt_tab <- gt(corps_biv[-1,], 
             rowname_col = "Variable", 
             groupname_col = "label") |>
  tab_spanner(                          # colonne de groupe de traitements
    label = "Traitement", 
    columns = c("Placebo","Traitement A","Traitement B")
  ) 
gt_tab[[2]]$column_label <- c("Variable", 
                              md("Placebo  N = 104"),
                              md("Traitement A  N = 84"),
                              md("Traitement B  N = 79"),
                              "p-value",
                              "Total",
                              "label")
gt_tab
# non il n'y a pas de retour à la ligne 

# |>
#   cols_label(
#     .list = list(
#       "Variable",
#       md("Placebo \n N = 104"),
#       md("Traitement A \n N = 84"),
#       md("Traitement B \n N = 79"), 
#       "p-value",
#       "Total",
#       "label")
#   ) # j'y arrive pas !


