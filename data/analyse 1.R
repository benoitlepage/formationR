# B Lepage le 22 aout 2025
# R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
# Copyright (C) 2025 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64


### 1ers exemples d'analyse de base de données : 
rm(list = ls())
# ---------------------------------------------------------------------------- #
# 1) Importer les bases ----
# ---------------------------------------------------------------------------- #
df_1 <- read.csv2("data/df_1.csv")
meta_df_1 <- read.csv2("data/meta_df_1.csv")
meta_df_1
#      var               label id_labs code_labs         labs
# 1 subjid Identifiant patient       1        NA             
# 2    sex                Sexe       1         0      Féminin
# 3    sex                Sexe       2         1     Masculin
# 4    imc         IMC (kg/m²)       1        NA             
# 5  trait          Traitement       1         1      Placebo
# 6  trait          Traitement       2         2 Traitement A
# 7  trait          Traitement       3         3 Traitement B
# 8    pas          PAS (mmHg)       1        NA             

# ---------------------------------------------------------------------------- #
# 2) Jeter un oeil à la base de données ----
# ---------------------------------------------------------------------------- #
head(df_1)
#   subjid sex  imc trait pas
# 1      1   0 24.8     2 140
# 2      2   0 24.1     3 109
# 3      3   0 26.4     1 156
# 4      4   0 23.3     2 124
# 5      5   0 25.4     2 131
# 6      6   1 25.0     3 148

tail(df_1)
#     subjid sex  imc trait pas
# 295    295   1 24.1     1 148
# 296    296   0 18.7     3 121
# 297    297   0 23.3     3 111
# 298    298   1 27.5     3 134
# 299    299   1 24.7     2 158
# 300    300   1 22.8     1 147

View(df_1)

str(df_1)
# 'data.frame':	300 obs. of  6 variables:
# $ X     : int  1 2 3 4 5 6 7 8 9 10 ...
# $ subjid: int  1 2 3 4 5 6 7 8 9 10 ...
# $ sex   : int  0 0 0 0 0 1 0 0 0 0 ...
# $ imc   : num  24.8 24.1 26.4 23.3 25.4 25 25.2 21.5 21.8 25.9 ...
# $ trait : int  2 3 1 2 2 3 3 3 1 1 ...
# $ pas   : int  140 109 156 124 131 148 125 117 132 133 ...


# ---------------------------------------------------------------------------- #
# 3) description simple des paramètres des distributions ----
# ---------------------------------------------------------------------------- #
summary(df_1)
#     subjid            sex            imc            trait            pas       
# Min.   :  1.00   Min.   :0.00   Min.   :15.40   Min.   :1.000   Min.   : 92.0  
# 1st Qu.: 75.75   1st Qu.:0.00   1st Qu.:22.30   1st Qu.:1.000   1st Qu.:125.0  
# Median :150.50   Median :0.00   Median :24.60   Median :2.000   Median :138.0  
# Mean   :150.50   Mean   :0.49   Mean   :24.48   Mean   :1.897   Mean   :137.1  
# 3rd Qu.:225.25   3rd Qu.:1.00   3rd Qu.:26.40   3rd Qu.:3.000   3rd Qu.:149.0  
# Max.   :300.00   Max.   :1.00   Max.   :32.50   Max.   :3.000   Max.   :177.0  
# à noter qu'ici, toutes les variables sont considérées comme des variables quantitatives
#                 ce n'est pas adapté, surtout pour la variable traitement !

# cela pourrait être utile de créer des variables labelisées, notamme pour les variables en classes
df_1$sexL <- factor(df_1$sex,
                    labels = meta_df_1$labs[meta_df_1$var == "sex"])
df_1$traitL <- factor(df_1$trait,
                    labels = meta_df_1$labs[meta_df_1$var == "trait"])

# +++ TOUJOURS VERIFIER QUE L'ON A CREE CORRECTEMENT LES NOUVELLES VARIABLES +++
table(df_1$sex, df_1$sexL, deparse.level = 2)
#         df_1$sexL
# df_1$sex Féminin Masculin
#        0     153        0
#        1       0      147

table(df_1$trait, df_1$traitL, deparse.level = 2)
#           df_1$traitL
# df_1$trait Placebo Traitement A Traitement B
#          1     120            0            0
#          2       0           91            0
#          3       0            0           89

summary(df_1)
#      subjid            sex            imc            trait            pas              sexL              traitL   
# Min.   :  1.00   Min.   :0.00   Min.   :15.40   Min.   :1.000   Min.   : 92.0   Féminin :153   Placebo     :120  
# 1st Qu.: 75.75   1st Qu.:0.00   1st Qu.:22.30   1st Qu.:1.000   1st Qu.:125.0   Masculin:147   Traitement A: 91  
# Median :150.50   Median :0.00   Median :24.60   Median :2.000   Median :138.0                  Traitement B: 89  
# Mean   :150.50   Mean   :0.49   Mean   :24.48   Mean   :1.897   Mean   :137.1                                    
# 3rd Qu.:225.25   3rd Qu.:1.00   3rd Qu.:26.40   3rd Qu.:3.000   3rd Qu.:149.0                                    
# Max.   :300.00   Max.   :1.00   Max.   :32.50   Max.   :3.000   Max.   :177.0

## Conseil : il vaut mieux créer de nouvelle variable qu'écraser les variables déjà exisante
##           (éviter de remplacer les valeurs de la variable sex initiale par le facteur par exemple)

# ---------------------------------------------------------------------------- #
## 3.1) variables quantitatives ----
# ---------------------------------------------------------------------------- #
### on veut récupérer les paramètres suivants : 
###  - effectifs non-manquants
###  - moyenne
###  - écart type
###  - minimum, 1er quartiles, médiane, 3ème quartile, maximum

# comme il n'y a pas de manquant dans cette base, on peut directement utiliser
# la fonction length() ou nrow() pour connaître les effectifs 
nrow(df_1)
length(df_1$imc)
table(!is.na(df_1$imc))["TRUE"] # tabuler l'évaluation logique d'IMC non-manquant
                                # et ne garder que la valeur associée à la réponse TRUE
                                # qui correspond au nombre de non-manquants

# moyennes
mean(df_1$imc, na.rm = TRUE) # 24.481
mean(df_1$pas, na.rm = TRUE) # 137.1467

# déviation standard (écart-type)
sd(df_1$imc, na.rm = TRUE) # 3.069072
sd(df_1$pas, na.rm = TRUE) # 16.80237

# quantiles
min(df_1$imc, na.rm = TRUE) # 15.4
min(df_1$pas, na.rm = TRUE) # 92

max(df_1$imc, na.rm = TRUE) # 32.5
max(df_1$pas, na.rm = TRUE) # 177

quantile(df_1$imc, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
#   0%  25%  50%  75% 100% 
# 15.4 22.3 24.6 26.4 32.5
quantile(df_1$pas, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
# 0%  25%  50%  75% 100% 
# 92  125  138  149  177

### Définir une fonction dans R
# pour récupérer l'ensemble de ces valeurs en une seule fois, 
# on peut créer une fonction qui va tous les calculer et retourner les résultats 
# sous forme de vecteur
# 3 arguments : 
#   - x la variable, 
#   - le nombre de chiffres après la virgule pour la présentation arrondie (on indique 2 par défaut)
#   - un paramètre pour définir si les manquants sont supprimés ou non pour 
#     le calcul des moyennes et autres paramètres (on indique TRUE par défaut)
univ_quanti <- function(x, dig = 2, na.remove = TRUE) { 
  n <- table(!is.na(x))["TRUE"]
  moy <- mean(x, na.rm = na.remove)
  sd <- sd(x, na.rm = na.remove)
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = na.remove)
  
  # on stocke les résultat dans un vecteur de réels 
  param <- c(n, 
             round(moy, digits = dig), 
             round(sd, digits = dig), 
             q)
  # on peut ajouter un nom à chaque élément du vecteur
  names(param) <- c("N", "mean", "sd", "min", "Q1", "median", "Q3", "max")
  return(param)
}

univ_quanti(df_1$imc, dig = 1)
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0   24.5    3.1   15.4   22.3   24.6   26.4   32.5
univ_quanti(df_1$pas, dig = 1)
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0  137.1   16.8   92.0  125.0  138.0  149.0  177.0 


### Fonction apply, lapply, sapply
# on peut appliquer une fonction de manière répétée : 
#  - à des colonne ou des lignes de matrices avec la fonction apply()
#  - à des vecteurs ou des listes avec lapply() ou sapply() 
#    lapply() retourne une liste
#    sapply() retourne une matrice (ou un vecteur)
apply(df_1[,c("imc", "pas")], 
      MARGIN = 2, # 2 pour appliquer la fonction par colonne, 1 par ligne 
      FUN = univ_quanti, # fonction à utiliser
      dig = 1) # on peut ajouter les arguments de la fonction à la suite
#          imc   pas      le résultat est une matrice de réels
# N      300.0 300.0
# mean    24.5 137.1
# sd       3.1  16.8
# min     15.4  92.0
# Q1      22.3 125.0
# median  24.6 138.0
# Q3      26.4 149.0
# max     32.5 177.0

lapply(df_1[,c("imc", "pas")], 
      FUN = univ_quanti,  # fonction à utiliser
      dig = 1) # on peut ajouter les arguments de la fonction à la suite
# $imc                                                    le résultat est une liste
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0   24.5    3.1   15.4   22.3   24.6   26.4   32.5 
# 
# $pas
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0  137.1   16.8   92.0  125.0  138.0  149.0  177.0 

sapply(df_1[,c("imc", "pas")],          # le résultat est une matrice
       FUN = univ_quanti,  # fonction à utiliser
       dig = 1) # on peut ajouter les arguments de la fonction à la suite

## Si on veut remplacer les noms de colonnes par les noms de variable
## en clair, on peut utiliser les méta-données
res_quanti <- sapply(df_1[,c("imc", "pas")],    
                     FUN = univ_quanti, 
                     dig = 1) 
colnames(res_quanti) <- c(meta_df_1$label[meta_df_1$var == "imc"],
                          meta_df_1$label[meta_df_1$var == "pas"])
res_quanti
#        IMC (kg/m²) PAS (mmHg)
# N            300.0      300.0
# mean          24.5      137.1
# sd             3.1       16.8
# min           15.4       92.0
# Q1            22.3      125.0
# median        24.6      138.0
# Q3            26.4      149.0
# max           32.5      177.0

library(tinytable)
res_quanti_df <- data.frame(paramètre = rownames(res_quanti),
                            res_quanti)
names(res_quanti_df)[c(2,3)] <- c(meta_df_1$label[meta_df_1$var == "imc"],
                                  meta_df_1$label[meta_df_1$var == "pas"])
tt(res_quanti_df)

library(flextable)
qflextable(res_quanti_df)

library(gt)
gt(res_quanti_df)

library(knitr)
# la fonction kable peut s'appliquer directement sur l'objet matrice initial
kable(res_quanti) 
kable(res_quanti, format = "simple")


# ---------------------------------------------------------------------------- #
## 3.2) variables qualitatives ----
# ---------------------------------------------------------------------------- #
### on veut récupérer les paramètres suivants : 
###  - effectifs non-manquants
###  - pourcentages

### sex
table(df_1$sexL) # un vecteur avec les effectifs
prop.table(table(df_1$sexL)) # un vecteur avec les pourcentages
# on va combiner ces deux vecteurs pour les afficher dans une matrice 
tab_sex <- cbind(table(df_1$sexL), 
                 round(prop.table(table(df_1$sexL)) * 100, digits = 1))
colnames(tab_sex) <- c("n", "pct")

### traitement
tab_trait <- cbind(table(df_1$traitL), 
                   round(prop.table(table(df_1$traitL)) * 100, digits = 1))
colnames(tab_trait) <- c("n", "pct")

# table synthétique
# on va créer un data frame qui regroupe ces deux matrices pour l'afficher au propre
tab_synt <- data.frame(variables = meta_df_1$label[meta_df_1$var == "sex" & meta_df_1$id_labs == 1],
                       n = "",
                       pct = "")
# on ajoute la table tab_sex
tab_synt <- rbind(tab_synt, 
                  cbind(variables = rownames(tab_sex), tab_sex))

# on ajoute le nom de la variable traitement
tab_synt <- rbind(tab_synt, 
                  c(meta_df_1$label[meta_df_1$var == "trait" & meta_df_1$id_labs == 1], "", ""))

# on ajoute la table tab_sex
tab_synt <- rbind(tab_synt, 
                  cbind(variables = rownames(tab_trait), tab_trait))
tab_synt

tt(data.frame(tab_synt))
qflextable(data.frame(tab_synt))

# pour info, il faut mieux mettre les labels au sein d'une colonne de matrice ou 
# data.frame plutôt qu'en attribut rownames()
# car les rownames() n'acceptent pas les valeurs en doublons, et ils ne s'affichent
# pas toujours facilement avec les fonction d'affichage de table au propre.


# ---------------------------------------------------------------------------- #
# 4) représentation graphiques des variables ----
# ---------------------------------------------------------------------------- #
# +++ C'est TOUJOURS UNE BONNE IDEE DE REGARDER SES DONNEES GRAPHIQUEMENT +++
# R base comporte quelques fonction graphiques permettant de représenter les variables
# qualitatives ou quantitatives

### Variables quantitatives : 
# histogrammes 
hist(df_1$imc, xlab = "IMC (kg/m²)", main = "Histogramme de l'IMC")
hist(df_1$pas, xlab = "PAS (mmHg)", main = "Histogramme de la PAS")

# boxplots
boxplot(df_1$imc, main = "Boxplot de l'IMC", ylab = "IMC (kg/m²)")
boxplot(df_1$pas, main = "Boxplot de la PAS", ylab = "PAS (mmHg)")

### variables qualitatives
# bar plots
barplot(table(df_1$sex)) # avec les effectifs
barplot(prop.table(table(df_1$sex))) # avec les pourcentages

# utiliser la variable en facteur permet d'indiquer directement les labesl en clair
barplot(prop.table(table(df_1$traitL)), 
        ylab = "Frequency",
        main = "Répartition du traitement")

### Croiser une variable quantitative en fonction d'une variable qualitative
# avec des boxplots
boxplot(df_1$pas ~ df_1$sexL, 
        ylab = "PAS (mmHg)",
        xlab = meta_df_1$label[meta_df_1$var == "sex" & meta_df_1$id_labs == 1],
        main = "Boxplot de la PAS")

boxplot(df_1$pas ~ df_1$traitL, 
        ylab = "PAS (mmHg)",
        xlab = meta_df_1$label[meta_df_1$var == "trait" & meta_df_1$id_labs == 1],
        main = "Boxplot de la PAS")

### Croiser deux variables quantitatives
# avec un nuage de points
plot(df_1$pas ~ df_1$imc, 
     xlab = "IMC (kg/m²)", ylab = "PAS (mmHg)",
     main = "Nuage de points de la PAS en fonction de l'IMC")

# Les paramètres R base offrent beaucoup de possibilité pour paramètrer 
# les graphiques
?par
# Par exemple, on peut représenter les hommes et les femmes par deux symboles 
# différents et les traitements par deux couleurs différentes
plot.new()
par(mar = c(5,4,4,2) + 0.1) # paramètre des marges par défaut
plot.window(xlim = c(15, 35), # range(df_1$imc)
            ylim = c(80, 180)) # range(df_1$pas)
grid() # ajoute une grille
# femmes, placebo
points(data = subset(df_1, subset = c(sex == "Féminin" & trait == "Placebo")), 
       pas ~ imc, 
       col = "indianred3", # placebo en rouge
       pch = 4) # femme avec une croix
# femmes, traitement A
points(data = subset(df_1, subset = c(sex == 0 & trait == 2)), 
       pas ~ imc, 
       col = "dodgerblue3", # traitement A en bleu
       pch = 4) # femme avec une croix
# femmes, traitement B
points(data = subset(df_1, subset = c(sex == 0 & trait == 3)), 
       pas ~ imc, 
       col = "palegreen3", # traitement B en vert
       pch = 4) # femme avec une croix
# hommes, placebo
points(data = subset(df_1, subset = c(sex == 1 & trait == 1)), 
       pas ~ imc, 
       col = "indianred3", # placebo en rouge
       pch = 1) # hommes avec un rond
# hommes, traitement A
points(data = subset(df_1, subset = c(sex == 1 & trait == 2)), 
       pas ~ imc, 
       col = "dodgerblue3", # traitement A en bleu
       pch = 1) # hommes avec un rond
# hommes, traitement B
points(data = subset(df_1, subset = c(sex == 1 & trait == 3)), 
       pas ~ imc, 
       col = "palegreen3", # traitement B en vert
       pch = 1) # hommes avec un rond
axis(1, # axe du bas
     lwd = 1, # largeur de la ligne
     font.axis=1) # taille de la police de caractère
axis(2, # axe à gauche
     lwd = 1, # largeur de la ligne
     font.axis=1) # taille de la police de caractère 
title(xlab = "IMC (kg/m²)")
title(ylab = "PAS (mmHg)")
title(main = "Nuage de points de la PAS en fonction de l'IMC")
legend("bottomright",
       c("Femme, P", "Femme, A", "Femme, B", 
         "Homme, P", "Homme, A", "Homme, B"), 
       pch = c(4,4,4,1,1,1),
       col = c("indianred3", "dodgerblue3", "palegreen3",
               "indianred3", "dodgerblue3", "palegreen3"),
       ncol = 1, 
       cex = 0.7)
### les figures de R base permettent de contrôler beaucoup de paramètre, 
### mais l'aide est difficile à utiliser ;
### vous pouvez trouver des informations détailler sur l'utilisation des 
### figures R base dans certains tutoriels, par exemple celui de Karolis Koncevičius: 
### https://github.com/karoliskoncevicius/r_notes/blob/main/baseplotting.md

# la fonction exemple permet d'avoir de nombre exemples de paramètres graphiques
example(line) 
example(axis)
example(legend)


# ---------------------------------------------------------------------------- #
# 5) comparaisons bivariées ----
# ---------------------------------------------------------------------------- #
biv_quanti <- function(x, y, dig = 1, na.rm = TRUE) {
  param = list()
  for(i in 1:length(unique(y))) {
    m_i = round(mean(x[y == unique(y)[i]], na.rm = na.rm), digits = dig)
    sd_i = round(sd(x[y == unique(y)[i]], na.rm = na.rm), digits = dig)
    param_i = paste0(as.character(m_i), " ± ",
                      as.character(sd_i))
    param[[i]] = param_i
  }
  return(param)
}

## Description (mean, sd) de l'imc et de la pas en fonction du sex
# on va stocker les résultats dans une base de données pour les mettre en forme 
# on veut une base selon le format suivant :
#   variables  |        femme       |    homme                #
# ----------------------------------------------------------- #
#              |        N = XXX     |    N = XXX              #
#     imc      |    moy ± sd        |    moy ± sd             #
#     pas      |    moy ± sd        |    moy ± sd             #

list_imc_sex <- biv_quanti(df_1$imc, df_1$sex, dig = 1, na.rm = TRUE)
list_pas_sex <- biv_quanti(df_1$pas, df_1$sex, dig = 1, na.rm = TRUE)

# on commence par la première ligne
tb_biv_quanti1 <- data.frame(variables = "",
                            femmes = paste0("N = ", table(df_1$sex)["0"]),
                            hommes = paste0("N = ", table(df_1$sex)["1"]))

# on ajoute les résultats de la description de l'imc en fonction du sexe
tb_biv_quanti1 <- rbind(tb_biv_quanti1, 
                       data.frame(variables = meta_df_1$label[meta_df_1$var == "imc"],
                                  femmes = list_imc_sex[[1]],
                                  hommes = list_imc_sex[[2]]))

# on ajoute les résultats de la description de la PAS en fonction du sexe
tb_biv_quanti1 <- rbind(tb_biv_quanti1, 
                       data.frame(variables = meta_df_1$label[meta_df_1$var == "pas"],
                                  femmes = list_pas_sex[[1]],
                                  hommes = list_pas_sex[[2]]))

tt(tb_biv_quanti1)
qflextable(tb_biv_quanti1)
names(tb_biv_quanti1)[2] <- "Femmes \n mean ± sd" # \n permet d'aller à la ligne
names(tb_biv_quanti1)[3] <- "Hommes \n mean ± sd"
qflextable(tb_biv_quanti1)


## Description (mean, sd) de l'imc et de la pas en fonction du sexe
list_imc_trait <- biv_quanti(df_1$imc, df_1$trait, dig = 1, na.rm = TRUE)
list_pas_trait <- biv_quanti(df_1$pas, df_1$trait, dig = 1, na.rm = TRUE)

## dans cet exemple, on ajoutera les effectifs dans la table finale 
## en titre de colonnes
# on commence par les valeurs de l'imc en fonction du traitement
tb_biv_quanti2 <- data.frame(variables = meta_df_1$label[meta_df_1$var == "imc"],
                             placebo = list_imc_trait[[1]],
                             ttt_A = list_imc_trait[[2]],
                             ttt_B = list_imc_trait[[3]])

# on ajoute les valeurs de la pas en fonction du traitement
tb_biv_quanti2 <- rbind(tb_biv_quanti2, 
                        data.frame(variables = meta_df_1$label[meta_df_1$var == "pas"],
                                   placebo = list_pas_trait[[1]],
                                   ttt_A = list_pas_trait[[2]],
                                   ttt_B = list_pas_trait[[3]]))

# On va donner des titres de colonnes plus précis
names(tb_biv_quanti2)[2] <- paste0("Placebo \n N = ",
                                   table(df_1$trait)["1"], "\n ",
                                   "mean ± sd")
names(tb_biv_quanti2)[3] <- paste0("Traitement A \n N = ",
                                   table(df_1$trait)["2"], "\n ",
                                   "mean ± sd")                                  
names(tb_biv_quanti2)[4] <- paste0("Traitement B \n N = ",
                                   table(df_1$trait)["3"], "\n ",
                                   "mean ± sd")                               

qflextable(tb_biv_quanti2)


# ---------------------------------------------------------------------------- #
## 5.1) Tests de Student et Wilcoxon ----
# ---------------------------------------------------------------------------- #
# Comparaison de l'IMC et de la PAS en fonction du sexe (2 moyennes)
### Test de Student
ttest.imc.sex <- t.test(data = df_1, imc ~ sex, 
                        var.equal = TRUE) # par défaut, c'est faux, il applique 
                                          # un test de Welch (pour variances inégales)
ttest.imc.sex
# Two Sample t-test
# 
# data:  imc by sex
# t = -2.2502, df = 298, p-value = 0.02517
# alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
# 95 percent confidence interval:
#   -1.48508718 -0.09935993
# sample estimates:
# mean in group 0 mean in group 1 
#        24.09281        24.88503 

ttest.imc.sex$p.value # [1] 0.02516741    p-value
ttest.imc.sex$conf.int # intervalle de confiance à 95% de la dif. des 2 moyennes
# [1] -1.48508718 -0.09935993
# attr(,"conf.level")
# [1] 0.95

ttest.pas.sex <- t.test(data = df_1, pas ~ sex, 
                        var.equal = TRUE)

### Test de Levene pour vérifier l'égalité des variances
# la fonction du test de Levene est dans le package "car"
?car::leveneTest
car::leveneTest(data = df_1, imc ~ sexL) # la variable en classe doit être un factor
car::leveneTest(data = df_1, pas ~ sexL) # la variable en classe doit être un factor
# OK, les deux p-values sont largement > 0.05


### on peut ajouter ces p-values aux tables descriptives réalisées précédemment : 
tb_biv_quanti1$`p-values` <- c("", 
                               as.character(round(ttest.imc.sex$p.value, digits = 2)), 
                               as.character(round(ttest.pas.sex$p.value, digits = 7)))
qflextable(tb_biv_quanti1)


### Si les variances n'avaient pas été égales, on aurait appliqué le 
### Test de Wilcoxon-Mann-Whitney
wilcox.test(data = df_1, imc ~ sex) # p-value = 0.02499
wilcox.test(data = df_1, pas ~ sex) # p-value = 1.529e-06

# ---------------------------------------------------------------------------- #
## 5.2) Tests d'ANOVA et de Kruskal-Wallis ----
# ---------------------------------------------------------------------------- #
# Comparaison de l'IMC et de la PAS en fonction du traitement (3 moyennes)
### Test d'Anova
# c'est un modèle linéaire, on va utiliser la fonction lm() pour linear model
# utiliser le facteur traitL permet de créer automatiquement des indicatrices
mod_imc_trait <- lm(imc ~ traitL, 
                    data = df_1)
summary(mod_imc_trait)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        24.32083    0.28032  86.761   <2e-16 ***
# traitLTraitement A  0.50774    0.42685   1.190    0.235    
# traitLTraitement B  0.02074    0.42957   0.048    0.962

# pour réaliser le test d'Anova, on le calcule avec la commande anova appliquée
# à notre résultat, pour avoir la table d'Anova avec les sources de variabilité
anova_imc_trait <- anova(mod_imc_trait) 
# Response: imc
#            Df Sum Sq Mean Sq F value Pr(>F)
# traitL      2   15.8  7.9009  0.8379 0.4336    p = 0.43
# Residuals 297 2800.5  9.4294

class(anova_imc_trait) # le résultat est un data.frame (bien adapté pour une table)
anova_imc_trait$`Pr(>F)`[1] # 0.4336368

anova_pas_trait <- anova(lm(pas ~ traitL, 
                            data = df_1)) 
anova_pas_trait$`Pr(>F)`[1] # p-value = 1.671077e-05 pour la PAS en fonction du ttt

### on peut ajouter ces p-values aux tables descriptives réalisées précédemment : 
tb_biv_quanti2$`p-values` <- c(as.character(round(anova_imc_trait$`Pr(>F)`[1], digits = 2)), 
                               as.character(round(anova_pas_trait$`Pr(>F)`[1], digits = 5)))
qflextable(tb_biv_quanti2)

### on vérifie l'égalité des variances : 
car::leveneTest(data = df_1, imc ~ traitL) # p = 0.3372   OK
car::leveneTest(data = df_1, pas ~ traitL) # p = 0.4703   OK

### si les variances avaient été inégales, on aurait appliqué un test de Kruskal-Wallis
kruskal.test(imc ~ traitL, data = df_1) # p-value = 0.327
kruskal.test(pas ~ traitL, data = df_1) # p-value = 6.33e-05
# les conclusions sont les mêmes qu'avec l'ANOVA



# ---------------------------------------------------------------------------- #
# 6) modèle multivarié ----
# ---------------------------------------------------------------------------- #
# on va faire un modèle de la PAS en fonction du traitement, ajusté sur le sexe et l'IMC
model <- lm(pas ~ traitL + sexL + imc, 
            data = df_1)
summary(model)
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        110.0173     7.1359  15.417  < 2e-16 ***
# traitLTraitement A -11.0405     2.1233  -5.200 3.74e-07 *** 
# traitLTraitement B  -2.9255     2.1361  -1.370 0.171866    
# sexLMasculin         8.5039     1.7785   4.782 2.75e-06 ***
# imc                  1.1102     0.2902   3.825 0.000159 ***

# on observait une PAS significativement moins élevée de -11.0 mmHg en moyenne 
# dans le groupe A versus placebo (p=3e-7)
# on observait une PAS moins élevée de -2.9 mmHg en moyenne dans le groupe B 
# versus placebo, de manière non-significative (p=0.17)

# par ailleurs, on observait que le sexe masculin et l'imc sont significativement
# associés à la PAS (mais ce n'était pas notre question, ce sont de simples
# facteurs prédictifs)

## on peut appliquer un diagnostic graphique des résidus avec la fonction plot()
## appliquée au model
plot(model)
# - le 1er graphique est le nuage de points des résidus en fonction des valeurs prédites
# - le 2ème graphique permet de vérifier la normalité des résidus standardisés
# - le 3ème graphique est le nuage de points de la racine carrée des résidus 
#   standardisés en fonction des valeurs prédites
# - le 4ème graphique le nuage de points des résidus standardisés en fonction des 
#   des distances de Cook (pour évaluer l'effet levier)

## on peut également faire à la main le nuage de points des résidus en fonction
## des valeurs prédites 
loes.model <- loess(model$residuals ~ model$fitted.values)
plot(model$residuals ~ model$fitted.values)
lines(predict(loes.model), col='red', lwd=2)

scatter.smooth(model$fitted.values, model$residuals, 
               lpars =list(col = "red", lwd = 0.5, lty = 1), 
               xlab = "Valeurs prédites", ylab = "Résidus")
abline(h = 0, lwd = 0.5, lty = 2) # ajoute une ligne de référence en pointillés


