# B Lepage le 25 aout 2025
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
# pipe natif de R : 
# le pipe 
# x |> f() est équivalente à f(x) pour une fonction dont le 1er argument est argument x
# et l’écriture x |> f(y) est équivalent à f(x, y) pour une fonction à deux arguments, x et y
# c'est à dire que la valeur x à gauche du pipe (LHS, left-hand side)
# est appliquée au premier argument dans la fonction à droite du pipe (RHS right-hand side)

df_1 |> head() # équivalent de head(df_1)

df_1 |> tail()  # équivalent de tail(df_1)

df_1 |> str() # équivalent de str(df_1)

# cela permet d'avoir un code dont la décomposition est plus lisible
# par exemple 
round(mean(df_1$imc, na.rm = TRUE), digits = 1)

# est équivalent à  :
df_1$imc |> 
  mean(na.rm = TRUE) |>
  round(digits = 1)
  
# il est possible de placer l'élément à gauche du pipe à un autre argument
# dans la fonction à droite du pipe avec un "placeholder" underscore _
# x |> f(y, argument = _)     est équivalent à f(y, argument = x)
paste0("la moyenne de l'IMC est ",
       round(mean(df_1$imc, na.rm = TRUE), digits = 1),
       " kg/m2")

# est équivalent à  :
df_1$imc |> 
  mean(na.rm = TRUE) |>
  round(digits = 1) |>
  paste0("la moyenne de l'IMC est ", 
         a = _, # ne fonctionne pas si on indique "_" seul, il faut un format d'argument "a = _"
         " kg/m2")


### fonctions with() et within()
# On peut utiliser la fonction with() pour:
#  - faire des calculs statistiques
#  - faire des calculs temporaires
#  - quand il n'y a pas besoin de modifier les données

# On peut utiliser la fonction within() pour:
#  - créer de nouvelles colonnes
#  - mettre à jour des colonnes existantes
#  - les tâches de transformation de données


### fonction with()
# La fonction with() permet d'appliquer une fonction à une variable dans un data.frame
# sans utiliser l'opérateur $
mean(df_1$imc) 
# est équivalent à :
with(df_1, mean(imc)) 
# [1] 24.481

with(df_1,
     imc |> 
       mean(na.rm = TRUE) |>
       round(digits = 1) |>
       paste0("la moyenne de l'IMC est ", a = _, " kg/m2"))

# ---------------------------------------------------------------------------- #
# 3) description simple des paramètres des distributions ----
# ---------------------------------------------------------------------------- #
# la fonction within() peut modifier un data frame et retourner le data frame modifié
# on peut le faire variable par variable 
df_1.bis <- within(df_1, # au sein de la base df_1
                   sexL <- factor(sex, # pas besoin du $
                                  labels = meta_df_1$labs[meta_df_1$var == "sex"]))
# df_1.bis <- within(df_1, # au sein de la base df_1
#                    sexL <- factor(sex, # pas besoin du $
#                                   labels = meta_df_1 |> 
#                                     subset(var == "sex", labs))) 
head(df_1.bis)
#   subjid sex  imc trait pas     sexL
# 1      1   0 24.8     2 140  Féminin
# 2      2   0 24.1     3 109  Féminin
# 3      3   0 26.4     1 156  Féminin
# 4      4   0 23.3     2 124  Féminin
# 5      5   0 25.4     2 131  Féminin
# 6      6   1 25.0     3 148 Masculin

df_1.bis <- within(df_1.bis, # on part du nouveau data.frame
                   traitL <- factor(trait,
                                    labels = meta_df_1$labs[meta_df_1$var == "trait"]))
head(df_1.bis)
#   subjid sex  imc trait pas     sexL       traitL
# 1      1   0 24.8     2 140  Féminin Traitement A
# 2      2   0 24.1     3 109  Féminin Traitement B
# 3      3   0 26.4     1 156  Féminin      Placebo
# 4      4   0 23.3     2 124  Féminin Traitement A
# 5      5   0 25.4     2 131  Féminin Traitement A
# 6      6   1 25.0     3 148 Masculin Traitement B

# mais également créer plusieurs variables en même temps en mettant les expressions 
# entre accolades {}
df_1 <- within(df_1, {
                 sexL <- factor(sex, 
                                labels = meta_df_1$labs[meta_df_1$var == "sex"])
                 traitL <- factor(trait,
                                  labels = meta_df_1$labs[meta_df_1$var == "trait"])
               })
head(df_1) # à noter qu'il a terminé par la première commande sex !
#   subjid sex  imc trait pas       traitL     sexL
# 1      1   0 24.8     2 140 Traitement A  Féminin
# 2      2   0 24.1     3 109 Traitement B  Féminin
# 3      3   0 26.4     1 156      Placebo  Féminin
# 4      4   0 23.3     2 124 Traitement A  Féminin
# 5      5   0 25.4     2 131 Traitement A  Féminin
# 6      6   1 25.0     3 148 Traitement B Masculin

# +++ TOUJOURS VERIFIER QUE L'ON A CREE CORRECTEMENT LES NOUVELLES VARIABLES +++
df_1 |> 
  subset(select = c(sex, sexL)) |> 
  table()
#    sexL
# sex Féminin Masculin
# 0     153        0
# 1       0      147

df_1 |> 
  subset(select = c(trait, traitL)) |> 
  table()
#      traitL
# trait Placebo Traitement A Traitement B
# 1     120            0            0
# 2       0           91            0
# 3       0            0           89

df_1 |> summary()
#     subjid            sex            imc            trait            pas                 traitL          sexL    
# Min.   :  1.00   Min.   :0.00   Min.   :15.40   Min.   :1.000   Min.   : 92.0   Placebo     :120   Féminin :153  
# 1st Qu.: 75.75   1st Qu.:0.00   1st Qu.:22.30   1st Qu.:1.000   1st Qu.:125.0   Traitement A: 91   Masculin:147  
# Median :150.50   Median :0.00   Median :24.60   Median :2.000   Median :138.0   Traitement B: 89                 
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
df_1 |> nrow()

df_1$imc |> length()
# la fonction with permet d'appliquer une fonction sur une variable sans avoir besoin de l'opérateur $
with(df_1, length(imc))
# on également peut utiliser le pipe au sein de la fonction with()
with(df_1, 
     imc |> length())

# pour compter les effectifs non-manquants : 
df_1$imc[!is.na(df_1$imc)] |> length()
# avec with
with(df_1,
     length(imc[!is.na(imc)]))
# avec le pipe
df_1 |>
  subset(subset = (!is.na(imc))) |> # sélectionne la base de données sans IMC manquant
  nrow() # nombre de lignes dans cette pas de données

# moyennes
with(df_1, imc |> mean(na.rm = TRUE)) # 24.481
with(df_1, pas |> mean(na.rm = TRUE)) # 137.1467

# déviation standard (écart-type)
with(df_1, imc |> sd(na.rm = TRUE)) # 3.069072
with(df_1, pas |> sd(na.rm = TRUE)) # 16.80237

# quantiles
with(df_1, imc |> quantile(probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)) 
#   0%  25%  50%  75% 100% 
# 15.4 22.3 24.6 26.4 32.5
with(df_1, pas |> quantile(probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)) 
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
univ_quanti <- function(x, dig = 2, remove_miss = TRUE) { 
  n <- length(x[!is.na(x)])     # table(!is.na(x))["TRUE"]
  moy <- mean(x, na.rm = remove_miss)
  sd <- sd(x, na.rm = remove_miss)
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = remove_miss)
  
  # on stocke les résultat dans un vecteur de réels 
  param <- c(n, 
             round(moy, digits = dig), 
             round(sd, digits = dig), 
             q)
  # on peut ajouter un nom à chaque élément du vecteur
  names(param) <- c("N", "mean", "sd", "min", "Q1", "median", "Q3", "max")
  return(param)
}

with(df_1, 
     imc |> univ_quanti(dig = 1, remove_miss = TRUE))
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0   24.5    3.1   15.4   22.3   24.6   26.4   32.5
with(df_1, 
     pas |> univ_quanti(dig = 1, remove_miss = TRUE))
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0  137.1   16.8   92.0  125.0  138.0  149.0  177.0 



### Fonction apply, lapply, sapply
# on peut appliquer une fonction de manière répétée : 
#  - à des colonne ou des lignes de matrices avec la fonction apply()
#  - à des vecteurs ou des listes avec lapply() ou sapply() 
#    lapply() retourne une liste
#    sapply() retourne une matrice (ou un vecteur)
df_1 |> 
  subset(select = c(imc, pas)) |>
  apply(MARGIN = 2, # applique la fonction par colonne
        FUN = univ_quanti,
        dig = 1, remove_miss = TRUE)
#          imc   pas      le résultat est une matrice de réels
# N      300.0 300.0
# mean    24.5 137.1
# sd       3.1  16.8
# min     15.4  92.0
# Q1      22.3 125.0
# median  24.6 138.0
# Q3      26.4 149.0
# max     32.5 177.0

## lapply retourne une liste de même longueur que X
df_1 |> 
  subset(select = c(imc, pas)) |>
  lapply(FUN = univ_quanti,
         dig = 1, remove_miss = TRUE)
# $imc                                                    le résultat est une liste
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0   24.5    3.1   15.4   22.3   24.6   26.4   32.5 
# 
# $pas
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0  137.1   16.8   92.0  125.0  138.0  149.0  177.0 

## sapply fonctionne comme lapply, mais retourne les résultats en vecteur ou matrice
df_1 |> 
  subset(select = c(imc, pas)) |>
  sapply(FUN = univ_quanti,
         dig = 1, remove_miss = TRUE)



# ---------------------------------------------------------------------------- #
## 3.2) variables qualitatives ----
# ---------------------------------------------------------------------------- #
### on veut récupérer les paramètres suivants : 
###  - effectifs non-manquants
###  - pourcentages

### sex
with(df_1, table(sexL)) # un vecteur avec les effectifs
with(df_1, prop.table(table(sexL))) # un vecteur avec les pourcentages
# on va combiner ces deux vecteurs pour les afficher dans une matrice 
tab_sex <- with(df_1,
                cbind(table(sexL),
                      prop.table(table(sexL)) * 100 |> 
                        round(digits = 1)))
colnames(tab_sex) <- c("n", "pct")

### traitement
tab_trait <- with(df_1,
                cbind(table(traitL),
                      prop.table(table(traitL)) * 100 |>
                        round(digits = 1)))
colnames(tab_trait) <- c("n", "pct")
# il ne fait pas l'arrondi sur les pourcentages ??!!


# ---------------------------------------------------------------------------- #
# 4) représentation graphiques des variables ----
# ---------------------------------------------------------------------------- #
# +++ C'est TOUJOURS UNE BONNE IDEE DE REGARDER SES DONNEES GRAPHIQUEMENT +++
# R base comporte quelques fonction graphiques permettant de représenter les variables
# qualitatives ou quantitatives

### Variables quantitatives : 
# histogrammes 
with(df_1, 
     imc |> hist(xlab = "IMC (kg/m²)", main = "Histogramme de l'IMC")) 
with(df_1, 
     pas |> hist(xlab = "PAS (mmHg)", main = "Histogramme de la PAS")) 

# boxplots
with(df_1,
     imc |> boxplot(main = "Boxplot de l'IMC", ylab = "IMC (kg/m²)"))
with(df_1,
     pas |> boxplot(main = "Boxplot de la PAS", ylab = "PAS (mmHg)"))

### variables qualitatives
# bar plots
with(df_1,
     table(sexL) |> barplot()) # avec les effectifs
with(df_1,
     table(sexL) |> 
       prop.table() |> 
       barplot()) # avec les pourcentages

# utiliser la variable en facteur permet d'indiquer directement les labesl en clair
with(df_1,
     table(sexL) |> 
       prop.table() |> 
       barplot(ylab = "Frequency",
               main = "Diagramme en barres du traitement")) 

### Croiser une variable quantitative en fonction d'une variable qualitative
# avec des boxplots
with(df_1,
     boxplot(pas ~ sexL,
             ylab = "PAS (mmHg)",
             xlab = meta_df_1$label[meta_df_1$var == "sex" & meta_df_1$id_labs == 1],
             main = "Boxplot de la PAS"))

with(df_1,
     boxplot(pas ~ traitL,
             ylab = "PAS (mmHg)",
             xlab = meta_df_1$label[meta_df_1$var == "trait" & meta_df_1$id_labs == 1],
             main = "Boxplot de la PAS"))

### Croiser deux variables quantitatives
# avec un nuage de points
# avec la fonction with()
with(df_1,
     plot(pas ~ imc,
          xlab = "IMC (kg/m²)", ylab = "PAS (mmHg)",
          main = "Nuage de points de la PAS en fonction de l'IMC"))

# avec le pipe natif
df_1 |>
  plot(pas ~ imc, 
       data = _, # indiquer le "placeholder" 
       xlab = "IMC (kg/m²)", ylab = "PAS (mmHg)",
       main = "Nuage de points de la PAS en fonction de l'IMC")
    


# ---------------------------------------------------------------------------- #
# 5) comparaisons bivariées ----
# ---------------------------------------------------------------------------- #
## 5.1) variables quantitatives vs quali ----
# ---------------------------------------------------------------------------- #
### on peut utiliser la fonction aggregate() qui permet d'appliquer 
### des fonctions à des sous-groupes de variables (définies en facteurs)
with(df_1,
     aggregate(x = list(imc = imc, pas = pas),
               by = list(sexL),
               FUN = mean,
               simplify = TRUE))
#    Group.1      imc      pas
# 1  Féminin 24.09281 132.4902
# 2 Masculin 24.88503 141.9932

# on modifie la fonction de calcul de paramètres en ajoutant un argument
# qui va calculer ou non les quantiles selon un argument VRAI/FAUX :
univ_quanti <- function(x, dig = 2, remove_miss = TRUE, quantiles = TRUE) { 
  n <- length(x[!is.na(x)])     # table(!is.na(x))["TRUE"]
  moy <- mean(x, na.rm = remove_miss)
  sd <- sd(x, na.rm = remove_miss)
  q <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = remove_miss)
  
  # on stocke les résultat dans un vecteur de réels 
  if (quantiles == TRUE) {
    param <- c(n, 
               round(moy, digits = dig), 
               round(sd, digits = dig), 
               q)
  } else {
    param <- c(n, 
               round(moy, digits = dig), 
               round(sd, digits = dig))
  }
  
  # on peut ajouter un nom à chaque élément du vecteur
  if (quantiles == TRUE) {
    names(param) <- c("N", "mean", "sd", "min", "Q1", "median", "Q3", "max")
  } else {
    names(param) <- c("N", "mean", "sd")
  }
  
  # retourne les résultats
  return(param)
}
with(df_1,
     imc |> univ_quanti(dig = 1, remove_miss = TRUE, quantiles = TRUE))
#     N   mean     sd    min     Q1 median     Q3    max 
# 300.0   24.5    3.1   15.4   22.3   24.6   26.4   32.5
with(df_1,
     imc |> univ_quanti(dig = 1, remove_miss = TRUE, quantiles = FALSE))
#     N  mean    sd 
# 300.0  24.5   3.1

imc_by_sex <- with(df_1,
                   aggregate(x = imc, 
                             by = list(sexL), 
                             FUN = univ_quanti, # fonction à utiliser
                             dig = 1, remove_miss = TRUE, quantiles = FALSE)) 
imc_by_sex
#    Group.1   x.N x.mean  x.sd
# 1  Féminin 153.0   24.1   3.1
# 2 Masculin 147.0   24.9   3.0

pas_by_sex <- with(df_1,
                   aggregate(x = pas, 
                             by = list(sexL), 
                             FUN = univ_quanti, # fonction à utiliser
                             dig = 1, remove_miss = TRUE, quantiles = FALSE))
pas_by_sex
#    Group.1   x.N x.mean  x.sd
# 1  Féminin 153.0  132.5  16.8
# 2 Masculin 147.0  142.0  15.5


### fonction tapply()
imc_by_sex.bis <- with(df_1,
                       tapply(X = imc, 
                              INDEX = list(sexL), 
                              FUN = univ_quanti, # fonction à utiliser
                              dig = 1, remove_miss = TRUE, quantiles = FALSE)) 
imc_by_sex.bis
# $Féminin
# N  mean    sd 
# 153.0  24.1   3.1 
# 
# $Masculin
# N  mean    sd 
# 147.0  24.9   3.0


# ---------------------------------------------------------------------------- #
## 5.2) variables qualitatives vs quali ----
# ---------------------------------------------------------------------------- #
### description du sexe en fonction du traitement :

## effectifs : 
trait_by_sex_N <- table(df_1$sexL, df_1$traitL)
trait_by_sex_N
#          Placebo Traitement A Traitement B
# Féminin       57           46           50
# Masculin      63           45           39

## pourcentages 
trait_by_sex_pct <- prop.table(trait_by_sex_N,
                               margin = 2) # 1 = par ligne, 2 = par colonne, NULL = par cellule)
trait_by_sex_pct
#            Placebo Traitement A Traitement B
# Féminin  0.4750000    0.5054945    0.5617978
# Masculin 0.5250000    0.4945055    0.4382022

## si on veut afficher dans une même table les effectifs et pourcentage, 
## on peut utiliser paste0 et arrondire les pourcentages
tab_biv_quali <- paste0(trait_by_sex_N, "(",round(trait_by_sex_pct * 100, digits = 1), "%)")
tab_biv_quali
# c'est devenu un vecteur atomique de caractères
# on va lui redonner les dimensions et noms des matrices initiales
dim(tab_biv_quali) <- dim(trait_by_sex_N)
dimnames(tab_biv_quali) <- dimnames(trait_by_sex_N)
tab_biv_quali
#         Placebo     Traitement A Traitement B
# Féminin  "57(47.5%)" "46(50.5%)"  "50(56.2%)" 
# Masculin "63(52.5%)" "45(49.5%)"  "39(43.8%)"


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

# ---------------------------------------------------------------------------- #
## 5.3) Tests de Student et Wilcoxon ----
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
## 5.4) Tests d'ANOVA et de Kruskal-Wallis ----
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
## 5.5) Tests du chi 2 et test exact de Fisher ----
# ---------------------------------------------------------------------------- #
# appliquer un test du chi2 à la table traitement en fonction du sex
chi2 <- chisq.test(table(df_1$sexL, df_1$traitL), 
                   correct = FALSE) # attention, par défaut il applique la correction de Yates pour les petits effectifs
chi2
# Pearson's Chi-squared test
# 
# data:  table(df_1$sexL, df_1$traitL)
# X-squared = 1.5512, df = 2, p-value = 0.4604      # p-value = 0.46

# vérification des conditions d'application : effectifs attendus dans chaque case : 
chi2$expected
#          Placebo Traitement A Traitement B
# Féminin     61.2        46.41        45.39
# Masculin    58.8        44.59        43.61
# les effectifs sont bien tous > ou = à 5

## si on avait du appliquer un test exact de fisher : 
fisher <- fisher.test(table(df_1$sexL, df_1$traitL))
fisher
# Fisher's Exact Test for Count Data
# 
# data:  table(df_1$sexL, df_1$traitL)
# p-value = 0.4536
# alternative hypothesis: two.sided



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

