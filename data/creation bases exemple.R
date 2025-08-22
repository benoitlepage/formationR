# B Lepage le 22 aout 2025
# R version 4.5.1 (2025-06-13 ucrt) -- "Great Square Root"
# Copyright (C) 2025 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64


rm(list=ls())
# ---------------------------------------------------------------------------- #
# création de df_1 ----
# ---------------------------------------------------------------------------- #
set.seed(54321)
N <- 300

### sex
sex <- rbinom(n = N, size = 1, prob = 0.5) # Femme = 0 ; Homme = 1

### IMC
imc <- rnorm(n = N, 
             mean = (24 * as.integer(sex == 0) + 
                       25 * as.integer(sex == 1)),
             sd = 3)
imc <- round(imc, digits = 1)
hist(imc)

### traitement
trait_cont <- runif(n = N)
trait <- ((1 * as.integer(trait_cont < 1/3)) + 
            (2 * as.integer(trait_cont >= 1/3 & trait_cont < 2/3)) +
            (3 * as.integer(trait_cont >= 2/3)))
boxplot(trait_cont ~ trait)
# P = 1 ; A = 2 ; B = 3

### PAS en mmHg
pas <- (110 + 
          (0 * as.integer(trait == 1) - 
             15 * as.integer(trait == 2) - 
             5 * as.integer(trait == 3)) + 
          1.2 * imc +
          10 * sex) + rnorm(n = N, mean = 0, sd = 15)
pas <- round(pas, digits = 0)
hist(pas)

model_pas <- lm(pas ~ as.factor(sex) + imc + as.factor(trait))
summary(model_pas)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -38.945 -10.096   0.013  10.857  41.157 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       110.0173     7.1359  15.417  < 2e-16 ***
#   as.factor(sex)1     8.5039     1.7785   4.782 2.75e-06 ***
#   imc                 1.1102     0.2902   3.825 0.000159 ***
#   as.factor(trait)2 -11.0405     2.1233  -5.200 3.74e-07 ***
#   as.factor(trait)3  -2.9255     2.1361  -1.370 0.171866    

# plot(model_pas)

df_1 <- data.frame(subjid = 1:300,
                   sex = sex, 
                   imc = imc, 
                   trait = trait,
                   pas = pas)
write.csv2(df_1, "data/df_1.csv", row.names = FALSE)

rm(imc, N, pas, sex, trait, trait_cont, model_pas)
## on crée une base de méta données qui sera utile pour la labellisation

## Pour chaque variable : 
## 1) on indique le nom de la variable et son label (répété autant de fois que nécessaire)
## 2) on commence par définir les modalités de réponses pour les variables catégorielles 
## qui nécessitent éventuellement plusieurs lignes par variable
## proposition : 
##  - id_labs : séquence de 1 jusqu'au numéro de labels nécessaire
##  - code_labs : code des labels
##  - labels : noms des labels

meta_df_1 <- data.frame(var = "subjid",
                        label = "Identifiant patient",
                        id_labs = 1,
                        code_labs = NA,
                        labs = "")

## On va ensuite fusionner cette base avec des bases temporaires qui seront fusionnées
## à la base initiale

## sex
meta_temp <- data.frame(var = rep("sex", 2),
                        label = rep("Sexe", 2),
                        id_labs = 1:2,
                        code_labs = c(0, 1),
                        labs = c("Féminin", "Masculin"))
meta_df_1 <- rbind(meta_df_1, meta_temp)

## imc
meta_temp <- data.frame(var = "imc",
                        label = "IMC (kg/m²)",
                        id_labs = 1,
                        code_labs = NA,
                        labs = "")
meta_df_1 <- rbind(meta_df_1, meta_temp)

## trait
meta_temp <- data.frame(var = rep("trait", 3), 
                        label = rep("Traitement", 3), 
                        id_labs = 1:3,
                        code_labs = 1:3,
                        labs = c("Placebo", "Traitement A", "Traitement B"))
meta_df_1 <- rbind(meta_df_1, meta_temp)

## pas
meta_temp <- data.frame(var = "pas",
                        label = "PAS (mmHg)",
                        id_labs = 1,
                        code_labs = NA,
                        labs = "")
meta_df_1 <- rbind(meta_df_1, meta_temp)
rm(meta_temp)

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

## Cette base de méta données pourra être utilisée pour labeliser les tableaux et graphiques
## Note les noms de colonnes sont arbitraires (je ne sais pas s'il existe des normes pour s'inspirer !)
write.csv2(meta_df_1, "data/meta_df_1.csv", row.names = FALSE)

# ---------------------------------------------------------------------------- #
# import from Stata ----
# ---------------------------------------------------------------------------- #
rm(list=ls())
library(haven)
df_1_stata <- read_dta("data/df_1.dta", 
                       encoding = "UTF-8")
View(df_1_stata)

attributes(df_1_stata$subjid)
attributes(df_1_stata$sex)
attributes(df_1_stata$imc)
attributes(df_1_stata$trait)
attributes(df_1_stata$pas)

# les méta données sont importées sous forme d'attributes +++

## On va ranger ces méta-données dans une base de méta-données qui 
## qui permet de conserver ces information de manière plus flexible
meta_df_1 <- data.frame(df = rep("", 0),
                        var = rep("", 0),
                        label = rep("", 0),
                        labels = rep(NA, 0),
                        labels_names = rep("", 0),
                        digits = rep(NA, 0))
for(i in 1:length(names(df_1_stata))) {
  
}


# ---------------------------------------------------------------------------- #
# import from SAS ----
# ---------------------------------------------------------------------------- #

# 
# ## install readxl package
# library(readxl)
# df_1 <- read_excel("data/base_1.xls")
# View(df_1)
# 
# 
# df_1$sex <- rep(NA, nrow(df_1))
