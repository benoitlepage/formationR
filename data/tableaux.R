### exemple pour créer des tableaux
rm(list=ls())

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
                   cbind(var = rep("sexL", 2), 
                         meta_df_1[meta_df_1$var == "sex", 2:5]))
meta_df_1 <- rbind(meta_df_1,
                   cbind(var = rep("traitL", 3), 
                         meta_df_1[meta_df_1$var == "trait", 2:5]))

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


