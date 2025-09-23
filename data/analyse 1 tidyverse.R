
library(tidyverse)

# 1) Importer les données ----
df_1 <- read_csv2("data/df_1.csv")
meta_df_1 <- read_csv2("data/meta_df_1.csv")
class(df_1)  # [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

# Voir les différences par rapport au data.frames https://tibble.tidyverse.org/articles/tibble.html
# La base de données est importé au format "tibble" (abbréviation "tbl"), grâce au package [`tibble`](https://tibble.tidyverse.org/)
# à la différence des data.frames: 
#  - tibble ne prend pas en compte les rownames()
df_test <- data.frame(a = 10:15, b = letters[1:6])
rownames(df_test) <- c("janvier", "février", "mars", "avril", "mai", "juin")
rownames(df_test)
rownames(as_tibble(df_test)) # [1] "1" "2" "3" "4" "5" "6"
#  - il est facile de créer des colonnes correspondant à des listes (un vecteur comportant des valeurs de types différents)
tibble(x = 1:3, 
       y = list(1:5, letters[1:10], 1:20))
#  - il ne change pas les noms de colonnes que l'on souhaite écrire dans un format non-conventionnelle
data.frame(`1 N (%)` = 1:3) # la commande data frame a changé le nom de variable
tibble(`1 N (%)` = 1:3) # le nom de variable non-conventionnel est conservé
#  - il recycle uniquement les vecteurs de longueur 1 pour éviter des erreurs
data.frame(id = 1:10,
           var1 = 20:24, # ces valeurs sont recyclées avec data.frame
           var2 = 30) # la valeur est recyclée avec data.frame
tibble(id = 1:10, 
       var1 = 20:24, # message d'erreur en cas valeurs de longueur > 1
       var2 = 30)
tibble(id = 1:10, 
       var2 = 30) # mais est capable de recycler des valeurs de longueur 1

# un sous-ensemble de tibble sélectionné par indexation retourne un autre tibble
df_1[c(100:103), c(2,3)]
# on peut sélectionner une variable avec [[]] ou l'opérateur dollar (comme pour les data.frames)
df_1[[4]]
df_1$trait


### Les fonctions de manipulation de données sont associées au package dplyr du tidyverse
# cf cheatsheet https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf


# 2) Inspecter les données ----

# contrairement au format data.frame, à l'affiche, le format tibble n'imprime à l'écran que les 10 premières lignes 
# et les colonnes qui peuvent être affichée dans un même écran. Sont également ajoutés le type de variable (double, caractère, facteur, ...)
# et met en valeurs les valeurs particulières par des couleurs (les NA par exemple)
df_1
# # A tibble: 300 × 5
#        subjid   sex   imc trait   pas
#         <dbl> <dbl> <dbl> <dbl> <dbl>
#      1      1     0  24.8     2   140
#      2      2     0  24.1     3   109
#      3      3     0  26.4     1   156
#      4      4     0  23.3     2   124
#      5      5     0  25.4     2   131
#      6      6     1  25       3   148
#      7      7     0  25.2     3   125
#      8      8     0  21.5     3   117
#      9      9     0  21.8     1   132
#     10     10     0  25.9     1   133
# # ℹ 290 more rows
# # ℹ Use `print(n = ...)` to see more row

# affiche une version transposée de la base (les colonnes sont imprimées en lignes)
# un peu à la manière de la fonction str() de R base
glimpse(df_1)

## La fonction `arrange` permet de trier les données selon une ou plusieurs variables
arrange(df_1, sex, imc) # trie la base df_1 sur le sexe, puis sur l'imc

## pour sélectionner des lignes, on peut utiliser la fonction slice et autres fonctions associées
## slice_head() et slice_tail() pour sélectionner les premières et dernière lignes
## voir l'aide ?dplyr::slice
## les arguments `n` permet d'indiquer le nombre de ligne, et `prop` le pourcentage de lignes que 
## l'on souhaite sélectionner
slice_head(df_1, n = 5) # voir les 5 premières lignes
df_1 %>% arrange(sex, imc) %>% slice_head(n = 6)  # voir les 6 premières lignes
# après avoir rangé les données selon le sexe et l'imc
## attention avec l'écriture par pipe : 
df_1 %>% slice_head(n = 6) %>% arrange(sex, imc) # ici, on a d'abord séléctionné
# les données (ce qui donne une base de type tibble de 6 lignes), et on a ensuite
# rangé cette petite base de 6 lignes selon le sexe et l'imc

df_1 %>% slice_tail(prop = 0.01) # voir 1% des dernières lignes (càd 3 lignes)

## slice_sample() sélection des rangs au hasard
df_1 %>% slice_sample(n = 6)

## slice() permet de sélection des lignes spécifiques (4ème, 10ème et 15ème ligne)
df_1 %>% slice(4, 10, 15) 
# est équivalent à l'écriture avec indexaction par position R base df_1[c(4,10,15), ]

## la fonction filter() permet de sélectionner des lignes en indiquant une condition
## voir l'aide ?dplyr::filter()
df_1 %>% filter(sex == 0 & imc < 21)
# si on écrit plusieurs conditions séparées par une virgule, 
# c'est équivalent à des conditions séparées par ET (condition1 & condition2)
df_1 %>% filter(sex == 0, imc < 21) 
# c'est équivalent à la sélection par condition entre crochets de R base 
df_1[df_1$sex == 0 & df_1$imc < 21, ]

## pour sélectionner des colonnes (= des variables) : fonction select()
# qui détaille un certains nombre de fonctions qui peuvent être utiles pour 
# faciliter la sélection de variable au sein de la fonction select()
df_1 %>% select(c(sex, trait)) # sélectionne les colonnes sexe et trait
df_1 %>% select(sex, trait) # équivalent à la syntaxe précédente (pas besoin de c())
# c'est équivalent à la sélection par indexation sur le nom R base
df_1[,c("sex", "trait")]

df_1 %>% select(subjid:imc) # sélectionne les colonnes allant de sexe à imc
df_1 %>% select(!sex) # ensemble complémentaire de la variable sexe
df_1 %>% select(-sex, -imc) # toutes les variables sauf sex et imc
df_1 %>% select(last_col()) # sélectionne la dernière colonne
df_1 %>% select(starts_with("pa")) # toutes les variables dont le nom commence par "pa"
df_1 %>% select(ends_with("as")) # toutes les variables dont le nom termine par "as"
df_1 %>% select(contains("bj")) # toutes les variables dont le nom contient par "bj"
# on peut également combiner plusieurs modes de sélection : 
df_1 %>% select(subjid, starts_with("im"), last_col())
# etc, voir l'aide
?select()

# select() permet également de renommer directement les variables sélectionnées
df_1 %>% select(Sexe = sex, Traitement = trait)

# on peut s'en servir pour changer l'ordre des variables
select(df_1, 
       imc, trait, subjid, # commence avec ces 3 variables dans cet ordre
       everything()) # everything() sélection toutes les autres variables

### pour sélectionner une variable et la retourner comme une colonne de base de données : 
df_1[3]
class(df_1[3]) # c'est une base de données (tibble ou data.frame)
# équivalent en dplyr
select(df_1, imc)
class(select(df_1, imc)) # une colonne de base de données (tibble ou data.frame)

### pour sélectionner une variable et la retourner comme un vecteur : 
df_1$imc # retourne le vecteur imc (équivalent à df_1[["imc"]])
df_1[[3]]
df_1[["imc"]]
class(df_1$imc) # c'est un vecteur numérique
typeof(df_1$imc) # un vecteur de réels (double) pour être plus précis
# équivalent en dplyr
pull(df_1, imc)
class(pull(df_1, imc)) # c'est un vecteur numérique
typeof(pull(df_1, imc)) # un vecteur de réels (double) pour être plus précis



# 3) Créer ou modifier une variable ----
## 3.1) Créer des variables ----
# la fonction mutate() permet de créer des nouvelles variable
# la fonction if_else() de dplyr fonctionne selon le même principe que la 
# fonction ifelse() de Rbase (notez la différence d'écriture) à quelques différences prêt :
#  - if_else() de dplyr s'assure que le résultat est du même "type"
ifelse(1:10 < 5, "<5", 0) # renvoi un vecteur caractère (le 0 est transformé en "0" par coercition)
if_else(1:10 < 5, "<5", 0) # renvoi une erreur
#  - if_else() permet de préciser un argument missing pour préciser comment 
#    considérer les manquants
ifelse(c(1,2,NA,4,5) <= 3, 1, 0) # renvoie NA pour la valeur manquante
if_else(c(1,2,NA,4,5) <= 3, 1, 0, missing = NULL) # renvoie NA pour la valeur manquante
if_else(c(1,2,NA,4,5) <= 3, 1, 0, missing = 9) # indique le code choisit (=9) au lieu de NA

### créer la variable obésité.
# note avec le pipe, penser à assigner le résultat dans df_1 pour que la 
# variable crée soit bien sauvegardée dans la base df_1
# (sinon, elle est uniquement crée de manière temporaire)
df_1 %>% 
  mutate(obesite = if_else(df_1$imc >= 30, 1, 0)) 
df_1 # la variable n'a pas été enregistrée dans la base df_1 !

df_1 <- df_1 %>% 
  mutate(obesite = if_else(df_1$imc >= 30, 1, 0))
df_1 # ici, on voit que la variable a bien été ajoutée dans la base df_1

# Vérifier que la variable est crée correctement en calculant le min et le max
# dans chaque groupe d'obésité nouvellement créé :
# on va utiliser la fonction summarise qui permet de calculer différents paramètres
# d'une distribution de variables, voir ?summarise()
# ces analyses peuvent se faire par sous-groupes, qui ont identifiés en amont 
# par la fonction group_by()
df_1 %>%
  group_by(obesite) %>% # prépare à une analyse par groupe
  summarise(min = min(imc), # calcul le min et max selon les groupes définis
            max = max(imc)) # par la fonction "group_by()"
# note : summarise()/summarize() peut s'écrire avec un "s" ou un "z"

### Créer la variable imc à 4 classe, on peut utiliser pour cela la fonction case_when
### qui fonctionne selon le principe de if_else, mais pour plus de 2 catégories :
### voir ?case_when : chaque catégorie va être définie par une formule avec : 
### la condition à gauche du signe ~ et la valeur à assigner à droite du signe ~
df_1 <- df_1 %>% 
  mutate(imc_cl = case_when(imc < 18.5 ~ 1,
                            imc >= 18.5 & imc < 25 ~ 2,
                            imc >= 25 & imc < 30 ~ 3,
                            imc >= 30 ~ 4))
df_1 # la variable imc_cl a été ajoutée à la base df_1

### vérifier que la variable a été correctement créée
df_1 %>%
  group_by(imc_cl) %>%
  summarise(min = min(imc),
            max = max(imc))

### le croisement entre imc en classe et obésité doit être cohérent :
with(df_1, table(as.factor(imc_cl), as.factor(obesite), deparse.level = 2))

df_1 %>% group_by(imc_cl, obesite) %>% summarise(n = n())
df_1 %>% count(imc_cl, obesite) # équivalent à la commande précédente

## le package dyplr contient également une fonction fonction rename() pour renommer
## une variable sans changer la position
df_1 %>% rename(imc_en_classe = imc_cl)

## fonction relocate() : pour changer l'ordre des colonnes
df_1
# déplacer les colonnes imc_cl et obesité (dans cet ordre) après la variable imc
df_1 %>% relocate(imc_cl, obesite, .after = imc)

### Compléter la base de méta-données en ajoutant des lignes
# la fonction add_row() du package tibble permet d'ajouter des lignes
meta_df_1
# on commence par rajouter les labels de la variable imc_cl
meta_df_1 <- meta_df_1 %>% add_row(var = rep("imc_cl", 4), 
                                   label = rep("IMC en classes", 4), 
                                   id_labs = 1:4, 
                                   code_labs = 1:4,
                                   labs = c("Maigreur", "Normal", "Surpoids", "Obèse"))
meta_df_1
# on ajoute les lables de la variable obesite 
# mais en les positionnant juste avant les labels de imc_cl
meta_df_1 <- meta_df_1 %>% add_row(var = rep("obesite", 2), 
                                   label = rep("Obésité", 2), 
                                   id_labs = c(1,2), 
                                   code_labs = c(0,1),
                                   labs = c("Non", "Oui"), 
                                   .before = 9) # avant la 9ème ligne
                      
## 3.2) Modifier des variables ----
df_1 %>% filter(subjid %in% 135:140)
# modifier la valeur de PAS du patient n°137 de 133 mmHg à  123 mmHg
# change la valeur à 123 si subjid == 137, sinon garde la valeur pas originale
df_1 <- df_1 %>% mutate(pas = ifelse(subjid == 137, 123, pas)) 
# vérifier :
df_1 %>% filter(subjid %in% 135:140)

## autre possibilité avec la fonction rows_update() où indique un sous-ensemble 
##de la base mise à jour
df_1 %>% # ici, on fait une transformation temporaire, car pas d'assignation
  rows_update(tibble(subjid = 137, pas = 133)) %>% # revient à la valeur de 133
  filter(subjid %in% 135:140) # visualiser les lignes subjid = 135 à 140

# 4) Sauvegarder la base de données ----
# fonction write_csv() et write_csv2() du tidyverse sont analogues aux fonctions
# write.csv() et write.csv2() de Rbase
# cf ?write_delim
write_csv2(df_1, "data/df_1_new.csv")


# 5) Analyses univariées ----
## 5.0) création de 2 variables de type "factor" à partir des variables sex et trait
## avec le package forcats
## https://forcats.tidyverse.org/

### on peut utiliser directement la fonction factor de Rbase
# df_1$sexL <- factor(df_1$sex,
#                     labels = meta_df_1$labs[meta_df_1$var == "sex"])
# df_1$traitL <- factor(df_1$trait,
#                       labels = meta_df_1$labs[meta_df_1$var == "trait"])

### syntaxe dyplr avec factor au sein de la fonction mutate()
### (c'est la même fonction factor de Rbase)
df_1 <- df_1 %>% 
  mutate(sexL = factor(sex, 
                       labels = meta_df_1$labs[meta_df_1$var == "sex"]),
         traitL = factor(trait))
df_1 # on voit que les deux variables sexL et traitL ont été ajoutées à df_1

### Le package forcats du tidyverse apporte de nombreuses fonctions 
### pour manipuler les variables de type "factor". Toutes ces fonctions
### commences par "ftc" (pour "factor").
### voir le cheatsheet https://raw.githubusercontent.com/rstudio/cheatsheets/main/factors.pdf


### Par exemple : 

### les labels n'ont pas été utilisés pour la variable traitL, 
### on observe donc les codes 1, 2, 3 à la place
### on peut recoder ces labels à la main avec la fonction fct_recode()
##  pour se rappeler du codage, on peut aller rechercher dans la base de 
## métadonnées
meta_df_1 %>% 
  filter(var == "trait")

df_1$traitL <- fct_recode(df_1$traitL, 
                          Placebo = "1", # nouveau label = "ancien label"
                          "Traitement A" = "2", 
                          "Traitement B" = "3")
# s'il y a des caractètres spéciaux (comme des espaces, le nouveau nom doit être
# entre guillements)

## note que la notation avec le pipe ci-dessous 
df_1 %>% fct_recode(traitL, 
                    Placebo = "1", # nouveau label = "ancien label"
                    "Traitement A" = "2", 
                    "Traitement B" = "3") 
## ne fonctionne pas car le premier argument de fct_recode doit être un facteur. 
## Cette écriture avec le pipe extrait la colonne au format "tibble" 
## (pas au format "factor") 



## 5.1) fonction summarise() ----
?summarize()
# permet de décrire différents paramètres avec les fonctions suivantes à indiquer 
# au sein de la fonction summarize()
#  - moyenne et écart type avec mean() et sd()
#  - min, max, médiane et intervalles interquartiles : min(), max(), media() IQR(), quantile()
#  - compte avec n() et n_distinct()

## 5.2) variables quantitatives ----
# on peut décrire pour une variable quantitative, 
# le nombre de valeurs non-manquantes, la moyenne, l'écart type, 
# le min, le max, l'intervalle interquartile et la médiane

## pour l'imc
df_1 %>% summarise(n = sum(!is.na(imc)), # nombre de non-manquants
                   mean = mean(imc), 
                   sd = sd(imc), 
                   min = min(imc), 
                   p25 = quantile(imc, probs = 0.25),
                   med = median(imc),
                   p75 = quantile(imc, probs = 0.75),
                   max = max(imc))

# df_1 %>% summarise(mean = mean(imc, pas)) # ça ne fonctionne pas

## pour faire un descriptif de plusieurs variables quantitatif, on peut utiliser 
## des fonctions connexes de summurize : summarise_at(), summarise_all(), 
## etc. voir ? summarise_at()
df_1 %>% summarise_at(c("imc", "pas"), 
                      list(mean = mean, 
                           sd = sd), 
                      na.rm = TRUE)
# il crée une nouvelle variable à chaque pour la présenter en tibble.
# pas très pratique pour décrire plus de paramètre sur un grand ensemble de variables

row_imc <- df_1 %>% 
  summarise(n = sum(!is.na(imc)),
            mean = mean(imc, na.rm = TRUE),
            sd = sd(imc, na.rm = TRUE), 
            min = min(imc, na.rm = TRUE), 
            p25 = quantile(imc, probs = 0.25, na.rm = TRUE),
            med = median(imc, na.rm = TRUE),
            p75 = quantile(imc, probs = 0.75, na.rm = TRUE),
            max = max(imc, na.rm = TRUE)) %>%
  mutate(var = "IMC (kg/m2)") %>%
  relocate(var, .before = n)

row_pas <- df_1 %>% 
  summarise(n = sum(!is.na(pas)),
            mean = mean(pas, na.rm = TRUE),
            sd = sd(pas, na.rm = TRUE), 
            min = min(pas, na.rm = TRUE), 
            p25 = quantile(pas, probs = 0.25, na.rm = TRUE),
            med = median(pas, na.rm = TRUE),
            p75 = quantile(pas, probs = 0.75, na.rm = TRUE),
            max = max(pas, na.rm = TRUE)) %>%
  mutate(var = "PAS (mmHg)") %>%
  relocate(var, .before = n)

## on a vu les fonctions Rbases rbind() et cbind() pour fusionner des vecteurs 
## ou des matrices ou des data.frame par rang ou par colonnes 
## Au sein du package dplyr du tidyverse, nous avons également les fonctions 
## bind_rows() et bind_cols() (avec des options et propriétés un peu différentes)
bind_rows(row_imc, row_pas)


## 5.3) variables qualitatives ----
### pour décrire les variables qualitatives, on peut utiliser les fonctions
### table et prop.table() de Rbase
table(df_1$traitL)
prop.table(table(df_1$traitL))

### il existe également une fonction fct_count() du package forcats
### pour dénombrer le nombre de réponses par modalités
fct_count(df_1$traitL)
# par rapport à la fonction table, la fonction fct_count()
# contient 2 arguments intéressants: 
# sort = TRUE pour trier les résultats du plus fréquent au moins fréquent
# prop = TRUE pour indiquer les résultats en %
fct_count(df_1$traitL, 
          sort = TRUE, # les résultats étaient déjà triés du + au - fréquent
          prop = TRUE)

# pour appliquer une syntaxe en "pipe", fct_count prend pour 1er argument 
# un vecteur de type "factor". Il faut donc utiliser la fonction "pull()" plutôt
# que "select()" pour sélectionner une variable et la retourner au format vecteur
df_1 %>%
  pull(traitL) %>%
  fct_count(sort = TRUE, prop = TRUE)


### combiner dans une seule table la description des variables sex et trait
bind_rows(tibble(f = meta_df_1$label[meta_df_1$var == "sex"][1]),
          fct_count(df_1$sexL, sort = TRUE, prop = TRUE),
          tibble(f = meta_df_1$label[meta_df_1$var == "trait"][1]),
          fct_count(df_1$traitL, sort = TRUE, prop = TRUE)) %>%
  rename("Variable" = f, 
         "%" = p)


# 6) Représentations grahpiques ----
## 6.1) Distributions univariées ----
## Le tidyverse utilise le package ggplot2 https://ggplot2.tidyverse.org/articles/ggplot2.html 
## pour réaliser des graphiques
## Autres aides : 
?ggplot2
## voir le livre R for data science pour de nombreux exemples de représentations
## graphiques
## https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf
## il existe un livre ggplot2 https://ggplot2-book.org/

## Les figures sont composées de calques successifs
## on commence par indiquer une première ligne avec comme argument : 
##  - les données
##  - les variables à utiliser (aesthetics)
ggplot(data = df_1, mapping = aes(x = imc))
## cela prépare un fond de graphique (1er calque)

## puis on ajoute un ou plusieurs calques (avec des signes +)
## pour les variables quantitatives 
##  - histogram
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_histogram()
# pour voir les options possibles de l'histogramme : 
?geom_histogram
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_histogram(bins = 10, # nombre de "rectangles" default = 30
                 colour = "black", # couleur du contour
                 linewidth = 3, # épaisseur de ligne
                 fill = "dodgerblue3")

##  - densité de kernel
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_density()
# pour voir les options possibles de les densités de kernel : 
?geom_density
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_density(bw = "nrd", n = 100, # bandwidth selector
               kernel = "rectangular") # kernel function
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_density(bw = "nrd0", # default bandwidth selector
               kernel = "gaussian", # default kernel function
               colour = "red", # couleur du contour
               linewidth = 2, # épaisseur de ligne
               linetype = 4) # type de ligne (dotdash)
# type de lignes : 
# 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 
# 6 = twodash

##  - boxplot
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_boxplot()

# représentation sur l'axe des Y
ggplot(data = df_1, mapping = aes(y = imc)) +
  geom_boxplot()

# pour voir les options possibles de les densités de kernel : 
?geom_boxplot
ggplot(data = df_1, mapping = aes(x = imc)) +
  geom_boxplot(outliers = TRUE, # afficher les outliers
               outlier.colour = "blue",
               outlier.fill = "green",
               outlier.shape = 7, # symbol des outlier voir ?shape
               outlier.size = 5, # taille des outliers
               outlier.stroke = 0.1, # épaisseur de trait des outliers
               outlier.alpha = NULL, # transparence des outliers
               staplewidth = 0.1) # largeur de la moustache

### Diagrammes en barres, appliqués aux "facteurs"
ggplot(data = df_1, mapping = aes(x = sexL)) +
  geom_bar()
# pour voir les options possibles de les diagrammes en barres : 
?geom_bar

# sur l'axe des Y plutôt que des X
ggplot(data = df_1, mapping = aes(y = sexL)) + # ici y = sexL
  geom_bar()

# afficher des % plutôt que des comptes
## ? je ne sais pas faire !!

# options
ggplot(data = df_1, mapping = aes(x = sexL)) +
  geom_bar(mapping = aes(y = after_stat(prop)), # pour des % plutôt que des N
           just = 0.1, # emplacement : 0=à gauche, 1=à droite, 0.5=centré
           width = 0.5, # largeur de bande (0.9 par défaut)
           colour = "red", # couleur de ligne
           linewidth = 2, # largeur de ligne
           fill = "orange2") # couleur de remplissage




## 6.2) Distributions bivariées ----

## on peut relabeliser les axes avec un filtre "labs"


### boxplots
### PAS en fonction du sex. 
ggplot(data = df_1, mapping = aes(x = sexL, y = pas)) +
  geom_boxplot(outliers = TRUE, staplewidth = 0.2) + 
  labs(x = "Sexe", y = "PAS (mmHg)", 
       title = "PAS en fonction du sexe", 
       subtitle = "sous-titre")

### PAS en fonction du traitement
ggplot(data = df_1, mapping = aes(x = traitL, y = pas)) +
  geom_boxplot(outliers = TRUE, staplewidth = 0.2) + 
  labs(x = "Traitement", y = "PAS (mmHg)", 
       title = "PAS en fonction du traitement")

### PAS en fonction du traitement et du sexe 
ggplot(data)

### Répartition du sexe par traitement
ggplot(data = df_1, mapping = aes(x = traitL, fill = sexL)) +
  geom_bar(position = "stack") + # empilé
  labs(x = "Traitement", title = "Sexe et traitement")

ggplot(data = df_1, mapping = aes(x = traitL, fill = sexL)) +
  geom_bar(position = "dodge") + # côte à côte
  labs(x = "Traitement", title = "Sexe et traitement")

### On peut représenter un nuage de points de la PAS en fonction de l'IMC : 
### où les hommes et les femmes ont deux symboles différents et les traitements 
### deux couleurs différentes
ggplot(data = df_1, mapping = aes(y = pas, 
                                  x = imc,
                                  shape = sexL,
                                  color = traitL)) +
  geom_point() 

ggplot(data = df_1, mapping = aes(y = pas, x = imc, shape = sexL, 
                                  color = traitL)) +
  geom_point(size = 2) + # taille des symboles
  labs(x = "IMC (kg/m²)", y = "PAS (mmHg)", # titre et axes
       title = "PAS en fonction de l'IMC") + 
  scale_colour_discrete(name = "Traitement", # 1ère légende liée aux couleurs
                          breaks = c("Placebo", "Traitement A", "Traitement B"),
                          labels = c("Placebo", "Traitement", "Traitement B")) +
  scale_shape_discrete(name = "Sex", # 2ème légende liée aux symboles
                       breaks = c("Féminin", "Masculin"),
                       labels = c("Féminin", "Masculin")) + 
  theme(axis.text = element_text(size = 12)) # taille des valeurs sur les axes
    

# si on veut choisir les symboles ?? et le faire en noir et blanc

# 7) Analyses bivariées ----
## 7.1) Variable quantitative $\times$ qualitative ----
## dplyr permet décrire une variable quantitative en fonction d'une variable, 
## qualitative, avec group_by
df_1 %>%
  group_by(sexL)
# notez qu'en description, le tibble indique qu'il considère la variable sexL
# comme une variable de groupe => cela pris en compte pour les analyses qui 
# suivront dans le pipe.
# Si vous avez sauvegardé une base tibble avec une propriété de sous-groupe, 
# vous pouvez la supprimer avec la fonction ungroup()
df_1 %>%
  group_by(sexL) %>%
  ungroup()

## IMC en fonction du sexe
df_1 %>%
  group_by(sexL) %>%
  summarise(n = sum(!is.na(imc)), # nombre de non-manquants
            mean = mean(imc), 
            sd = sd(imc))

## PAS en fonction du traitement
df_1 %>%
  group_by(traitL) %>%
  summarise(n = sum(!is.na(pas)), # nombre de non-manquants
            mean = mean(pas), 
            sd = sd(pas))

## 7.2) Variable qualitative $\times$ qualitative ----
### Décrire les effectifs et % de la variable sexL (en lignes),
### en fonction de la variable traitL (en colonnes) 
df_1 %>%
  group_by(sexL) %>%
  summarise("Placebo, n" = sum(trait == 1), # effectif
            "Placebo, %" = mean(trait == 1) * 100, # % en ligne (selon le sex)
            "Traitement A, n" = sum(trait == 2), 
            "Traitement A, %" = mean(trait == 2) * 100,
            "Traitement B, n" = sum(trait == 3), 
            "Traitement B, %" = mean(trait == 3) * 100)

## 7.3) Comparer 2 moyennes ----
# le test de Student se fait avec les commandes R base
# on peut vérifier la normalité des distributions dans chaque groupe
# avec ggplot2
ttest_pas_sex <- df_1 %>% t.test(data = ., # placeholder pour indiquer la base de données
                                 pas ~ sexL, # formula
                                 var.equal = TRUE) # pour le test de Student

# test de Levene
df_1 %>% car::leveneTest(data = ., 
                         pas ~ sexL) 

# distribution 
df_1 %>% 
  filter(sex == 0) %>%
  ggplot(aes(x = pas)) + 
  geom_density() + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_1$pas[df_1$sex == 0]), 
                            sd = sd(df_1$pas[df_1$sex == 0])),
                col = "red", lwd = 0.5, lty = "dashed")

df_1 %>% 
  filter(sex == 1) %>%
  ggplot(aes(x = pas)) + 
  geom_density() + 
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_1$pas[df_1$sex == 1]), 
                            sd = sd(df_1$pas[df_1$sex == 1])),
                col = "red", lwd = 0.5, lty = "dashed")

# on peut également combiner 2 QQ-plots 
df_1 %>% ggplot(mapping = aes(sample = pas, colour = sexL)) + 
  stat_qq() + 
  stat_qq_line() 

# si on veut appliquer un test de Wilcoxon
df_1 %>% wilcox.test(data = ., # placeholder
                     pas ~ sex) # p-value = 1.529e-06


## 7.4) Comparer 3 moyennes ou plus ----
# on peut utiliser le pipe pour enchaîner les analyses nécessaires à une anova
anova_pas_trait <- df_1 %>%
  lm(pas ~ traitL, 
     data = .) %>% # placeholder
  anova()
# un inconvenient est qu'aucun des objets calculés n'est en mémoire, 
# on récupérer simplement le résultat de la table d'anova (dernière commande)

# pour tester la normalité avec un QQ-plot sur les résidus du modèle linéaire : 
mod_pas_trait <- df_1 %>%
  lm(pas ~ traitL, 
     data = .)

ggplot(mapping = aes(sample = mod_pas_trait$residuals)) + 
  geom_qq() + 
  stat_qq() + stat_qq_line() # ajoute la ligne de référence

## si on veut faire un test de Kruskal Wallis
df_1 %>% 
  kruskal.test(pas ~ traitL, data = .) # p-value = 7.336e-05


## 7.5) Comparer des pourcentages ----
# on utilise la même fonction qu'en R base, exemple avec un pipeline
chi2 <- df_1 %>% 
  with(table(sexL, traitL)) %>%
  chisq.test(correct = FALSE)

# effectifs attendus
chi2$expected

# test exact de Fisher
df_1 %>% 
  with(table(sexL, traitL)) %>%
  fisher.test()

## 7.6) Corrélations ----
# corrélation de Pearson entre PAS et IMC
# corrélations de Pearson et de Spearman
df_1 %>% with(cor.test(imc, pas, method = "pearson"))
df_1 %>% with(cor.test(imc, pas, method = "spearman"))


# 8) Analyse multivariée ----
# les commandes sont les mêmes, exemple avec pipeline
model <- df_1 %>% lm(pas ~ traitL + sexL + imc, 
                     data = .)

df_1 %>% 
  lm(pas ~ traitL + sexL + imc, 
     data = .) %>%
  summary()

# graphique de diagnostic des rédidus en fonction des valeurs prédites
ggplot() + 
  geom_point(mapping = aes(y = model$residuals, # résidus
                           x = model$fitted.values)) + # valeurs prédites
  geom_smooth(mapping = aes(y = model$residuals, 
                           x = model$fitted.values)) + 
  labs(y = "résidus", x = "valeurs prédites")

# par défaut, geom_smooth fait un spline
# si on veut une fonction loess comme dans l'exemple du chap 2
ggplot() + 
  geom_point(mapping = aes(y = model$residuals, # résidus
                           x = model$fitted.values), # valeurs prédites
             shape = 1) + # symbole des points
  geom_smooth(mapping = aes(y = model$residuals, 
                            x = model$fitted.values),
              method = "loess", # méthode de lissage
              colour = "red", # couleur de ligne
              linetype = 2, # type de ligne
              linewidth = 0.5, # largeur de ligne
              se = FALSE) + # supprime la bande d'IC95% 
  labs(y = "résidus", x = "valeurs prédites")
               
## QQ plot des résidus
ggplot(mapping = aes(sample = model$residuals)) + 
  geom_qq(shape = 1) + 
  stat_qq(shape = 1) + stat_qq_line() 