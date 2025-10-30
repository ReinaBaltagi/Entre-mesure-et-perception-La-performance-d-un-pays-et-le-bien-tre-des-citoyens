#Nettoyer l'environment de travail
rm(list = ls())

#Définir le répertoire de travail 
setwd("C:/Users/dell/Desktop/Memoire") 
#tu adaptes selon ton pc :)

# Charger les Packages Necesaires 
library(dplyr)
library(readr)
library(tidyr)
library(corrplot)
library(car)
library(lmtest)
library(FactoMineR)
library(factoextra)
library(ggplot2)

#Importer la base de données
df<-read.csv("Final_Database_Clean.csv")

#Afficher le nombre de lignes
nrow(df)

#Afficher le nombre de colonnes
ncol(df)

#Afficher les noms des variables 
names(df)

#Afficher les types des variables
sapply(df, class)

#On simplifie les noms de variables parce qu'ils étaient trop longs et pas pratique
#à utiliser dans le code
df<-rename(df,
           Gini=Gini.coefficient,
           EPI=EPI.new,
           Edu_Spending=Public.spending.on.education.as.a.share.of.GDP,
           Years_Schooling=Average.years.of.schooling,
           Primary_Enroll=PrimaryEnrollmentRate,
           Gov_Effectiveness=Government.Effectiveness,
           Stability=Political.Stability.and.Absence.of.Violence.Terrorism,
           Rule_Law=Rule.of.Law,
           Happiness=Ladder.score,
           Log_GDP=Logged.GDP.per.capita,
           Social_Support=Social.support,
           Life_Expectancy = Healthy.life.expectancy,
           Freedome_Choices=Freedom.to.make.life.choices,
           Corruption = Perceptions.of.corruption,
           Generosity = Generosity)

#On vérifie la bonne implémentation du changement de noms
names(df)


# Supprimer la colonne "Country" pour ne garder que les variables numériques
df_numeric <- select(df, -Country)

# Center et réduidre toutes les colonnes de df_numeric
df_scaled <- as.data.frame(scale(df_numeric))


desc_stats <- summarise_all(
  df_numeric,
  list(
    Moyenne = function(x) round(mean(x), 2),
    Mediane = function(x) round(median(x), 2),
    Min = function(x) round(min(x), 2),
    Max = function(x) round(max(x), 2),
    Ecart_type = function(x) round(sd(x), 2)
  )
)

print(desc_stats)
View(desc_stats)

# Calculer la matrice de corrélation
cor_matrix<-cor(df_numeric,use = "complete.obs")

# Afficher la matrice de corrélation
corrplot(cor_matrix,
         method = "color",       
         type = "full",         
         addCoef.col = "black",  
         tl.col = "black",       
         tl.cex = 0.7,           
         number.cex = 0.6,       
         tl.srt = 45)  

# Selon la matrice de corrélation, on observe des relations très fortes entre plusieurs variables.
# Par exemple, Rule_Law et Gov_Effectiveness sont corrélées à 0.94, Log_GDP et Life_Expectancy à 0.89, 
# ainsi que d'autres combinaisons similaires. Ces variables apportent donc des informations proches,
# ce qui peut indiquer un risque de multicolinéarité.

# Quand on regarde la variable Happiness et sa corrélation avec les variables explicatives, 
# on remarque une forte corrélation avec plusieurs d’entre elles : 
# Social_Support (0.84), Log_GDP (0.87), EPI (0.83), Stability (0.79), 
# Life_Expectancy (0.78), Rule_Law (0.77) et Years_Schooling (0.77).
# Cela laisse penser que ces variables peuvent bien expliquer le niveau de bonheur.

# En revanche, Generosity (0.08) ne montre pratiquement aucun lien avec Happiness, 
# ce qui indique qu’elle ne joue probablement pas un grand rôle  dans son explication.

modele_All_V <- lm(Happiness ~ Gini + EPI + Edu_Spending + Years_Schooling + Primary_Enroll 
             + Gov_Effectiveness + Stability + Rule_Law + Log_GDP + Social_Support + Life_Expectancy 
             + Freedome_Choices + Corruption + Generosity , data = df_scaled)

# On a effectué un test VIF pour vérifier de plus la multicolinéarité entre les variables explicatives du modèle  
vif(modele_All_V)

# Les variables Log_GDP, Rule_Law et Gov_Effectiveness présentent des valeurs bien supérieures à 10
# ce qui montre qu’elles apportent presque toutes la même information.
# Cela peut poser problème en rendant les estimations instables ou difficiles à interpréter.
# Donc il serait mieux d'en retirer au moins une pour améliorer la qualité du modèle.

# D’autres variables comme Years_Schooling, Life_Expectancy, Social_Support ou encore EPI
# présentent des VIF modérément élevés (entre 5 et 10), ce qui indique la présence de multicolinéarité
# mais d'une façon modérée, ce qui reste acceptable mais à surveiller pour éviter une multicolinéarité excessive 

# Pour les autres variables, elles présentent des VIF inférieur à 5, ce qui indique une bonne indépendance 
# par rapport au autre variable du modèle.


#######------ Visualisation des distributions de quelque variables

# Histogramme du score de bonheur
hist(df$Happiness, 
     main = "Distribution du bonheur", 
     xlab = "Score de bonheur (Ladder score)", 
     col = "skyblue")

# Histogramme du PIB par habitant (en log)
hist(df$Log_GDP, 
     main = "Distribution du PIB par habitant (log)", 
     xlab = "PIB par habitant en log (Log GDP per capita)", 
     col = "lightgreen")

# Histogramme du soutien social perçu
hist(df$Social_Support, 
     main = "Distribution du soutien social perçu", 
     xlab = "Soutien social (Social support)", 
     col = "orange")

# Histogramme de l'indice de performance environnementale (EPI)
hist(df$EPI,
     main = "Distribution de l'indice environnemental (EPI)",
     xlab = "Indice de performance environnementale",
     col="lightpink"
     )

# Ici on affiche les 10 pays les plus heureux les 10 moins heureux.
# Cela nous donne une vue conctrète sur les écarts de bonheur dans notre échantillon

top10 <- df[order(-df$Happiness),c("Country","Happiness")][1:10,]
print(top10)

bottom10 <- df[order(df$Happiness),c("Country","Happiness")][1:10,]
print(bottom10)


#######------ Coefficient de variation = écart-type / moyenne 
# c'est un coefficient qui aide à identifier les variables les plus dispersées relativement à leur moyenne
cv <- sapply(df_numeric, function(x) sd(x) / mean(x))
cv <- round(cv, 2)
cv
# Les variables comme Life_expectancy (cv=0.07) ou Primary_enroll (cv=0.09) varient très peu par rapport à leur moyenne
# ce qui signifie qu'elles sont trés similaires dans la majorité des pays
# Elles n'apportent pas d'information différenciante, donc il ya un risque qu'elles ne soient pas significatives 
# dans un modèle de regression 

# Alors que les variables comme Generosity (cv=34.05) ou Stability (cv=21.68) varient énormément d'un pays à l'autre.
# Cette grande dispersion peut rendre leur interprétation difficile et meme rendre les résultats instables.

# On confirmera ou non ces hypothèses lors de l'analyse de la régression.


########------ Corrélation entre chaque variable et Happiness
cor_Happiness<-cor(df_numeric, use="complete.obs")[,"Happiness"]
cor_Happiness_sorted<-sort(cor_Happiness,decreasing = TRUE)
round(cor_Happiness_sorted,2)

# Ici on classe les variables selon leur corrélation avec Happiness
# ce qui permet d'identifier les meilleurs prédicteurs potentiels


# Le boxplot permet de visualiser les valeurs extreme et la dispersion de la variable Happiness
boxplot(df$Happiness, 
        main = "Boxplot d'Indice de bonheur", 
        col = "lightgreen", 
        horizontal = TRUE)


#On essaye le modèle de régression sans la variable Government_effectivness pour éviter le risque de multicolinéarité
modele_sans_gov_effectivness<-lm( Happiness~ Gini + EPI + Edu_Spending + Years_Schooling + Primary_Enroll 
                                  + Rule_Law + Stability  + Log_GDP + Social_Support + Life_Expectancy 
                                  + Freedome_Choices + Corruption + Generosity , data = df_scaled)
summary(modele_sans_gov_effectivness)

AIC(modele_All_V,modele_sans_gov_effectivness)

#On trouve que celui sans la rule of Law est légèrement meielleur (38.3<40.04)


################################################################################################
############################            Partie regression              #########################
################################################################################################

summary(modele_All_V)

# modele_All_V : c'est un modèle de regression multiple qui inclut toutes les 14 variables explicatives
# Le R² ajusté est de 0.8869, ce qui signifie que le modèle explique environ 88.7% des variations de la variable bonheur
# Le test de fisher est très significative (p < 2.2e-16), ce qui indique que le modèle global est statistiquement pertinent.

# Parmi les 14 variables, seules 4 sont significatives au seuil de 5%
# 1- Social_Support (p=0.0012): un effet fortement significative et postive 
# 2- Corruption (p=0.00773): un effect négative et aussi fortement significative
# une perception de corruption plus elevée est asscoié à un niveau de bonheur plus faible
# 3- EPI (p = 0.0402): effet positif et modérément significatif.
# 4- Log_GDP (p = 0.0329): Le PIB par habitant est positivement lié au bonheur.
# Les autres variables ne sont pas significatives

# À noter que la variable Gini (p = 0.10975) est proche du seuil de 10 %, ce qui suggère un effet potentiel
# Il est donc pertinent de construire deux modèles réduits : 
# - le premier contient uniquement les variables significatives
# - le second ajoute la variable Gini pour tester son effet

modele_reduit <- lm(Happiness ~ Social_Support + Log_GDP + Corruption + EPI, data = df_scaled)
summary(modele_reduit)

# modele_reduit : c’est un modèle basé uniquement sur les variables significatives du modèle complet
# Il inclut : Social_Support, Log_GDP, Corruption et EPI

# Le R² ajusté est de 0.8832, très proche de celui du modèle complet (0.8869)
# Cela montre que le modèle réduit conserve pratiquement le même pouvoir explicatif, avec moins de variables
# Le test de Fisher reste très significatif (p < 2.2e-16), ce qui confirme la pertinence globale du modèle

# Toutes les variables du modèle sont significatives ou proches du seuil :
# 1- Social_Support (p = 2.28e-07) : effet très significatif et positif
# 2- Corruption (p = 2.28e-05) : effet très significatif et négatif
# 3- Log_GDP (p = 0.0051) : effet significatif et positif
# 4- EPI (p = 0.06399) : légèrement au-dessus du seuil de 5 %, mais reste proche

# Ce modèle est donc plus simple, plus clair, et reste très performant statistiquement.
# On test le modèle réduit mais cette fois sans EPI pour voir si ul'EPI avec ce dépassement de seuil reste pertinente pour le modèle ou pas. 

modele_reduit_withoutEPI <- lm(Happiness ~ Social_Support + Log_GDP + Corruption, data = df_scaled)
summary(modele_reduit_withoutEPI)

AIC(modele_reduit, modele_reduit_withoutEPI)

# Le modèle modele_reduit_withoutEPI a un R² ajusté qui est légèrement inférieur 0.8778< 0.8832, 
# ce qui montre qu'on perd un peu du pouvoir explicative quand on retire la variable EPI.

# De plus, le critère AIC est plus élevé pour ce modèle 49.99>48.21, 
# ce qui signifie qu’il est moins performant en termes d'ajustement et complexité.

# On peut donc conclure que la variable EPI, meme si qu'elle est modérément significative, apporte quand meme un pouvoir explicative au modèle.
# Donc on retient le modèle réduit avec EPI.


modele_reduit_with_Gini <- lm(Happiness ~ Social_Support + Log_GDP + Corruption + EPI + Gini, data = df_scaled)
summary(modele_reduit_with_Gini)

# modele_reduit_with_Gini : c’est un modèle basé sur les variables significatives + la variable Gini, 

# Le R² ajusté est de 0.8916 ce qui est légèrement supérieur à celui du modèle réduit sans Gini (0.8832) et du modèle complet avec toutes les variables (0.8869 )
# Cela montre que l’ajout de la variable Gini améliore légèrement la capacité explicative du modèle
# Le test de Fisher reste très significatif (p < 2.2e-16), ce qui confirme la pertinence globale du modèle

# Dans ce modèle, toutes les variables sont significatives au seuil de 5 % :
# 1- Social_Support (p = 1.49e-07) : effet très significatif et fortement positif
# 2- Corruption (p = 3.46e-06) : effet très significatif et négatif
# 3- Log_GDP (p = 0.00378) : effet positif et significatif
# 4- EPI (p = 0.02581) : effet modérément significatif
# 5- Gini (p = 0.02572) : effet qui devient significatif au seuil de 5 %, ce qui confirme la pertinence de son ajout

# Donc, l'ajout de Gini qui était au début  proche du seuil de 10 % , apporte un pouvoir explicative de plus et améliore la performance du modèle

# On compare maintenant tous les modèles en utilisant AIC pour voir lequel est le plus performant

AIC(modele_All_V, modele_reduit, modele_reduit_withoutEPI,modele_sans_gov_effectivness, modele_reduit_with_Gini)

# Le modèle modele_réduit_with_Gini présente la valeur d'AIC la plus faible (44.63875) entre tous les modèles testées 
# ce qui en fait le modèle le plus efficace et le plus pertinent

# Ici, on s'assure meme si on a retiré les variable corrélées , s'il ya encore de multicolinéarité
vif(modele_reduit_with_Gini)
# Le VIF des variables du modèle réduit avec Gini est inférieur à 5 :
# Social_Support (3.121166), Log_GDP (4.227595), Corruption (1.631578), EPI(3.539345), Gini (1.204783)
# Cela indique qu'il n'y a pas de problème de multicolinéarité entre ces variables
# Ce résultat renforce la pertinence du modèle réduit.

# Ici, on utilise le test ANOVA pour comparer le modéle réduit au modèle réduit avec gini
anova(modele_reduit,modele_reduit_with_Gini)
# La p-value est de 0.02572, ce qui inférieur à 5%, donc on rejette l'hypothèse nulle: Gini n'apporte aucune information supplémentaire.
# Cela confirme que le modèle réduit avec Gini est statistiquement préférable.


# Test de normalité de résidu de Shapiro sur les résidus de modèle choisit (le modèle réduit)
shapiro.test(residuals(modele_reduit_with_Gini))
# Ici, la p-value = 0.7768 qui est supérieur à 0.05 donc on ne rejette pas l'hypothèse nulle
# Les résidus suivent une distribution normale, ce qui valide cette hypothèse du modèle linéaire

# Test de Breusch-Pagan pour l’homoscédasticité des résidus
bptest(modele_reduit_with_Gini)
# Ici, la p-value = 0.9 > 0.05 , donc on ne rejette pas l’hypothèse nulle de variance constante
# Il n’y a donc pas de problème d’hétéroscédasticité dans le modèle réduit

# Test de Durbin-watson pour l'indépendance des erreurs
dwtest(modele_reduit_with_Gini)
# Ici, la p-value = 0.09496 > 0.05, donc on ne rejette pas l'hypothèse nulle d'indépendance des erreurs
# Donc il y a pas de probleme d'autocorrélation des résidus, l'hypothèse est donc respectée

# Les quatres graphiques des résidus
par(mfrow = c(2, 2))
plot(modele_reduit_with_Gini)

################################################################################################
###############         Partie création du pays parfait & les scores       #####################
################################################################################################

#Construction du profil synthétique 

# Variables sélectionnées du modèle réduit avec Gini
vars_utiles <- c("Log_GDP", "Social_Support", "EPI" ,"Corruption", "Gini")

# Création du pays parfait ayant les meilleures valeurs pour chaque variable

# On maximise Log_GDP, Social_Support, EPI et minimise Corruption et
pays_parfait <- c(
  max(df$Log_GDP, na.rm = TRUE),
  max(df$Social_Support, na.rm = TRUE),
  max(df$EPI, na.rm = TRUE),
  min(df$Corruption, na.rm = TRUE),
  min(df$Gini, na.rm = TRUE)
)

names(pays_parfait) <- vars_utiles 

print("Profil du pays parfait (meilleures valeurs observées) :")
print(round(pays_parfait, 3))

pays_max <- sapply(vars_utiles, function(v) {
  if (v %in% c("Corruption","Gini")) {
    df$Country[which.min(df[[v]])] 
  } else {
    df$Country[which.max(df[[v]])]
  }
})

print("Pays ayant les meilleures valeurs pour chaque indicateur :")
print(pays_max)

# On peut définir le pays parfait par ce profil synthétique 
# avec les meilleures valeurs observées pour chaque variable explicative du modèle réduit. 

# Ce pays a un PIB par habitant en logarithme de 11,660 avec un niveau de soutien social de 0,969,
# un indice de performance environnementale (EPI) de 77,9 ainsi q'un faible niveau de corruption perçue (0,182) 
# ainsi qu’un faible indice d’inégalités (Gini) de 0,243.


######################## Le scoring synthétique

#---- Méthode 1 : Score pondéré avec les coefficients de la régression

# On récupère les coefficients du modèle réduit (sans l'intercept)
coefs <- coef(modele_reduit_with_Gini)[-1]

print("Coefficients de régression utilisés comme pondération :")
print(round(coefs, 3))

# On observe à travers les coefficients que la variable la plus influante est celle du social support avec un coefficient de 0.457
# La varuable la moins influente est l'EPI avec un coefficient de 0.108

# On réordonne les coefficients pour que ça soit correspondant à l'ordre de vars_utiles définis au début
coefs <- coefs[vars_utiles]

# Les variables sont centrées et réduites ici car le modèle de régression (modele_reduit_with_Gini) a été estimé avec des données standardisées.
# Donc il est necessaire d'utiliser les memes transformations pour quoi les scores calculés soint juste.
# Sinon, on appliquerait les coefficients du modèle standarisé sur des valeurs qui ne sont pas à la même échelle, ce qui fausserait les résultats.

df_scaled_vars<-scale(df[, vars_utiles])

# Score pondéré pour chaque pays
df$Score_Pondere <- as.matrix(df_scaled_vars) %*% coefs

# Concernant le score du pays parfait pondéré, nous l'avons calculé en multipliant les valeurs des variables du profil du pays parfait avec les coefficients:

# On transforme le vecteur du pays parfait en ligne pour qu'on puisse faire des opérations comme si c'était un pays du tableau
pays_parfait_df <- as.data.frame(t(pays_parfait))

# On Standardise les valeurs du pays parfait avec les mêmes paramètres que df_scaled_vars
pays_parfait_scaled <- scale(pays_parfait_df,
                             center = attr(df_scaled_vars, "scaled:center"),
                             scale = attr(df_scaled_vars, "scaled:scale"))


# On calcule le score pondéré
score_parfait_pondere <- sum(pays_parfait_scaled * coefs)

# Affichage du résultat
cat("\nScore du pays parfait (pondéré) :", round(score_parfait_pondere, 3), "\n")

# Ce score parfait synthétique a été obtenu suite à la combinaison des meilleures valeurs observées pour chaque variable, 
# multipliés (pondérées) par leur degrés d'importance (coefficients) dans le modèle.

# Le score est donc de 2.022 représentant le "niveau maximal" qu'un pays fictif pourrait atteindre 
# s'il a les meilleures conditions sur tous les critères retenus dans le modèle réduit.


#----- Méthode 2 : Score non pondéré avec l'ACP

# On a deja standardisé les données pour qu'elles soient comparables donc on utilise df_scaled_vars directement
# On réalise l’ACP

res_acp <- PCA(df_scaled_vars, graph = FALSE)

# L'affichage des valeurs propres: la variance expliquée par chaque composante
eig <- res_acp$eig
print("Valeurs propres (inertie, % expliquée, % cumulée) :")
print(round(eig, 2))

# Les valeurs propres détaillent la variance qui est expliquée par chaque composante principale.
# Les résultats montrent que la PC1 explique 69,5 % de la variance totale, en plus, la PC2 ajoute 21 %, pour un pourcentage cumulé de 90,5 %.
# Les composantes 3 et 4 expliquent très peu de variance (avec 5.48% et 4% repectivement), ainsi on ne se concentre que sur PC1 et PC2.

# Graphique de la variance expliquée
fviz_eig(res_acp, addlabels = TRUE, main = "Variance expliquée par les composantes")


#Le code suivant a pour but d'afficher la contribution des variables aux composantes principales PC1 et PC2
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribution des variables à la PC1")

fviz_contrib(res_acp, choice = "var", axes = 2, top = 10) +
  labs(title = "Contribution des variables à la PC2")

#Ce cercle de corrélation montre la relation entre les variables et les axes principaux
fviz_pca_var(res_acp,
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle de corrélation des variables")

#Pour montrer la qualité de représentation des variables sur les axes à travers le cos2
print("Cos2 : qualité de représentation des variables :")
print(round(res_acp$var$cos2, 3))

# Les variables Log_GDP avec (0.864), EPI(0.851) et Social_Support(0.711), avec des cos2 proche de 1 donc ils sont très bien représentés sur la Dim.1 (PC1),
# Quant à la Corruption, elle est mieux représentée sur la Dim.2 avec cos2 de (0.616),
# Ce qui montre que la PC1 expose principalement la richesse, l’environnement ainsi que le soutien social,
# alors que la PC2 montre la dimension de la corruption.


#Il s'agit ici de la projection des pays sur le plan factoriel (avec PC1 et PC2)
fviz_pca_ind(res_acp,
             col.ind = "cos2",
             gradient.cols = c("blue", "white", "red"),
             label = "none",
             repel = TRUE,
             title = "Projection des pays dans le plan ACP (PC1 & PC2)")


# On extrait le score de chaque pays sur l'axe PC1, donc le score synthétique principal défini par l'ACP
df$Score_ACP <- res_acp$ind$coord[, 1]

# Pour calculer le score PC1 du pays parfait fictif
# On a déja standarisé les valeurs du pays fictif dans pays_parfait_scaled, donc on peut l'utiliser directement

# Calcul du score synthétique du pays parfait selon la première composante, en combinant les valeurs du pays parfait avec les poids (coordonnées) du PC1
score_parfait_acp <- sum(pays_parfait_scaled * res_acp$var$coord[, 1])
cat("\nScore du pays parfait (ACP - PC1) :", round(score_parfait_acp, 3), "\n")
#Le score synthétique obtenue(ACP-PC1) est de 7.165  


#############################################################################################################
######################      Classement des pays réel par rapport au pays parfait       ######################
#############################################################################################################


##################--------- Pour la méthode pondérée


# Nous allons faire le calcul des écarts absolus entre le score de chaque pays et le score du pays parfait
#On le fait pour les deux méthodes (Pondérée et ACP)

df <- df %>%
  mutate(Ecart_au_parfait_pond = abs(Score_Pondere - score_parfait_pondere),
         Ecart_au_parfait_acp = abs(Score_ACP - score_parfait_acp))

# On classe les pays selon leur proximité du pays parfait (Pour la méthode pondérée)
classement_pond <- df %>%
  select(Country, Score_Pondere, Ecart_au_parfait_pond, Happiness) %>%
  arrange(Ecart_au_parfait_pond)

#On affiche les 10 pays les plus proche du pays parfait selon la méthode pondérée
cat("\n--- TOP 10 pays les plus proches du pays parfait (Méthode pondérée) ---\n")
print(head(classement_pond, 10))

#On affiche les 10 pays les plus éloignés du pays parfait selon la méthode pondérée
cat("\n--- BOTTOM 10 pays les plus éloignés du pays parfait (Méthode pondérée) ---\n")
print(tail(classement_pond, 10))


##################--------- Pour la méthode ACP

#On classe les pays selon leur proximité du pays parfait (Pour ACP)
classement_acp <- df %>%
  select(Country, Score_ACP, Ecart_au_parfait_acp, Happiness) %>%
  arrange(Ecart_au_parfait_acp)

#On affiche les 10 pays les plus proche du pays parfait selon la méthode de l'ACP
cat("\n--- TOP 10 pays les plus proches du pays parfait (ACP - PC1) ---\n")
print(head(classement_acp, 10))

#On affiche les 10 pays les plus éloignés du pays parfait selon la méthode de l'ACP
cat("\n--- BOTTOM 10 pays les plus éloignés du pays parfait (ACP - PC1) ---\n")
print(tail(classement_acp, 10))

#Nous pouvons constater à priori que les deux méthodes semblent associer des pays proche de celui parfait ayant des hauts niveau de "Happiness", 


#Pour plus de pertinence méthodologique, nous avions décidé de tester la corrélation de chaque score avec la variable Happiness
cor(df$Score_Pondere, df$Happiness)
cor(df$Score_ACP, df$Happiness)
#Nous avons également décidé de faire une régression linéaire pour voir le R2 ajusté
summary(lm(Happiness ~ Score_Pondere, data = df))
summary(lm(Happiness ~ Score_ACP, data = df))

#Dans les deux test nous trouvons que la méthode pondérée donnent des résultats légèrement meilleurs 
#R2 pondéré= 0.8892 > 0.8748 pour l'ACP 
#Corrélation: pondérée=0.9439>0.9364 pour l'ACP
#Avec une pertinence théorique et empirique, la méthode pondérée sera adopté finalement (Plus de détails dans le rapport) 


###################--------- Visualisations 

# Graphe pour visualiser les scores pondérés vs Happiness, montrant le lien entre le score pondéré et la variable happiness
ggplot(df, aes(x = Score_Pondere, y = Happiness)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_vline(xintercept = score_parfait_pondere, linetype = "dashed", color = "red") +
  labs(title = "Lien entre score de perfection pondéré et bonheur perçu",
       x = "Score de perfection (pondéré)",
       y = "Score de bonheur (Happiness)") +
  theme_minimal()

#Graphique pour montrer la relation entre la performance synthétique (score de perfection) et Happiness
ggplot(df, aes(x = Score_Pondere, y = Happiness, label = Country)) +
  geom_point(color = "blue") +
  geom_text(size = 3, vjust = -1, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Lien entre performance synthétique et bonheur perçu",
       x = "Score de perfection (pondéré)",
       y = "Happiness") +
  theme_minimal()

#Graphique pour comparer entre les deux méthodes 
ggplot(df, aes(x = Score_Pondere, y = Score_ACP)) +
  geom_point() +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(title = "Comparaison des deux méthodes de scoring",
       x = "Score pondéré",
       y = "Score ACP")
