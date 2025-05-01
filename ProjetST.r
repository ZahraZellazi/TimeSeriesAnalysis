# Projet Serie Temporelle 


# Liste des packages nécessaires
packages <- c(
  "tidyverse", "lubridate", "forecast", "tseries",
  "urca", "tsibble", "feasts", "readxl", "patchwork",
  "fable", "knitr","xts" 
)

# Boucle : installe si nécessaire, puis charge
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}


# DataSet 1 : Industrial Production electronic S C1

# Phase 1 : 

# Charger le fichier avec skip pour ignorer les métadonnées
production_electronique <- read_excel(file.choose(), skip = 10)


production_electronique$observation_date <- as.Date(production_electronique$observation_date)

head(production_electronique)

tail(production_electronique)

str(production_electronique)

summary(production_electronique)

class(production_electronique)

#serie_ts
# Créer une série temporelle avec fréquence mensuelle
production_electronique_ts <- ts(production_electronique$IPB53122N,
                                 start = c(2009, 1),  # Début en janvier 2009
                                 frequency = 12)      # Fréquence mensuelle


# Afficher les premières lignes de la série multivariée
head(production_electronique_ts)


# Vérifiez la série temporelle
class(production_electronique_ts)


#Visualisation 
plot(production_electronique_ts,
     main = "Production industrielle électronique (2009-2024)",
     ylab = "Index IPB53122N", xlab = "Année",
     col = "blue", lwd = 2)


# serie_xts
Date<-seq(as.Date("2009/01/01"), as.Date("2024/03/01"),by="months")

production_electronique_xts <- xts(production_electronique$IPB53122N, order.by = Date)

class(production_electronique_xts)

plot(production_electronique_xts,
     main = "Production industrielle électronique (2009-2024)",
     ylab = "Index IPB53122N", xlab = "Année",
     col = "pink", lwd = 2)

# Sélectionner une sous-série de 2010 à 2012
zoom_production <- window(production_electronique_ts, start=c(2010, 1), end=c(2013, 12))

# Tracer la sous-série
plot(zoom_production,  ylab="Production", xlab="Année",  col = "green")

# Interpretation: 

# 📈 Tendance générale croissante : depuis janvier 2009 jusqu’à mars 2024, la production industrielle dans le secteur électronique montre une croissance soutenue, avec quelques fluctuations temporaires.

# 🔁 Périodicité apparente : on remarque la présence de motifs récurrents d’une année à l’autre, ce qui suggère une saisonnalité.
# Cette saisonnalité semble relativement stable en amplitude, ce qui justifierait une modélisation mutiplicative, à vérifier par la décomposition.

# Résumé statistique
summary(production_electronique_ts)

# Moyenne
mean(production_electronique_ts)

# Variance
var(production_electronique_ts)

# Écart-type
sd(production_electronique_ts)

# Décomposition de la série temporelle
decomp <- decompose(production_electronique_ts, type = "multiplicative")

# Afficher les composants de la décomposition
decomp

# Afficher la tendance
decomp$trend
# Afficher la saisonnalité
decomp$seasonal

decomp$random

plot(decomp)

# Interpretation : 
# ✖️ Modèle : Décomposition multiplicative car l'amplitude des variations est  proportionnelle à la tendance 
# 📈 La tendance : indique une forte croissance au début, suivie d’un ralentissement récent.
# 🎢 Saisonnalité : Stable et régulière
# 🎲 Les résidus sont modérés, représentant des événements aléatoires.

# Autocorrelation : 
lag.plot(production_electronique_ts , lags=36)

# Interpretation : 
# 📍Les points suivent une forme proche d'une droite 
# ➡️ Il existe une forte corrélation entre les valeurs successives de la série temporelle.

# Calcul de l'autocorrélation (ACF) 
acf(production_electronique_ts)
acf(production_electronique_ts, plot=F )

# Interpretation : 
# 🔻 La série production_electronique_ts montre une autocorrélation qui diminue 
# progressivement à mesure que l'on s'éloigne dans le temps. Cela suggère qu'il pourrait y avoir 
# une forte influence des valeurs récentes sur les valeurs passées,
# mais cette influence se dissipe avec le temps.

# 📉 Une décroissance lente de l'autocorrélation à des lags plus élevés 

# Ajustement :

# Lissage par moyenne mobile (k=12 pour saisonnalité mensuelle)
mm <- filter(production_electronique_ts, filter = rep(1/12, 12), sides = 2)

plot(production_electronique_ts, col = "gray", main = "Ajustement par moyenne mobile")
lines(mm, col = "blue", lwd = 2)
legend("topleft", legend = c("Série Originale", "Moyenne Mobile"), col = c("gray", "blue"), lty = 1)
summary(mm)

# Lissage lm 
temps <- time(production_electronique_ts)
modele_lm <- lm(production_electronique_ts ~ temps)
summary(modele_lm)

plot(production_electronique_ts, main="Ajustement linéaire", col="gray")
abline(modele_lm, col="red", lwd=2)

# Lissage tendance Polynomiale 

# Création du modèle polynomial (degré 2)
temps <- time(production_electronique_ts)
modele_poly <- lm(production_electronique_ts ~ poly(temps, 2, raw = TRUE))

# Résumé du modèle
summary(modele_poly)

# Créer un data frame propre avec les valeurs ajustées
df <- data.frame(
  temps = as.numeric(temps),
  production = as.numeric(production_electronique_ts),
  ajuste = fitted(modele_poly)
)

# Tracer la série originale
plot(df$temps, df$production, type = "l", col = "gray",
     main = "Ajustement polynomial (degré 2)", xlab = "Temps", ylab = "Production")

# Ajouter la courbe ajustée
lines(df$temps, df$ajuste, col = "blue", lwd = 2)

# Légende
legend("topleft", legend = c("Série", "Ajustement polynôme d'ordre 2"), 
       col = c("gray", "blue"), lty = 1)



# Ajustement de la composante saisonnaire 
# Version corrigée et commentée
t <- 1:length(production_electronique_ts)  # Création du vecteur temps

# Initialisation des matrices pour les termes cosinus et sinus
Mc <- matrix(0, nrow = length(production_electronique_ts), ncol = 6)  # 6 termes cosinus
Ms <- matrix(0, nrow = length(production_electronique_ts), ncol = 6)  # 6 termes sinus

# Remplissage des matrices avec les termes harmoniques
for (i in 1:6) {
  Mc[, i] <- cos(2 * pi * t * i / 12)  # Termes cosinus
  Ms[, i] <- sin(2 * pi * t * i / 12)  # Termes sinus
}

# Construction du modèle de régression avec les termes harmoniques
harm_model <- lm(production_electronique_ts ~ Mc + Ms)

# Résumé du modèle
summary(harm_model)



# stationnarité : 


# Test de la stationnarité avec Augmented Dickey-Fuller Test (ADF)

adf.test(production_electronique_ts)  # Test de Dickey-Fuller (ADF)
# Hypothèse nulle (H₀) : la série n’est pas stationnaire (elle a une racine unitaire).

# Hypothèse alternative (H₁) : la série est stationnaire.

# p-value = 0.4313 > 0.05, on accepte H₀ → La série n’est pas stationnaire.

# Appliquer une différenciation d'ordre 1
production_electronique_diff <- diff(production_electronique_ts)

# Afficher la série différenciée
plot(production_electronique_diff, main = "Série différenciée d'ordre 1")
acf(production_electronique_diff, main = "ACF de la série différenciée")


pacf(production_electronique_diff, main = "PACF de la série différenciée")

library(forecast)
modele_auto <- auto.arima(production_electronique_ts)
summary(modele_auto)


# Test de Ljung-Box sur les résidus du modèle
residus <- residuals(modele_auto)
stats::Box.test(residus, lag = 20, type = "Ljung-Box")

# Prévoir les 12 prochains mois
prevision <- forecast(modele_auto, h = 12)

# Afficher les résultats
plot(prevision, main = "Prévision de la production électronique")

prevision

