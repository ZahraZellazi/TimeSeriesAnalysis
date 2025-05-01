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

# Tracer la sous-série zoom
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




# Test de la stationnarité avec Augmented Dickey-Fuller Test (ADF)

adf.test(production_electronique_ts)  # Test de Dickey-

# Appliquer une différenciation d'ordre 1
production_electronique_diff <- diff(production_electronique_ts)

# Afficher la série différenciée
plot(production_electronique_diff, main = "Série différenciée d'ordre 1")
adf.test(production_electronique_diff)
