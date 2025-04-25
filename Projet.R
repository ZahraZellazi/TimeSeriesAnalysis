# Liste des packages nécessaires
packages <- c(
  "tidyverse", "lubridate", "forecast", "tseries",
  "urca", "tsibble", "feasts", "readxl", "patchwork",
  "fable", "knitr"
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
# Charger le fichier avec skip pour ignorer les métadonnées
production_electronique <- read_excel(file.choose(), skip = 10)
production_electronique$observation_date <- as.Date(production_electronique$observation_date)
head(production_electronique)
tail(production_electronique)
str(production_electronique)
summary(production_electronique)
class(production_electronique)
# Créer une série temporelle avec fréquence mensuelle
production_electronique_ts <- ts(production_electronique$IPB53122N,
                                 start = c(2009, 1),  # Début en janvier 2009
                                 frequency = 12)      # Fréquence mensuelle
# Afficher les premières lignes de la série multivariée
head(production_electronique_ts)


# Vérifiez la série temporelle
plot(production_electronique_ts)
class(production_electronique_ts)
#Visualisation 
plot(production_electronique_ts,
     main = "Production industrielle électronique (2009-2024)",
     ylab = "Index IPB53122N", xlab = "Année",
     col = "blue", lwd = 2)
# Sélectionner une sous-série de 2010 à 2012
zoom_production <- window(production_electronique_ts, start=c(2010, 1), end=c(2013, 12))
# Tracer la sous-série
plot(zoom_production,  ylab="Production", xlab="Année")
# Résumé statistique
summary(production_electronique_ts)

# Moyenne
mean(production_electronique_ts)

# Variance
var(production_electronique_ts)

# Écart-type
sd(production_electronique_ts)

# Décomposition de la série temporelle
decomp <- decompose(production_electronique_ts)

# Afficher les composants de la décomposition
decomp

# Afficher la tendance
decomp$trend
# Afficher la saisonnalité
decomp$seasonal

decomp$random

plot(decomp)

lag.plot(production_electronique_ts , lags=36)
