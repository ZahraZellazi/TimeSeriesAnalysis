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
str(production_electronique)
summary(production_electronique)
class(production_electronique)
