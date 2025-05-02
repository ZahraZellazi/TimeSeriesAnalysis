# Projet Serie Temporelle 

# Étape 1 : Packages nécessaires
packages <- c(
  "tidyverse", "lubridate", "forecast", "tseries",
  "urca", "tsibble", "feasts", "readxl", "patchwork",
  "fable", "knitr", "xts", "moments", "zoo"
)

# Étape 2 : Installer/charger les packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}


# DataSet 2 : construction spending S C3

# Étape 3 : Lire le fichier Excel
data <- read_excel(file.choose(), skip = 7)

# Étape 4 : Nettoyage et renommage des colonnes
# Pour dates Excel stockées en format numérique
if (is.numeric(data$Period)) {
  data <- data %>%
    rename(Period = 1, Value = 2) %>%
    mutate(Period = as.Date(Period, origin = "1899-12-30"))
} else {
  data <- data %>%
    rename(Period = 1, Value = 2) %>%
    mutate(Period = lubridate::parse_date_time(Period, orders = c("ymd", "my", "bY", "mY")))
}


# Étape 5 : Interpolation des valeurs manquantes
data$Value <- zoo::na.approx(data$Value, na.rm = FALSE)

# Supprimer les NA restants si présents
data <- data %>% drop_na(Value)

# Étape 6 : Créer une tsibble
data_tsibble <- data%>%
  mutate(Period = yearmonth(Period)) %>%
  as_tsibble(index = Period)

# Étape 7 : Visualisation
ggplot(data_tsibble, aes(x = Period, y = Value)) +
  geom_line(color = "darkblue") +
  labs(title = "Dépenses mensuelles de construction aux États-Unis",
       x = "Date", y = "Dépenses (Millions de $)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Étape 8 : Création d’un objet ts
construction_ts <- ts(data$Value, start = c(2002, 1), frequency = 12)
plot(construction_ts,
     main = "Série temporelle des dépenses de construction (ts)",
     ylab = "Dépenses (Millions de $)", xlab = "Année", col = "darkblue")

# Étape 9 : Zoom sur 2015–2020
construction_zoom <- window(construction_ts, start = c(2015, 1), end = c(2020, 12))
plot(construction_zoom, col = "darkred", lwd = 2,
     main = "Zoom : Dépenses de construction (2015–2020)",
     ylab = "Dépenses (Millions de $)", xlab = "Années",
     xaxt = "n")
axis(1, at = seq(2015, 2020, by = 1), labels = seq(2015, 2020, by = 1))

# Étape 10 : Statistiques descriptives
mean_value <- mean(construction_ts)
var_value <- var(construction_ts)
std_value <- sd(construction_ts)
cat("Moyenne:", mean_value, "\n")
cat("Variance:", var_value, "\n")
cat("Écart-type:", std_value, "\n")
head(construction_ts)

tail(construction_ts)

str(construction_ts)

summary(construction_ts)

class(construction_ts)
# Étape 11 : Décomposition multiplicative
decomp_multiplicative <- decompose(construction_ts, type = "multiplicative")
plot(decomp_multiplicative)

# Étape 12 : Autocorrélation
lag.plot(construction_ts, lags = 36)

# Calcul de l'autocorrélation (ACF) 
acf(construction_ts)

# Ajustement par moyenne mobile (k=12 pour saisonnalité mensuelle)
mm <- stats::filter(construction_ts, rep(1/12, 12), sides = 2)
plot(construction_ts, col = "gray", main = "Ajustement par moyenne mobile")
lines(mm, col = "blue", lwd = 2)
legend("topleft", legend = c("Série Originale", "Moyenne Mobile"), col = c("gray", "blue"), lty = 1)
summary(mm)



# Ajustement de la composante saisonnaire 
t <- 1:length(construction_ts)  # Création du vecteur temps

# Initialisation des matrices pour les termes cosinus et sinus
Mc <- matrix(0, nrow = length(construction_ts), ncol = 6)  # 6 termes cosinus
Ms <- matrix(0, nrow = length(construction_ts), ncol = 6)  # 6 termes sinus

# Remplissage des matrices avec les termes harmoniques
for (i in 1:6) {
  Mc[, i] <- cos(2 * pi * t * i / 12)  # Termes cosinus
  Ms[, i] <- sin(2 * pi * t * i / 12)  # Termes sinus
}

# Construction du modèle de régression avec les termes harmoniques
harm_model <- lm(construction_ts ~ Mc + Ms)

# Résumé du modèle
summary(harm_model)


# Test de stationnarité
adf_result <- adf.test(construction_ts)
cat("Résultat du test ADF: \n")
print(adf_result)


# Si la série n'est pas stationnaire, appliquer une différenciation d'ordre 1
construction_diff <- diff(construction_ts)
plot(construction_diff, main = "Série différenciée d'ordre 1")


acf(construction_diff, main = "ACF de la série différenciée")
pacf(construction_diff, main = "PACF de la série différenciée")

# Modèle ARIMA automatique
modele_auto <- auto.arima(construction_ts)
summary(modele_auto)

# Test de Ljung-Box sur les résidus du modèle
residus <- residuals(modele_auto)
Box.test(residus, lag = 20, type = "Ljung-Box")
checkresiduals(modele_auto)

# Prévision des 12 prochains mois
prevision <- forecast(modele_auto, h = 12 )
plot(prevision, main = "Prévision des dépenses de construction")

prevision
xsx