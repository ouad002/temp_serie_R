# Majeure Science des Données 2025-2026

###partie 1:Importation des données et analyse préliminaire

# Charger la bibliothèque nécessaire
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

##1-importation des données depuis le fichier "data.csv"
# Lire les données avec les noms de colonnes spécifiques du fichier
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)
# Renommer les colonnes pour mieux manipuler
colnames(data) <- c("Date", "Temp_F")
# Vérifier le nombre de valeurs manquantes 
sum(is.na(data$Temp_F))

##2-conversion de la série en degré celsius et en série mensuelle
# Convertir la colonne Date en format Date
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
# Convertir la température de Fahrenheit en Celsius
data$Temp_C <- (data$Temp_F - 32) * 5/9
# Afficher un aperçu des données après la conversion
head(data)

# Agréger par mois (moyenne des températures mensuelles)
data_monthly <- data %>%
  mutate(Year = as.integer(format(Date, "%Y")),
         Month = as.integer(format(Date, "%m"))) %>%
  group_by(Year, Month) %>%
  summarise(Temp_C = mean(Temp_C, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Year, Month)
head(data_monthly)

##3- création d'un objet ts (R)
# Créer la série temporelle mensuelle ts
temps_ts <- ts(data_monthly$Temp_C, start = c(data_monthly$Year[1], data_monthly$Month[1]), frequency = 12)

##4-Plot de la série
plot(temps_ts, main = "Températures mensuelles moyennes (°C) - Lyon", ylab = "Température (°C)", xlab = "Année")
grid()

##5-Statistique descriptive complète
# Installer et charger la bibliothèque e1071 pour skewness et kurtosis
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
library(e1071)

# Extraire la série en vecteur numérique
temps_vec <- as.numeric(temps_ts)

# Calcul des statistiques descriptives
mean_val <- mean(temps_vec, na.rm = TRUE)
median_val <- median(temps_vec, na.rm = TRUE)
variance_val <- var(temps_vec, na.rm = TRUE)
skewness_val <- skewness(temps_vec, na.rm = TRUE)
kurtosis_val <- kurtosis(temps_vec, na.rm = TRUE)

# Affichage des résultats
cat("Statistiques descriptives de la série mensuelle :\n")
cat("Moyenne :", mean_val, "\n")
cat("Médiane :", median_val, "\n")
cat("Variance :", variance_val, "\n")
cat("Asymétrie (skewness) :", skewness_val, "\n")
cat("Aplatissement (kurtosis) :", kurtosis_val, "\n")

##6-Analyse visuelle des tendances, saisonnalités et volatilités éventuelles.
#Décomposition STL (trend, saisonnalité, résidu)
decomp_stl <- stl(temps_ts, s.window = "periodic")
plot(decomp_stl, main = "Décomposition STL des températures mensuelles - Lyon")
#Décomposition additive
decomp_add <- decompose(temps_ts, type = "additive")
plot(decomp_add)

##7 - Suppression de la saisonnalité
# Extraire la série sans saisonnalité (= trend + remainder)
des_data <- decomp_stl$time.series[, "trend"] + decomp_stl$time.series[, "remainder"]

# Convertir en objet ts avec mêmes paramètres que temps_ts
des_data <- ts(des_data, start = start(temps_ts), frequency = frequency(temps_ts))

# Tracer la série sans saisonnalité
plot(des_data, main = "Série des températures sans saisonnalité", 
     ylab = "Température (°C)", xlab = "Année", col = "blue", lwd = 1)
grid()



## 8. Tester la stationnarité avec ADF et KPSS 
if (!requireNamespace("tseries", quietly = TRUE)) install.packages("tseries")
library(tseries)

# Test sur la série désaisonnalisée
adf_result <- adf.test(des_data)
kpss_result <- kpss.test(des_data, null = "Level")

cat("---- Série désaisonnalisée ----\n")
adf_result
kpss_result

# la série non stationnaire, on différencie
des_data_diff <- diff(des_data)

# Tracer la série différenciée
plot(des_data_diff, type = "l", main = "Série désaisonnalisée et différenciée",
     ylab = "Température (°C)", xlab = "Année", col = "darkred", lwd = 1)
grid()

# Retester stationnarité sur la série différenciée
adf_result_diff <- adf.test(des_data_diff)
kpss_result_diff <- kpss.test(des_data_diff, null = "Level")

cat("\n---- Série différenciée ----\n")
adf_result_diff
kpss_result_diff

###partie 2 :

##1-Tracer les fonctions ACF et PACF de la série stationnaire
par(mfrow = c(1,2))  # pour afficher côte à côte

acf(des_data_diff, main = "ACF de la série désaisonnalisée et différenciée")
pacf(des_data_diff, main = "PACF de la série désaisonnalisée et différenciée")

par(mfrow = c(1,1))  # retour à un seul graphique
