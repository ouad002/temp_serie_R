#partie 1:
# Charger la bibliothèque nécessaire
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)
#1-importation des données depuis le fichier "data.csv"
# Lire les données avec les noms de colonnes spécifiques du fichier
data <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE)

# Renommer les colonnes pour mieux manipuler
colnames(data) <- c("Date", "Temp_F")
#2-conversion de la série en degré celsius et en série mensuelle
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
#4- création d'un objet ts (R)
# Créer la série temporelle mensuelle ts
temps_ts <- ts(data_monthly$Temp_C, start = c(data_monthly$Year[1], data_monthly$Month[1]), frequency = 12)
# Tracer la série
plot(temps_ts, main = "Températures mensuelles moyennes (°C) - Lyon", ylab = "Température (°C)", xlab = "Année")
grid()
#5-
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
#6-
#Décomposition STL (trend, saisonnalité, résidu)
decomp_stl <- stl(temps_ts, s.window = "periodic")
decomp_add <- decompose(temps_ts, type = "additive")
plot(decomp_stl, main = "Décomposition STL des températures mensuelles - Lyon")
plot(decomp_add)
#7-
# Extraire la série sans saisonnalité (= trend + remainder)
des_data <- decomp_stl$time.series[, "trend"] + decomp_stl$time.series[, "remainder"]

# Convertir en objet ts avec mêmes paramètres que temps_ts
des_data <- ts(data, start = start(temps_ts), frequency = frequency(temps_ts))

# Tracer la série sans saisonnalité
plot(des_data, main = "Série des températures sans saisonnalité", ylab = "Température (°C)", xlab = "Année")
grid()
## 8. Tester la stationnarité avec ADF et KPSS
install.packages("tseries")
library(tseries)

adf_result <- adf.test(des_data)  # Test Augmented Dickey-Fuller
adf_result

kpss_result <- kpss.test(des_data, null = "Level")  # Test KPSS
kpss_result

# Différencier la série pour stabiliser la moyenne
des_data_diff <- diff(des_data)
plot(des_data_diff, type="l", main="Série différenciée")

# Retester stationnarité sur la série différenciée
adf.test(des_data_diff)
adf_result
kpss.test(des_data_diff)
kpss_result
#partie 2 :
#1-
# Tracer la fonction d'autocorrélation (ACF)
acf(temps_ts, main = "Fonction d'autocorrélation (ACF) des températures mensuelles")

#) Tracer la fonction d'autocorrélation partielle (PACF)
pacf(temps_ts, main = "Fonction d'autocorrélation partielle (PACF) des températures mensuelles")

