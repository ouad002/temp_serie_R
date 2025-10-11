# Majeure Science des Données 2025-2026
#TP noté séries temporelles
#groupe : 
#Karima AMIRI
#Ouadie BOUSSETTA
#Abdelaziz MOHAMAD

###partie1 :Importation des données et analyse préliminaire

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



##8. Tester la stationnarité avec ADF et KPSS 
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
adf_result_diff <- adf.test(des_data_diff, alternative = "stationary")
kpss_result_diff <- kpss.test(des_data_diff, null = "Level")

cat("\n---- Série différenciée ----\n")
adf_result_diff
kpss_result_diff

###partie 2: Modélisation et analyse

##1-Tracer les fonctions ACF et PACF de la série stationnaire
par(mfrow = c(1,2))  # pour afficher côte à côte
#On travaille sur la série désaisonnalisée et différenciée
acf(des_data_diff, main = "ACF")
pacf(des_data_diff, main = "PACF")

par(mfrow = c(1,1))  # retour à un seul graphique

##2- détermination des ordres optimaux des modèles AR ou MA selon BIC/AIC

# Définir une plage raisonnable d’ordres à tester
max_p <- 6
max_q <- 6

# Grille AR(p)
ar_results <- lapply(1:max_p, function(p) {
  fit <- arima(des_data_diff, order = c(p, 0, 0))
  data.frame(Model = paste0("AR(", p, ")"), p = p, q = 0, AIC = AIC(fit), BIC = BIC(fit))
})
ar_results <- do.call(rbind, ar_results)

# Grille MA(q)
ma_results <- lapply(1:max_q, function(q) {
  fit <- arima(des_data_diff, order = c(0, 0, q))
  data.frame(Model = paste0("MA(", q, ")"), p = 0, q = q, AIC = AIC(fit), BIC = BIC(fit))
})
ma_results <- do.call(rbind, ma_results)

# Trouver les meilleurs selon AIC et BIC
best_ar_aic  <- ar_results[which.min(ar_results$AIC), ]
best_ar_bic  <- ar_results[which.min(ar_results$BIC), ]
best_ma_aic  <- ma_results[which.min(ma_results$AIC), ]
best_ma_bic  <- ma_results[which.min(ma_results$BIC), ]

list(
  Best_AR_by_AIC = best_ar_aic,
  Best_AR_by_BIC = best_ar_bic,
  Best_MA_by_AIC = best_ma_aic,
  Best_MA_by_BIC = best_ma_bic
)

##3-Ajustement des modèles AR et ARMA:
#Pour le modèle AR
# Supposons que p_opt est l'ordre optimal AR trouvé (6)
p_opt <- best_ar_bic$p
p_opt
# Ajuster le modèle AR
fit_ar <- arima(des_data_diff, order = c(p_opt, 0, 0))
#Pour le modèle ARMA
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
library(forecast)
fit_auto <- forecast::auto.arima(des_data_diff, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
fit_auto
# on choisit le modèle ARMA(1,0,2) pour notre cas
fit_arma <- arima(des_data_diff, order = c(1, 0, 2))

#Analyse des résidus
# Analyse des résidus du modèle AR
res_ar <- residuals(fit_ar)
acf(res_ar, main = paste("ACF des résidus du modèle AR(", p_opt, ")"))
Box.test(res_ar, lag = 12, type = "Ljung-Box")

# Analyse des résidus du modèle ARMA
res_arma <- residuals(fit_arma)
acf(res_arma, main = "ACF des résidus du modèle ARMA(1,0,2)")
Box.test(res_arma, lag = 12, type = "Ljung-Box")


###partie3 :Modèles ARCH et GARCH

##1-Appliquer le test ARCH d’Engle
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("FinTS", quietly = TRUE)) install.packages("FinTS")
library(FinTS)
library(lmtest)
# Pour le modèle AR :
arch_test_ar <- ArchTest(res_ar, lags = 12)
print(arch_test_ar)

# Pour le modèle ARMA :
arch_test_arma <- ArchTest(res_arma, lags = 12)
print(arch_test_arma)

##2- Estimation du modèle ARCH(1), GARCH(1,1)
if (!requireNamespace("rugarch", quietly = TRUE)) install.packages("rugarch")
library(rugarch)
#Le modèle ARCH(1)
spec_arch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)), # ARCH(1) : p=1, q=0
  mean.model     = list(armaOrder = c(0,0)),                   # moyenne : pas d'ARMA
  distribution.model = "norm"
)
fit_arch <- ugarchfit(spec = spec_arch, data = res_arma)
show(fit_arch)

#Le modèle GARCH(1,1)
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), # GARCH(1,1): p=1, q=1
  mean.model     = list(armaOrder = c(0,0)),                    # moyenne : pas d'ARMA
  distribution.model = "norm"
)
fit_garch <- ugarchfit(spec = spec_garch, data = res_arma)
show(fit_garch)

# Pour ARCH(1)
# Coefficients et p-values
coef_arch <- coef(fit_arch)
pvalues_arch <- fit_arch@fit$matcoef[,"Pr(>|t|)"]
print(data.frame(Coefficient = coef_arch, P_value = pvalues_arch))

# Pour GARCH(1,1)
# Coefficients et p-values
coef_garch <- coef(fit_garch)
pvalues_garch <- fit_garch@fit$matcoef[,"Pr(>|t|)"]
print(data.frame(Coefficient = coef_garch, P_value = pvalues_garch))

##3- Comparaison robuste AIC/BIC
# Extraction des critères d'information
ic_arch  <- infocriteria(fit_arch)
ic_garch <- infocriteria(fit_garch)
# Création du tableau de comparaison
comp <- data.frame(
  Model = c("ARCH(1)", "GARCH(1,1)"),
  AIC   = c(ic_arch[1], ic_garch[1]),
  BIC   = c(ic_arch[2], ic_garch[2])
)
print(comp)

##4-Vérification de la qualité des résidus finales 
# Choisir les résidus du modèle final (ARCH(1))
res <- residuals(fit_arch, standardize = TRUE)  # résidus standardisés rugarch
res <- as.numeric(res)

#Absence d'autocorrélation (bruit blanc)
par(mfrow = c(1,2))
acf(res, main = "ACF résidus standardisés")
acf(res^2, main = "ACF résidus^2 (volatilité)")
par(mfrow = c(1,1))
lb1  <- Box.test(res, lag = 12, type = "Ljung-Box")
lb2  <- Box.test(res^2, lag = 12, type = "Ljung-Box")
print(lb1)
print(lb2)

#Normalité (QQ-plot + tests)
par(mfrow = c(1,2))
hist(res, breaks = "FD", main = "Histogramme des résidus", xlab = "résidus")
qqnorm(res); qqline(res, col = 2, lwd = 2)
par(mfrow = c(1,1))
if (!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")
library(nortest)
print(ad.test(res))     # Anderson–Darling
print(shapiro.test(res[sample.int(length(res), min(5000, length(res))) ]))  # Shapiro (échantillon)

#Hétéroscédasticité résiduelle (ARCH restant)
if (!requireNamespace("FinTS", quietly = TRUE)) install.packages("FinTS")
library(FinTS)
print(ArchTest(res, lags = 12))


###Partie 4: Prévision avec ARMA(1,2)-ARCH(1)

## 1. Prévision pour une année (12 observations)
#Modèle ARMA(1,2)-ARCH(1) complet
spec_arma_arch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)), # ARCH(1)
  mean.model     = list(armaOrder = c(1,2)),                   # ARMA(1,2) pour la moyenne
  distribution.model = "norm"
)

#Ajustement sur la série désaisonnalisée et différenciée
fit_arma_arch <- ugarchfit(spec = spec_arma_arch, data = des_data_diff)
show(fit_arma_arch)

#Prévisions à 12 pas
fcast_complete <- ugarchforecast(fit_arma_arch, n.ahead = 12)

#Extraire les prévisions
mu_pred_complete    <- fitted(fcast_complete)    # moyenne ARMA prédite
sigma_pred_complete <- sigma(fcast_complete)     # volatilité ARCH prédite
upper95_complete    <- mu_pred_complete + 1.96 * sigma_pred_complete
lower95_complete    <- mu_pred_complete - 1.96 * sigma_pred_complete

#Créer les séries ts pour l'alignement temporel
last_time   <- time(des_data_diff)[length(des_data_diff)]
start_year  <- floor(last_time)
start_month <- round((last_time - start_year) * 12) + 1

#Si start_month > 12, ajuster
if (start_month > 12) {
  start_year  <- start_year + 1
  start_month <- start_month - 12
}

#Convertir en objets ts
preds_complete <- ts(mu_pred_complete, start = c(start_year, start_month), frequency = 12)
upper_complete <- ts(upper95_complete, start = c(start_year, start_month), frequency = 12)
lower_complete <- ts(lower95_complete, start = c(start_year, start_month), frequency = 12)

#Affichage du tableau récapitulatif
library(zoo)
months <- as.yearmon(time(preds_complete))
forecast_table <- data.frame(
  Month         = format(months, "%b %Y"),
  Mean_Pred     = round(as.numeric(preds_complete), 4),
  Lower_95_CI   = round(as.numeric(lower_complete), 4),
  Upper_95_CI   = round(as.numeric(upper_complete), 4)
)
print(forecast_table)


## 2. Tracé avec intervalles de confiance à 95%
plot(des_data_diff, type = "l", col = "black", lwd = 1.5,
     ylab = "Température différenciée (°C)", xlab = "Temps",
     main = "Prévision ARMA(1,2)-ARCH(1) avec IC 95%")

lines(preds_complete, col = "blue", lwd = 2)
lines(upper_complete, col = "red", lty = 2)
lines(lower_complete, col = "red", lty = 2)

legend("topleft", legend = c("Historique", "Prévision", "IC 95%"),
       col = c("black", "blue", "red"), lty = c(1,1,2), lwd = c(1.5,2,1))

##3.Évaluation des performances prédictives (MSE, MAPE)
# Pour cela, on utilise les 12 dernières observations comme "vraies valeurs"
n_obs <- length(des_data_diff)
actual_values <- des_data_diff[(n_obs-11):n_obs]  # 12 dernières observations

# Refaire une prévision sur les données tronquées (sans les 12 derniers points)
data_truncated <- des_data_diff[1:(n_obs-12)]

# Réajuster le modèle sur les données tronquées
fit_truncated <- ugarchfit(spec = spec_arma_arch, data = data_truncated)
fcast_truncated <- ugarchforecast(fit_truncated, n.ahead = 12)
predicted_values <- fitted(fcast_truncated)

# Calcul des erreurs
errors <- actual_values - predicted_values

# Métriques de performance
MSE  <- mean(errors^2, na.rm = TRUE)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(errors), na.rm = TRUE)
MAPE <- mean(abs(errors / actual_values), na.rm = TRUE) * 100

# Affichage des résultats
cat("\n=== ÉVALUATION DES PERFORMANCES PRÉDICTIVES ===\n")
cat("MSE  (Mean Squared Error)     :", round(MSE, 4), "\n")
cat("RMSE (Root Mean Squared Error):", round(RMSE, 4), "\n")
cat("MAE  (Mean Absolute Error)    :", round(MAE, 4), "\n")
cat("MAPE (Mean Absolute Perc. Err):", round(MAPE, 2), "%\n")

# Graphique de comparaison prévisions vs réalisations
plot(1:12, actual_values, type = "o", col = "black", pch = 19, lwd = 2,
     ylab = "Valeurs", xlab = "Mois", 
     main = "Comparaison Prévisions vs Réalisations")
lines(1:12, predicted_values, type = "o", col = "blue", pch = 17, lwd = 2)
legend("topright", legend = c("Valeurs réelles", "Prévisions"),
       col = c("black", "blue"), pch = c(19, 17), lty = 1)
grid()

##4.Comparaison : prévision ARMA(1,2) sans ARCH

library(forecast)

#Ajustement d’un modèle ARMA(1,2) sur la série désaisonnalisée différenciée
fit_arma_only <- Arima(des_data_diff, order = c(1, 0, 2), include.constant = TRUE)
summary(fit_arma_only)

#Prévision à 12 pas
fc_arma_only <- forecast(fit_arma_only, h = 12)

#Extraction des prévisions et intervalles 95 %
preds_arma    <- fc_arma_only$mean
lower_arma    <- fc_arma_only$lower[,2]  # borne basse 95%
upper_arma    <- fc_arma_only$upper[,2]  # borne haute 95%

#Construction des ts pour alignement temporel
last_time     <- time(des_data_diff)[length(des_data_diff)]
start_year    <- floor(last_time)
start_month   <- round((last_time - start_year) * 12) + 1
if (start_month > 12) {
  start_year  <- start_year + 1
  start_month <- start_month - 12
}
preds_arma_ts <- ts(preds_arma, start = c(start_year, start_month), frequency = 12)
lower_arma_ts <- ts(lower_arma, start = c(start_year, start_month), frequency = 12)
upper_arma_ts <- ts(upper_arma, start = c(start_year, start_month), frequency = 12)

#Tracé comparatif
plot(des_data_diff, type = "l", col = "black", lwd = 1.5,
     ylab = "Température différenciée (°C)",
     xlab = "Temps",
     main = "Prévision ARMA(1,2) vs ARMA(1,2)-ARCH(1)")
lines(preds_arma_ts, col = "darkgreen", lwd = 2)
lines(lower_arma_ts, col = "darkgreen", lty = 2)
lines(upper_arma_ts, col = "darkgreen", lty = 2)
lines(preds_complete, col = "blue", lwd = 2)       # ARMA+ARCH
lines(lower_complete, col = "red",    lty = 2)     # IC ARCH
lines(upper_complete, col = "red",    lty = 2)
legend("bottomleft",
       inset = c(0.002, 0.005),
       legend = c("Historique", "ARMA(1,2) prévision", "IC ARMA", 
                  "ARMA(1,2)-ARCH(1) prévision", "IC ARCH"),
       col    = c("black",       "darkgreen",       "darkgreen",
                  "blue",         "red"),
       lty    = c(1,             1,                 2,
                  1,             2),
       lwd    = c(1.5,           2,                 1,
                  2,             1))

#Évaluation hors-échantillon
n_obs         <- length(des_data_diff)
actual_values <- des_data_diff[(n_obs-11):n_obs]
# ARMA seul
fc_trunc_arma <- forecast(Arima(des_data_diff[1:(n_obs-12)], order = c(1,0,2)), h = 12)
preds_arma_ho <- fc_trunc_arma$mean

# Erreurs et métriques
errors_arma    <- actual_values - preds_arma_ho
MSE_arma       <- mean(errors_arma^2)
MAPE_arma      <- mean(abs(errors_arma / actual_values)) * 100

cat("=== Performance ARMA(1,2) seul ===\n")
cat("MSE :", round(MSE_arma, 4), "\n")
cat("MAPE:", round(MAPE_arma, 2), "%\n")



