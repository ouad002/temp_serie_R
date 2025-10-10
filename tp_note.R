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
adf_result_diff <- adf.test(des_data_diff, alternative = "stationary")
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

##2- détermination des ordres optimaux des modèles AR ou MA selon BIC/AIC
if (!requireNamespace("forecast", quietly = TRUE)) install.packages("forecast")
library(forecast)
fit_auto <- forecast::auto.arima(des_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
fit_auto
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
##3-
# Supposons que p_opt est l'ordre optimal AR trouvé (ex: p_opt <- best_ar_bic$p)
p_opt <- best_ar_bic$p
p_opt
# Ajuster le modèle AR
fit_ar <- arima(des_data_diff, order = c(p_opt, 0, 0))

# on choisit le modèle ARMA(1,1,2) pour notre cas
fit_arma <- arima(des_data_diff, order = c(1, 1, 2))

# Analyse des résidus du modèle AR
res_ar <- residuals(fit_ar)
acf(res_ar, main = paste("ACF des résidus du modèle AR(", p_opt, ")"))
Box.test(res_ar, lag = 12, type = "Ljung-Box")

# Analyse des résidus du modèle ARMA
res_arma <- residuals(fit_arma)
acf(res_arma, main = "ACF des résidus du modèle ARMA(1,1,2)")
Box.test(res_arma, lag = 12, type = "Ljung-Box")
#partie 3 :
##1-
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

#2- estimation du modèle ARCH(1), GARCH(1,1)
if (!requireNamespace("rugarch", quietly = TRUE)) install.packages("rugarch")
library(rugarch)

spec_arch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)), # ARCH(1) : p=1, q=0
  mean.model     = list(armaOrder = c(0,0)),                   # moyenne : pas d'ARMA
  distribution.model = "norm"
)
fit_arch <- ugarchfit(spec = spec_arch, data = res_arma)
show(fit_arch)
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), # GARCH(1,1): p=1, q=1
  mean.model     = list(armaOrder = c(0,0)),                    # moyenne : pas d'ARMA
  distribution.model = "norm"
)
fit_garch <- ugarchfit(spec = spec_garch, data = res_arma)
show(fit_garch)
# 3- Comparaison robuste AIC/BIC
ic_arch  <- infocriteria(fit_arch)
ic_garch <- infocriteria(fit_garch)
print(ic_arch)
print(names(ic_arch))
print(ic_garch)
print(names(ic_garch))

# Ordre dans infocriteria: Akaike, Bayes, Shibata, Hannan-Quinn
aic_arch  <- as.numeric(ic_arch[1])
bic_arch  <- as.numeric(ic_arch[2])
aic_garch <- as.numeric(ic_garch[1])
bic_garch <- as.numeric(ic_garch[2])


# Affichage tableau
comp <- data.frame(
  Model = c("ARCH(1)", "GARCH(1,1)"),
  AIC   = c(aic_arch, aic_garch),
  BIC   = c(bic_arch, bic_garch)
)
print(comp)

# Vérifications NA
if (any(is.na(comp$AIC)) || any(is.na(comp$BIC))) {
  cat("\nAttention: au moins une valeur AIC/BIC est NA. Vérifier l'estimation des modèles.\n")
} else {
  if (aic_arch < aic_garch && bic_arch < bic_garch) {
    cat("\nConclusion: ARCH(1) préféré selon AIC et BIC.\n")
  } else if (aic_garch < aic_arch && bic_garch < bic_arch) {
    cat("\nConclusion: GARCH(1,1) préféré selon AIC et BIC.\n")
  } else {
    cat("\nConclusion: critères divergents (AIC vs BIC). Choisir le plus parcimonieux (BIC) et vérifier les diagnostics des résidus.\n")
  }
}
#4-
# Choisir les résidus du modèle final (ex: ARCH(1) via rugarch)
res <- residuals(fit_arch, standardize = TRUE)  # résidus standardisés rugarch
res <- as.numeric(res)

# 1) Absence d'autocorrélation (bruit blanc)
par(mfrow = c(1,2))
acf(res, main = "ACF résidus standardisés")
acf(res^2, main = "ACF résidus^2 (volatilité)")
par(mfrow = c(1,1))
lb1  <- Box.test(res, lag = 12, type = "Ljung-Box")
lb2  <- Box.test(res^2, lag = 12, type = "Ljung-Box")
print(lb1)
print(lb2)

# 2) Normalité (QQ-plot + tests)
par(mfrow = c(1,2))
hist(res, breaks = "FD", main = "Histogramme des résidus", xlab = "résidus")
qqnorm(res); qqline(res, col = 2, lwd = 2)
par(mfrow = c(1,1))
if (!requireNamespace("nortest", quietly = TRUE)) install.packages("nortest")
library(nortest)
print(ad.test(res))     # Anderson–Darling
print(shapiro.test(res[sample.int(length(res), min(5000, length(res))) ]))  # Shapiro (échantillon)

# 3) Hétéroscédasticité résiduelle (ARCH restant)
if (!requireNamespace("FinTS", quietly = TRUE)) install.packages("FinTS")
library(FinTS)
print(ArchTest(res, lags = 12))

# 4) Indépendance série/variance (tests additionnels rugarch si dispo)
if ("rugarch" %in% .packages()) {
  show(fit_arch)  # déjà contient les Weighted Ljung-Box et Sign Bias, etc.
}

# 5) Récapitulatif décisionnel
pvals <- c(LB_res = lb1$p.value, LB_res2 = lb2$p.value)
cat("\nSeuil 5%: p-values Ljung-Box résidus =", round(pvals["LB_res"],4),
    ", résidus^2 =", round(pvals["LB_res2"],4), "\n")


