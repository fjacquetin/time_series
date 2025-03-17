rm(list=ls())

library(tidyverse) # dplyr, magrittr
library(insee) # API Insee
library(openxlsx) # saveworkbook
library(lubridate) # as.Date
library(urca) # ur.df
library(estimatr) # lm_robust
library(ggcorrplot) #ggcorrplot
library(scales) # scales_x_dateS
library(rwebstat) #rwebstat
library(zoo) # yearqtr
library(ggpubr) # ggarrange
library(vars) #VARselect
library(forecast)
library(fUnitRoots) # adfTest
library(strucchange)
library(portes)
library(gridExtra) # grid.arrange
library(seasonal)

source('utils.R')
select <- dplyr::select

## Préambule : Pour bien comprendre comment fonctionne l'API de l'Insee
?get_dataset_list # pas d'argment, importe juste toutes les séries existantes
?get_idbank_list # importe toutes les noms de séries d'une key list
?get_insee_dataset # importe toutes les séries (brutes) choisies dans une key list

## D'abord, on recense toutes les ensembles de données de l'Insee

all_datasets <- get_dataset_list()

keys_list = c(
  'IPPI-2021' # Indices de prix de production et d'importation dans l'industrie
  # Equipements électriques
)

df_idbank_list_selected =
  get_idbank_list(keys_list)

wb <- createWorkbook()
addWorksheet(wb=wb,sheetName = "All Datasets")
writeData(wb=wb,sheet="All Datasets",x=all_datasets)
if (!file.exists('data/All_Datasets.xlsx')) {
  saveWorkbook(wb=wb,file="data/All_Datasets.xlsx",overwrite=TRUE)
}


## A partir de cette liste, on garde


indicators <- all_datasets %>%
  filter(id %in% keys_list)


## Boucle sur chaque élément de keys_list
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Main indicators")
addWorksheet(wb, sheetName = keys_list)
writeData(wb, sheet = "Main indicators", x = indicators)
writeData(wb, sheet = keys_list, x = df_idbank_list_selected)
saveWorkbook(wb, file = "data/series.xlsx", overwrite = TRUE)

serie <- c('010764038') # IPPI Equipements électriques

ippi <- get_insee_idbank(serie) %>%
  filter(FREQ == "M") %>%
  select(date = DATE, values = OBS_VALUE, id_bank = IDBANK) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  arrange(date) %>%
  select(-id_bank) %>%
  rename(ippi=values)

### Visualisation de l'IPPI


gg_ippi <- ggplot(ippi) + aes(x=date,y=ippi, color="blue") +
  theme_bw() +
  geom_line(linewidth=1, color="blue") +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        plot.title = element_text(hjust=0.5,size=15),
        legend.position = "None"
  ) +
  labs(title = "Indice des prix à la production - IPPI (équipements électriques)",
       y="IPPI"
  )

gg_ippi

ippiQ <- mensuel_to_trimestriel(ippi,value_col="ippi")

ippiQ_ts <- df_to_ts(ippiQ,freq=4)

# ippi_ts <- df_to_ts(ippi,freq = 12)
start(ippiQ_ts)
end(ippiQ_ts)

ippi_ts <- window(ippiQ_ts, start=c(2002,4), end = c(2024,12))
ln_ippi_ts = log(ippiQ_ts)

serie_x13 <- seas(ln_ippi_ts)
ln_ippi_cvs_ts <- final(serie_x13)

df <- tibble(
  Date = time(final(serie_x13)),
  CVS = as.numeric(final(serie_x13)), 
  Tendance = as.numeric(trend(serie_x13)),
  Saisonnalité = as.numeric(seasonal(serie_x13)),
  Irrégulier = as.numeric(irregular(serie_x13))
)

# Fonction pour générer un ggplot
plot_series <- function(df, col, title) {
  ggplot(df, aes(x = Date, y = !!sym(col))) +
    geom_line(color = "steelblue", linewidth = 1) +
    theme_minimal() +
    labs(title = title) +
    theme(axis.title = element_blank())
}

# Création des graphiques
p1 <- plot_series(df, "CVS", "Série CVS (corrigée)")
p2 <- plot_series(df, "Tendance", "Tendance")
p3 <- plot_series(df, "Saisonnalité", "Composante Saisonnière")
p4 <- plot_series(df, "Irrégulier", "Composante Irrégulière")

# Affichage en grille
grid.arrange(p1, p2, p3, p4, ncol = 2)

dln_ippi_cvs_ts <- diff(ln_ippi_cvs_ts)
dln_ippi_cvs <- ts_to_df(dln_ippi_cvs_ts) %>%
  rename(inflation=x)

gg_ippi <- ggplot(dln_ippi_cvs) + aes(x = date, y=inflation) +
  theme_minimal() +
  geom_line(color = "steelblue", linewidth = 1) +
  theme(axis.title = element_blank(),
        plot.title=element_text(hjust=0.5)) +
  labs(title = expression(Delta ~ log ~ IPPI[CVS])) +
  scale_y_continuous(label=scales::percent)

gg_ippi

adf_test <- ur.df(ln_ippi_cvs_ts, type = "trend", selectlags = "BIC")  # Utilisation du type "drift" (trend) et sélection des lags par AIC
summary(adf_test)

phi3_hat <- adf_test@teststat[2]  # La statistique de test est stockée dans teststat
phi3 <- adf_test@cval[2, 3]

adf_test <- ur.df(dln_ippi_cvs_ts, type = "drift", selectlags = "BIC")  # Utilisation du type "drift" (trend) et sélection des lags par AIC

# Le test sélectionne bien 1 lag dans le test augmenté de DF
summary(adf_test)

# On vérifie que le nombre de retards optimaux est 1 avec la fonction VARselect
VARselect(dln_ippi_cvs_ts,lag.max=12)

## Test de la moyenne
mean(dln_ippi_cvs_ts)

# Test de la moyenne (test t pour une moyenne nulle)
t_test <- t.test(dln_ippi_cvs_ts, mu = 0)
t_test$p.value ## 0.0009997

## On rejette le test de la moyenne sur la série DLOG(IPPI)
## Il y a donc un "drift" (dérive systématique) de la série
## qu'on introduit en choisissant un test de DF "avec drift"
## C'est également cohérent avec la théorie économique : l'inflation a une tendance positive
## de long terme

## La série différenciée vérifie le test de Dickey-Fuller avec drift
tau2_hat <- adf_test@teststat[1]  # La statistique de test est stockée dans teststat
tau2 <- adf_test@cval[1, 2]

stationarity_log <- ifelse(phi3_hat > phi3, "Oui", "Non")
stationarity_diff <- ifelse(tau2_hat > tau2, "Oui", "Non")

# ADF quick and dirty:
adfTest(dln_ippi_cvs_ts, type = "c", lags=1)

# Philips-Perron (PP)
summary(ur.pp(dln_ippi_cvs_ts, model="constant", type="Z-tau", use.lag=1)) 
#reject H0 -17.132 

# Elliott-Rothenberg-Stock (ERS)
summary(ur.ers(dln_ippi_cvs_ts, model="constant", type="DF-GLS", lag.max=1)) #reject H0 -9.0845 < -1.94
summary(ur.ers(dln_ippi_cvs_ts, model="constant", type="P-test", lag.max=1)) #reject H0 0.2192 < 3.26

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS)
summary(ur.kpss(dln_ippi_cvs_ts, type="tau", lags="long")) #trend #do not reject H0  0.1294 < 0.146
summary(ur.kpss(dln_ippi_cvs_ts, type="mu", lags="long")) #constant # do not reject Ho  0.1474 < 0.463

### For M1:
# Check empirical ACF and PACF to fix maximum (p,q) for the potential ARMA(p,q)
# Calcul de l'ACF et PACF
acf_data <- acf(dln_ippi_cvs_ts, plot = FALSE,lag.max = 40)
pacf_data <- pacf(dln_ippi_cvs_ts, plot = FALSE,lag.max = 40)

# Conversion en data.frame
acf_df <- data.frame(Lag = acf_data$lag*12, ACF = acf_data$acf)
pacf_df <- data.frame(Lag = pacf_data$lag*12, PACF = pacf_data$acf)

# Graphique ACF
p1 <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  geom_hline(yintercept = c(-1.96/sqrt(length(dln_ippi_cvs_ts)), 1.96/sqrt(length(dln_ippi_cvs_ts))), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Autocorrelation Function (ACF)", x = "Lag", y = "ACF")

# Graphique PACF
p2 <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  geom_hline(yintercept = c(-1.96/sqrt(length(dln_ippi_cvs_ts)), 1.96/sqrt(length(dln_ippi_cvs_ts))), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Partial Autocorrelation Function (PACF)", x = "Lag", y = "PACF")

# Affichage des graphiques
grid.arrange(p1, p2, ncol = 2)

## On estime les 1ers modèles pour (p,d,q) = (3,0,4) car ce sont les lags les plus élevés pour lesquels
## l'ACF et la PACF sont signficativements non nuls

ar3 <- Arima(dln_ippi_cvs_ts,order=c(3,0,0))
ar3
ar2 <- Arima(dln_ippi_cvs_ts,order=c(2,0,0))
ar2
ar1 <- Arima(dln_ippi_cvs_ts,order=c(1,0,0))
ar1

ma4 <- Arima(dln_ippi_cvs_ts,order=c(0,0,4))
ma4

arma34 <- Arima(dln_ippi_cvs_ts,order=c(3,0,4))
arma34
arma24 <- Arima(dln_ippi_cvs_ts,order=c(2,0,4))
arma24
arma14 <- Arima(dln_ippi_cvs_ts,order=c(1,0,4))
arma14

ar1$bic
ma4$bic
arma14$bic

## On sélectionne le modèle MA(4) qui minimise le critère bayésien.
model <- ma4

hist(model$residuals,breaks=50) # residuals do not really seem to be normally dis.
Box.test(model$residuals, lag=40, type="Ljung-Box") #p value > 5%: on rejette l'hypothèse d'autocorrélation
shapiro.test(model$residuals) # on rejette H0 : les résidus ne suivent pas une distribution gaussienne
ks.test(model$residuals, rnorm) # De même

# png("plot6.png", width = 800, height = 500) #create a plot to visually inspect stationarity
# tsdiag(model,gof.lag=2) #reassuringly residuals seem to be uncorrelated
dev.off()

#============================== Forecasting ==================================#
# in sample and out of sample forecasts for both ts

### M1

# dynamic in-sample forecast: long period
# Générer un dataframe avec les valeurs observées et fittées

serie <- window(dln_ippi_cvs_ts,start=c(2015,1))
serie1 <- serie
serie2 <- window(dln_ippi_cvs_ts,end=c(2019,1))

dynamic_forecast <-forecast(object=serie1, model=ma4, h=88)

df_fitted <- data.frame(
  date = time(serie1),
  value = as.numeric(serie1),
  type = "Observé"
)

df_fitted$fitted <- fitted(dynamic_forecast)  # Ajout des valeurs ajustées

# Ajouter les prévisions sur 3 périodes avec l'intervalle de confiance
df_forecast <- plot_forecast(serie = serie1,forecast_model = dynamic_forecast,periods = 3)

df_forecast
