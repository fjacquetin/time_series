## 0. Initializing ----

rm(list=ls())

# List of required packages
packages <- c("tidyverse", "insee", "openxlsx", "lubridate", "urca", "zoo", 
              "ggpubr", "forecast", "fUnitRoots", "gridExtra", "ggrepel", 
              "seasonal", "broom")

# Install missing packages
packages_to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

library(tidyverse) # dplyr, magrittr
library(insee) # API Insee
library(openxlsx) # saveworkbook
library(lubridate) # as.Date
library(urca) # ur.df
library(zoo) # yearqtr
library(ggpubr) # ggarrange
library(forecast) # forecast
library(fUnitRoots) # adfTest
library(gridExtra) # grid.arrange
library(ggrepel) # geom_text_repel
library(seasonal) # seas
library(broom) # tidy

## Load useful functions

select <- dplyr::select ## To avoid mistakes with different packages

ts_to_df <- function(data_ts){
  
  data_df <- as.data.frame(data_ts)
  freq <- frequency(data_ts)
  date_ts <- time(data_ts)
  date_df <- as.Date(date_ts)
  df_date <- data.frame(date=date_df)
  data <- cbind(df_date,data_df)
  
  return(data)
}

df_to_ts <- function(data, date_col = "date") {
  
  period <- data[[date_col]]  
  data2 <- data %>% select(-all_of(date_col))
  start_year <- lubridate::year(period[1])
  start_month <- lubridate::month(period[1])
  data_ts <- ts(data2, start = c(start_year, start_month), frequency = 12)
  
  return(data_ts)
}

plot_series <- function(df, col, title) {
  ggplot(df, aes(x = Date, y = !!sym(col))) +
    geom_line(color = "steelblue", linewidth = 1) +
    theme_minimal() +
    labs(title = title) +
    theme(axis.title = element_blank())
}

compare_arma_models <- function(series, p_max, q_max, file_name = "results/BIC_Models.xlsx") {
  models <- list()
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      
      if(p==0 & q==0){} else {
        model_name <- paste0("ARMA(", p, ",", q, ")")
        model <- tryCatch(
          Arima(series, order = c(p, 0, q)), 
          error = function(e) NULL
        )
        
        if (!is.null(model)) {
          models[[model_name]] <- model
        }
      }
    } 
    }
    bic_values <- sapply(models, BIC)
    aic_values <- sapply(models, AIC)
    
    bic_table <- data.frame(Model = names(bic_values), BIC = round(bic_values,1), AIC = round(aic_values,1))
    
    bic_table <- bic_table[order(bic_table$BIC), ]
    
    write.xlsx(paste0("results/",bic_table), file_name, row.names = FALSE)
    
    return(bic_table)

  }

## Start : How does work Insee's API
?get_dataset_list # No argument, just imports all existing series
?get_idbank_list # Imports all series names from a key list
?get_insee_dataset # Imports all (raw) series selected from a key list


## 1. Dataset ----

## First, list all datasets from Insee

all_datasets <- get_dataset_list()

keys_list = c(
  'IPI-2021' # IPI
)

df_idbank_list_selected =
  get_idbank_list(keys_list)

wb <- createWorkbook()
addWorksheet(wb=wb,sheetName = "All Datasets")
writeData(wb=wb,sheet="All Datasets",x=all_datasets)
if (!file.exists('data/All_Datasets.xlsx')) {
  saveWorkbook(wb=wb,file="data/All_Datasets.xlsx",overwrite=TRUE)
}

## We list all the different IPI in the file "data/series.xlsx"

indicators <- all_datasets %>%
  filter(id %in% keys_list)

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Main indicators")
addWorksheet(wb, sheetName = keys_list)
writeData(wb, sheet = "Main indicators", x = indicators)
writeData(wb, sheet = keys_list, x = df_idbank_list_selected)
saveWorkbook(wb, file = "data/series.xlsx", overwrite = TRUE)

## We chose the index "Manufacture of computers and peripheral equipment"

serie <- c(
  '010768019' # 26.20 - Manufacture of computers
 ) 

ipi <- get_insee_idbank(serie) %>%
  filter(FREQ == "M") %>%
  select(date = DATE, values = OBS_VALUE, id_bank = IDBANK) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  arrange(date) %>%
  select(-id_bank) %>%
  rename(ipi=values)

write.csv2(x = ipi,file="data/ipi.csv", row.names = FALSE)

### IPI Vizualisation ----

## We plot the monthly serie

gg_ipi <- ggplot(ipi) + aes(x=date,y=ipi, color="blue") +
  theme_bw() +
  geom_line(linewidth=1, color="blue") +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        plot.title = element_text(hjust=0.5,size=15),
        legend.position = "None"
  ) +
  labs(title = "IPI (base 100: 2021)",
       y="IPI (base 100: 2021)"
  )

gg_ipi

## We observe three dynamics
## (1990-200 : long-run increase
## 2000s-2016 : long-run decrease
## 2017-now : stable / slow decrease

## because we want to model the current dynamics, we focus on the last window (2017 to today)

ipi_ts <- window(df_to_ts(ipi), start=c(2017,1))

ln_ipi_ts = log(ipi_ts) ## Logarithmic transformation (stabilize variance and study log-growth)
ln_ipi <- ts_to_df(ln_ipi_ts) ## Function to transform a time serie into a dataframe object
ln_ipi$date <- as.Date(as.yearqtr(ln_ipi$date), format = "%YQ%q")

## First, we check that there is no seasonality by using a X13 Filter

serie_x13 <- seas(ln_ipi_ts) 

df <- tibble(
  Date = time(final(serie_x13)),
  `Deaseasonalized serie` = as.numeric(final(serie_x13)), 
  `Trend` = as.numeric(trend(serie_x13)),
  `Seasonal Component` = as.numeric(seasonal(serie_x13)),
  `Stochastic component` = as.numeric(irregular(serie_x13))
)

# Combination of the results of X13 Filter
p1 <- plot_series(df, "Deaseasonalized serie", "Deaseasonalized serie")
p2 <- plot_series(df, "Trend", "Trend")
p3 <- plot_series(df, "Seasonal Component", "Seasonal Component")
p4 <- plot_series(df, "Stochastic component", "Stochastic component")

grid.arrange(p1, p2, p3, p4, ncol=2)
gr <- grid.arrange(p1, p2, p3, p4, ncol=2)
## It's ok even if expected because CVS

ggsave(filename="figures/x13.png",gr, width=10,height=6)

## We have to look at the existence of a trend (deterministic or stochastic)
## The level looks non stationary because we observe a slow decrease since 2017

# We computed the first difference of the serie log(IPI)

dln_ipi_ts <- diff(ln_ipi_ts)
dln_ipi <- ts_to_df(dln_ipi_ts)

gg_dln_ipi <- ggplot(dln_ipi) + aes(x=date,y=ipi, color="blue") +
  theme_bw() +
  geom_line(linewidth=1, color="blue") +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size=13),
        plot.title = element_text(hjust=0.5,size=15),
        legend.position = "None"
  ) +
  labs(    title = expression(Delta * " log(IPI)"),
       y= expression(Delta * " log(IPI)")
  )

gg_dln_ipi
## It looks stationary, we will test it formally below

gr_ipi <- grid.arrange(gg_ipi,gg_dln_ipi,ncol=2)
ggsave("figures/ipi.png",gr_ipi,width=15,height=5)

dln_ipi$date <- as.Date(as.yearqtr(dln_ipi$date), format = "%YQ%q")

## Stationnarity tests ----

## We run 3 tests : ADF with trend on log(IPI), ADF with drift, ADF with drift on delta_log(IPI)
## the lags are automatically selected using a BIC criterion

# Augmented Dickey-Fuller (ADF) Test with trend
adf_test_trend <- ur.df(ln_ipi_ts, type = "trend", selectlags = "BIC")
summary(adf_test_trend)

phi3_hat <- adf_test_trend@teststat[2]  # Test statistic for trend
phi3 <- adf_test_trend@cval[2, 3]       # Critical value for trend
# Check if the null hypothesis (H0) can be rejected
H0_TEST_TREND <- phi3_hat < phi3
H0_TEST_TREND # Can't reject H0, so the trend coefficient is assumed to be null

# Augmented Dickey-Fuller (ADF) Test with drift
adf_test_drift <- ur.df(ln_ipi_ts, type = "drift", selectlags = "BIC")
summary(adf_test_drift)

# Extract the test statistic and the critical value for the drift test
tau2_hat <- adf_test_drift@teststat[1]  # Test statistic for drift
tau2 <- adf_test_drift@cval[1, 2]       # Critical value for drift
# Check if the null hypothesis (H0) can be rejected
H0_TEST_DRIFT <- tau2_hat > tau2
H0_TEST_DRIFT # Can't reject the hypothesis of a unit root, move to differenciating the series

# Check the mean of the differenced series
mean(dln_ipi_ts)

# Perform a t-test for the mean (null hypothesis: mean = 0)
t_test <- t.test(dln_ipi_ts, mu = 0)
t_test$p.value # 0.8441784, cannot reject the null hypothesis

## Technically, we could assume no drift, but we take this most general case anyway
adf_test_diff <- ur.df(dln_ipi_ts, type = "drift", selectlags = "BIC")
summary(adf_test_diff)

# Extract the test statistic and the critical value for the differenced series test
tau2_hat2 <- adf_test_diff@teststat[1]
tau22 <- adf_test_diff@cval[1, 2]
H0_TEST_DIFF <- tau2_hat2 > tau22
H0_TEST_DIFF  # FALSE => Reject H0, the differenced series is stationary

# Additional tests to confirm stationarity

# Phillips-Perron (PP) Test
pp_test <- ur.pp(dln_ipi_ts, model = "constant", type = "Z-tau", use.lag = 1)
summary(pp_test)
pp_stat <- pp_test@teststat
pp_crit <- pp_test@cval[1, 2]
H0_PP <- pp_stat > pp_crit
H0_PP  # FALSE → Reject H0, the series is stationary

# Elliott-Rothenberg-Stock (ERS) Test (DF-GLS)
ers_dfgls_test <- ur.ers(dln_ipi_ts, model = "constant", type = "DF-GLS", lag.max = 1)
summary(ers_dfgls_test)
ers_dfgls_stat <- ers_dfgls_test@teststat
ers_dfgls_crit <- ers_dfgls_test@cval[1, 2]
H0_ERS_DFGLS <- ers_dfgls_stat > ers_dfgls_crit
H0_ERS_DFGLS  # FALSE → Reject H0, the series is stationary

# Elliott-Rothenberg-Stock (ERS) Test (P-test)
ers_p_test <- ur.ers(dln_ipi_ts, model = "constant", type = "P-test", lag.max = 1)
summary(ers_p_test)
ers_p_stat <- ers_p_test@teststat
ers_p_crit <- ers_p_test@cval[1, 2]
H0_ERS_P <- ers_p_stat > ers_p_crit
H0_ERS_P  # FALSE → Reject H0, the series is stationary

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test (Tau)
kpss_tau_test <- ur.kpss(dln_ipi_ts, type = "tau", lags = "long")
summary(kpss_tau_test)
kpss_tau_stat <- kpss_tau_test@teststat
kpss_tau_crit <- kpss_tau_test@cval[1, 2]
H0_KPSS_TAU <- kpss_tau_stat < kpss_tau_crit
H0_KPSS_TAU  # TRUE → Do not reject H0, the series is stationary

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test (Mu)
kpss_mu_test <- ur.kpss(dln_ipi_ts, type = "mu", lags = "long")
summary(kpss_mu_test)
kpss_mu_stat <- kpss_mu_test@teststat
kpss_mu_crit <- kpss_mu_test@cval[1, 2]
H0_KPSS_MU <- kpss_mu_stat < kpss_mu_crit
H0_KPSS_MU  # TRUE → Do not reject H0, the series is stationary


test_results <- data.frame(
  Test = c("ADF Test (Trend)", "ADF Test (Drift)", "ADF Test (Differenced)",
           "Phillips-Perron Test", "ERS Test (DF-GLS)", "ERS Test (P-test)",
           "KPSS Test (Tau)", "KPSS Test (Mu)"),
  Coefficient_Tested = c("Linear Trend", "Unit Root", "Unit Root", "Unit Root", "Unit Root", "Unit Root", "Unit Root", "Unit Root"),
  Test_Statistic = c(phi3_hat, tau2_hat, tau2_hat2, pp_stat, ers_dfgls_stat, ers_p_stat, kpss_tau_stat, kpss_mu_stat),
  Critical_Value = c(phi3, tau2, tau22, pp_crit, ers_dfgls_crit, ers_p_crit, kpss_tau_crit, kpss_mu_crit),
  H0 = c("Trend Coefficient = 0", "Not Stationary", "Not stationary", "Not Stationary", "Not stationary", "Not Stationary", "Stationary", "Stationary"),
  Testing_H0 = c(H0_TEST_TREND, H0_TEST_DRIFT, H0_TEST_DIFF, H0_PP, H0_ERS_DFGLS, H0_ERS_P, H0_KPSS_TAU, H0_KPSS_MU)
  
)

test_results$Test_Statistic <- round(test_results$Test_Statistic,2)
test_results$Test_Statistic <- round(test_results$Critical_Value,2)
test_results$Testing_H0 <- as.logical(test_results$Testing_H0)

write.xlsx(test_results, "results/adf_test_results.xlsx")

## 2. Modeling ----
## using the Box-Jenkins approach

# Check empirical ACF and PACF to fix maximum (p,q) for the potential ARMA(p,q)

acf_data <- acf(dln_ipi_ts, plot = FALSE,lag.max = 30)
pacf_data <- pacf(dln_ipi_ts, plot = FALSE,lag.max = 30)

acf_df <- data.frame(Lag = acf_data$lag*12, ACF = acf_data$acf)
pacf_df <- data.frame(Lag = pacf_data$lag*12, PACF = pacf_data$acf)

# Autocorrelation function (ACF)
p1 <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  geom_hline(yintercept = c(-1.96/sqrt(length(dln_ipi_ts)), 1.96/sqrt(length(dln_ipi_ts))), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Autocorrelation Function (ACF)", x = "Lag", y = "ACF")

# Partial autocorrelation function (PACF)
p2 <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  geom_hline(yintercept = c(-1.96/sqrt(length(dln_ipi_ts)), 1.96/sqrt(length(dln_ipi_ts))), 
             linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Partial Autocorrelation Function (PACF)", x = "Lag", y = "PACF")

# Visualization of ACF and PACF
grid.arrange(p1, p2, ncol = 2)
acf <- grid.arrange(p1, p2, ncol = 2)

ggsave("figures/acf.png",acf,width=8,height=5)

## Graphically (pmax,qmax) = (2,1) because these are the highest lags for which the ACF and PACF are
## statistically signifiant

## We create the function below to produce all possible models ARMA(p,q) with p<=p_max, q<=q_max

## Application to our serie with pmax=2, qmax=1
bic_results <- compare_arma_models(dln_ipi_ts, p_max = 2, q_max = 1)
bic_results[1,]

## Comparison with the (dirty) automatic method
auto.arima(dln_ipi_ts) # The dirty way concludes to the same model

## We select the AR2=ARMA(2,0) model that minimizes the BIC
arma20 <- Arima(dln_ipi_ts,order=c(2,0,0))
best_model <- arma20

# Produce the regression table
arma20_tidy <- tidy(arma20)
arma20_tidy$z_stat <- arma20_tidy$estimate / arma20_tidy$std.error
arma20_tidy$Significance <- with(arma20_tidy, 
                                 ifelse(abs(z_stat) >= 3, "***", 
                                        ifelse(abs(z_stat) >= 2, "**", 
                                               ifelse(abs(z_stat) >= 1.96, "*", "ns"))))


# Calculate R-squared and RMSE
residuals <- residuals(arma20)
r_squared <- 1 - sum(residuals^2) / sum((dln_ipi_ts - mean(dln_ipi_ts))^2)
rmse <- sqrt(mean(residuals^2))

arma20_tidy <- rbind(arma20_tidy, data.frame(term = "R-squared", estimate = r_squared, std.error = NA, z_stat = NA, Significance = NA))
arma20_tidy <- rbind(arma20_tidy, data.frame(term = "RMSE", estimate = rmse, std.error = NA,  z_stat = NA, Significance = NA))

write.xlsx(arma20_tidy, "results/arma20_model_results.xlsx")


## Testing the residuals ----

## First, we check graphically the residuals
theme_set(theme_bw())
png("figures/residuals_diagnostics.png", width = 800, height = 500,bg = "white")
par(bg = "white")
checkresiduals(best_model) ## Residuals look non correlated and normal
dev.off() 

## Then, we test these assumptions formally

## We perform a test of autocorrelation (Ljung-Box) then 2 tests of normality (Shapiro, Kolmogorov-Smirnov)
ljung_box <- Box.test(best_model$residuals, lag = 40, type = "Ljung-Box")
print(ljung_box) # p-value > 5%: fail to reject H0 (no autocorrelation)

# Perform Shapiro-Wilk test for normality
shapiro_res <- shapiro.test(best_model$residuals)
print(shapiro_res) # Reject H0: residuals do not follow a normal distribution


# Perform Kolmogorov-Smirnov test for normality
ks_res <- ks.test(best_model$residuals, "pnorm", mean(best_model$residuals), sd(best_model$residuals))
print(ks_res) # Same: do not reject H0 (not normal)

diagnostic_tests <- data.frame(
  Test = c("Ljung-Box Test", "Shapiro-Wilk Test", "Kolmogorov-Smirnov Test"),
  H0 = c("No autocorrelation", "Normality", "No Normality"),
  p_value = c(ljung_box$p.value, shapiro_res$p.value, ks_res$p.value),
  Testing_H0 = c(ljung_box$p.value < 0.05, shapiro_res$p.value < 0.05, ks_res$p.value < 0.05)
)

diagnostic_tests <- diagnostic_tests %>%
  mutate(
    p_value = round(p_value, 2),
    Testing_H0 = ifelse(Testing_H0, "TRUE", "FALSE")  # Traduire les booléens en français
    )

write.xlsx(diagnostic_tests, "results/diagnostic_tests.xlsx")


## 3. Forecasting ----

## We run two forecasts : one "in-sample" forecast using the fitted value, and one "out-of-sample"
## using the observed values and assuming the innovation null on the forecast period

serie <- window(dln_ipi_ts,start=c(2023,1))
horizon <- 2 ## Horizon of the forecast
dynamic_forecast <- forecast(object = serie, model = best_model, h = horizon)

# Observed values
df_observed <- data.frame(
  date = as.numeric(time(serie)),
  value = as.numeric(serie),
  type = "IPI"
)

# Fitted values
df_fitted <- data.frame(
  date = as.numeric(time(serie)),
  value = as.numeric(fitted(dynamic_forecast)),
  type = "In-sample forecast"
)

## last Observation

last_obs <- df_observed[nrow(df_observed), ] %>%
  mutate(
    lower_80 = value, upper_80 = value,
    lower_95 = value, upper_95 = value,
    type = "Forecast"
  )

# Forecasted values and confidence intervals
df_forecast <- data.frame(
  date = as.numeric(time(dynamic_forecast$mean)),
  value = as.numeric(dynamic_forecast$mean),
  lower_80 = as.numeric(dynamic_forecast$lower[,1]),
  upper_80 = as.numeric(dynamic_forecast$upper[,1]),
  lower_95 = as.numeric(dynamic_forecast$lower[,2]),
  upper_95 = as.numeric(dynamic_forecast$upper[,2]),
  type = "Forecast"
)

df_forecast1 <- bind_rows(last_obs, df_forecast)
df_forecast2 <- df_forecast %>% select(date,value) %>% mutate(type="IPI")
df_forecast3 <- rbind(df_observed[nrow(df_observed),],df_forecast2) %>% mutate(type="Forecaste")

# Plot of the forecast
gg_forecast <- ggplot(df_observed, aes(x = date, y = value, color = type)) +
  geom_line(size=1) +
  geom_line(data=df_fitted,aes(x=date,y=value)) +
  geom_line(data=df_forecast1,aes(x=date,y=value), color="red",linetype="dashed") +
  geom_point(data=df_forecast, aes(x=date, y=value), color="red", size=1) +
  geom_label_repel(data=df_forecast, aes(x = date, y = value, label = paste0(round(100*value, 1),"%" )),
             fill = "white", color = "red", size = 3) +
  geom_ribbon(data = df_forecast1, aes(ymin = lower_80, ymax = upper_80,fill = "80% Confidence Interval"), alpha = 0.2) +
  geom_ribbon(data = df_forecast1, aes(ymin = lower_95, ymax = upper_95,fill = "95% Confidence Interval"), alpha = 0.1) +
  labs(
       y = expression(Delta * " log(IPI)")) +
  theme_minimal() +
  theme(axis.title=element_text(size=13),
        axis.title.x=element_blank(),
        axis.text=element_text(size=12),
        plot.title = element_text(hjust=0.5,size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  scale_x_continuous(breaks = seq(min(df_observed$date), max(df_observed$date), by = 1))  +
scale_y_continuous(label=scales::percent,breaks = seq(-0.6, 0.6, by = 0.1),limits = c(-0.6,0.6)) +
  scale_fill_manual(values = c("80% Confidence Interval" = "blue", "95% Confidence Interval" = "lightblue"))

gg_forecast

ggsave("figures/forecast.png",gg_forecast,height=5,width=10)