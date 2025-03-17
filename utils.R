df_to_ts <- function(data, date_col = "date", freq = 4) {
  
  period <- data[[date_col]]  # Extraire la colonne date
  data2 <- data %>% select(-all_of(date_col))  # Supprimer la colonne date
  
  if(freq == 4) {
    start_year <- as.numeric(substr(period[1], 1, 4))
    start_quarter <- as.numeric(substr(period[1], 7, 7))
    data_ts <- ts(data2, start = c(start_year, start_quarter), frequency = freq)
  } else if (freq == 12) {
    start_year <- lubridate::year(period[1])  # Extraire l'année
    start_month <- lubridate::month(period[1])  # Extraire le mois
    data_ts <- ts(data2, start = c(start_year, start_month), frequency = freq)
  }
  
  return(data_ts)
}

convert_to_quarter <- function(ts_index) {
  year <- floor(ts_index)
  quarter <- ((ts_index - year) * 4) + 1
  quarter_label <- paste0(year, "Q", quarter)
  return(quarter_label)
}

ts_to_df <- function(data_ts){
  
  data_df <- as.data.frame(data_ts)
  freq <- frequency(data_ts)
  date_ts <- time(data_ts)
  
  if(freq==4) {
    date_df <- convert_to_quarter(date_ts)
  } else if (freq==12) {
    date_df <- as.Date(date_ts)
  }
  df_date <- data.frame(date=date_df)
  data <- cbind(df_date,data_df)
  
  return(data)
}

library(zoo)

mensuel_to_trimestriel <- function(data, date_col = "date", value_col) {
  data <- data %>%
    mutate(quarter = as.yearqtr(.data[[date_col]])) 
  
  # Vérifier le nombre d'observations par trimestre
  counts <- data %>%
    group_by(quarter) %>%
    summarise(n = n(), .groups = "drop")
  
  # Filtrer les trimestres complets (3 mois)
  full_quarters <- counts %>%
    filter(n == 3) %>%
    pull(quarter)
  
  # Agrégation trimestrielle
  data_q <- data %>%
    filter(quarter %in% full_quarters) %>%
    group_by(quarter) %>%
    summarise(!!value_col := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    rename(date = quarter)
  
  return(data_q)
}

plot_forecast <- function(serie, forecast_model, periods = 3) {
  # Créer le dataframe des valeurs observées et fittées
  df_fitted <- data.frame(
    date = time(serie),
    value = as.numeric(serie),
    type = "Observé"
  )
  
  df_fitted$fitted <- fitted(forecast_model)  # Ajout des valeurs ajustées
  
  # Ajouter les prévisions sur 'periods' périodes avec l'intervalle de confiance
  df_forecast <- data.frame(
    date = time(forecast_model$mean)[1:periods],
    value = as.numeric(forecast_model$mean[1:periods]),
    lower = as.numeric(forecast_model$lower[1:periods, 2]),
    upper = as.numeric(forecast_model$upper[1:periods, 2]),
    type = "Forecast"
  )
  
  # Transformer les données en format long
  df_fitted_long <- df_fitted %>%
    select(date, value) %>%
    mutate(type = "Observé")
  
  df_fitted_long_fitted <- df_fitted %>%
    select(date, fitted) %>%
    rename(value = fitted) %>%
    mutate(type = "Fitté")
  
  df_plot <- bind_rows(df_fitted_long, df_fitted_long_fitted, df_forecast)
  
  # Tracer le graphique avec ggplot2
  ggplot(df_plot, aes(x = date, y = value, color = type)) +
    geom_line() +
    geom_ribbon(data = df_forecast, aes(ymin = lower, ymax = upper), fill = "grey", alpha = 0.4) +
    labs(title = "Prévision avec intervalle de confiance",
         x = "Temps",
         y = "Valeur",
         color = "Type") +
    theme_minimal() +
    theme(axis.title=element_blank()) +
    scale_y_continuous(labels=scales::percent)
}
