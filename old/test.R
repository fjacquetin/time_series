library(openxlsx)
library(tidyverse)
library(insee)
library(gridExtra) # grid.arrange
source("utils.R")

liste <- read.xlsx('data/series.xlsx',sheet="IPI-2021") %>%
  filter(BDM_EUR_IPI=='BDM',CORRECTION=="CVS-CJO",NATURE=="INDICE") %>%
  select(idbank) %>%
  arrange(idbank) %>%
  filter(!idbank %in% c('010767970'))

liste2 <- as.numeric(liste[[1]])
# liste2 <- liste2[1]
# Fonction pour tracer ACF et PACF
plot_acf_pacf <- function(series, serie_name ,option=1){
  # Extraire la série temporelle
  serie <- series %>% select(ipi)
  serie <- serie[,1]
  
  # ACF et PACF
  acf_data <- acf(serie, plot = FALSE, lag.max = 40)
  pacf_data <- pacf(serie, plot = FALSE, lag.max = 40)
  
  # Conversion en data.frame
  acf_df <- data.frame(Lag = acf_data$lag, ACF = acf_data$acf)
  pacf_df <- data.frame(Lag = pacf_data$lag, PACF = pacf_data$acf)
  serie_df <- data.frame(Date = time(serie), IPI = as.numeric(serie))
  
  # Graphique de la série temporelle
  p0 <- ggplot(serie_df, aes(x = Date, y = IPI)) +
    geom_line(color = "black") +
    theme_minimal() +
    labs(title = paste("Série temporelle -", serie_name), x = "Date", y = "IPI")
  
  # Graphique ACF
  p1 <- ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
    geom_hline(yintercept = c(-1.96/sqrt(length(serie)), 1.96/sqrt(length(serie))), 
               linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Autocorrelation Function (ACF) -", serie_name), x = "Lag", y = "ACF")
  
  # Graphique PACF
  p2 <- ggplot(pacf_df, aes(x = Lag, y = PACF)) +
    geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
    geom_hline(yintercept = c(-1.96/sqrt(length(serie)), 1.96/sqrt(length(serie))), 
               linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = paste("Partial Autocorrelation Function (PACF) -", serie_name), x = "Lag", y = "PACF")
  
  # Affichage des trois graphiques
  grid_plot <- grid.arrange(p0, p1, p2, ncol=2, nrow=2)
  
  # Enregistrer les graphiques dans des fichiers
  if(option == 1) {
    ggsave(paste0("acf_mois/", serie_name, ".png"), grid_plot, width = 12, height = 8)
  } else {
    ggsave(paste0("acf_trim/", serie_name, ".png"), grid_plot, width = 12, height = 8)
  }
}


# Itération sur la liste des séries
for(i in 1:length(liste2)) {
  serie <- paste0("0",as.character(liste2[i]))   # Convertir l'élément de la liste en vecteur numérique
  
  serie2 <- get_insee_idbank(serie) %>%
    select(date = DATE, values = OBS_VALUE, id_bank = IDBANK) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    arrange(date) %>%
    select(-id_bank) %>%
    rename(ipi=values) %>%
    filter(year(date)>=2021)
  
  serie3 <- mensuel_to_trimestriel(serie2,value_col="ipi")
  
  serie2 <- df_to_ts(serie2,freq=12)
  serie3 <- df_to_ts(serie3,freq=4)
  
  serie2 <- window(serie2, start=c(2000,1), end = c(2024,12))
  serie3 <- window(serie3, start=c(2000,1), end = c(2024,4))
  
  ln_ipi_ts = log(serie2)
  ln_ipi_ts2 = log(serie3)

  # Différencier la série
  dserie <- ts_to_df(diff(ln_ipi_ts))
  dserie2 <- ts_to_df(diff(ln_ipi_ts2))
  
  # Tracer et enregistrer ACF/PACF pour la série différenciée
  plot_acf_pacf(dserie, liste2[i],option=1)
  plot_acf_pacf(dserie2, liste2[i],option=2)
}
