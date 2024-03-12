# Lade die benötigten Bibliotheken
source('C:\\Users\\mho\\Documents\\StatisticalComputing\\BibliothekenInstallierenAufrufen.R')

libs_to_install <- c("dplyr", "ggplot2", "forecast", "zoo", "tseries", "lubridate", "gridExtra", "cowplot")
install_if_not_installed(libs_to_install)
load_libraries(libs_to_install)

# Filtere die Daten für die Station Norderney
luftdruck_data <- filter(Luftdruck_data_OBS_DEU_P1D_P0, Wetterstation_Name == "Norderney")
niederschlag_data <- filter(Niederschlag_data_OBS_DEU_P1D_RR, Wetterstation_Name == "Norderney")
temperatur_data <- filter(Temperatur_data_OBS_DEU_P1D_T2M, Wetterstation_Name == "Norderney")

# Konvertierung  Datum in Date-Objekte
luftdruck_data$Zeitstempel <- as.Date(luftdruck_data$Zeitstempel, format = "%Y-%m-%d")
niederschlag_data$Zeitstempel <- as.Date(niederschlag_data$Zeitstempel, format = "%Y-%m-%d")
temperatur_data$Zeitstempel <- as.Date(temperatur_data$Zeitstempel, format = "%Y-%m-%d")

# Startdatum Zeitreihen ermitteln
start_dates <- c(min(luftdruck_data$Zeitstempel, na.rm = TRUE),
                 min(niederschlag_data$Zeitstempel, na.rm = TRUE),
                 min(temperatur_data$Zeitstempel, na.rm = TRUE))
start_date <- min(start_dates, na.rm = TRUE)

start_year <- lubridate::year(start_date)
start_month <- lubridate::month(start_date)
frequency <- 365

# Erstellung Zeitreihenobjekte
luftdruck_ts <- ts(luftdruck_data$Wert, start = c(start_year, start_month), frequency = frequency)
niederschlag_ts <- ts(niederschlag_data$Wert, start = c(start_year, start_month), frequency = frequency)
temperatur_ts <- ts(temperatur_data$Wert, start = c(start_year, start_month), frequency = frequency)

# Prognosen
steps_to_forecast <- ceiling(365.25 * 3) # Berechnung basierend auf 3 Jahren

# Luftdruckprognose
forecast_fit_luftdruck <- auto.arima(luftdruck_ts, stepwise = TRUE, seasonal = TRUE, D = 1,
                                     max.p=5, max.q=5, max.P=2, max.Q=2, max.order=10, max.d=2, max.D=1)
forecast_result_luftdruck <- forecast(forecast_fit_luftdruck, h = steps_to_forecast)

# Niederschlagsprognose
forecast_fit_niederschlag <- auto.arima(niederschlag_ts, stepwise = TRUE, seasonal = TRUE, D = 1,
                                        max.p=5, max.q=5, max.P=2, max.Q=2, max.order=10, max.d=2, max.D=1)
forecast_result_niederschlag <- forecast(forecast_fit_niederschlag, h = steps_to_forecast)

# Temperaturprognose
forecast_fit_temperatur <- auto.arima(temperatur_ts, stepwise = TRUE, seasonal = TRUE, D = 1,
                                      max.p=5, max.q=5, max.P=2, max.Q=2, max.order=10, max.d=2, max.D=1)
forecast_result_temperatur <- forecast(forecast_fit_temperatur, h = steps_to_forecast)

# Setze das Startdatum der Prognose auf den Tag nach dem last_date
start_forecast_date <- as.Date("2024-01-01")

# Funktion zum Runden der Vorhersagewerte
round_forecast <- function(df, decimal_places = 1) {
  df$Mean <- round(df$Mean, decimal_places)
  df$Lo80 <- round(df$Lo80, decimal_places)
  df$Hi80 <- round(df$Hi80, decimal_places)
  df$Lo95 <- round(df$Lo95, decimal_places)
  df$Hi95 <- round(df$Hi95, decimal_places)
  return(df)
}

# Umwandeln des Forecast-Objekts in ein Data Frame - Luftdruck
forecast_df_luftdruck <- data.frame(
  Date = seq(from = start_forecast_date, by = "1 day", length.out = length(forecast_result_luftdruck$mean)),
  Mean = forecast_result_luftdruck$mean,
  Lo80 = forecast_result_luftdruck$lower[, "80%"],
  Hi80 = forecast_result_luftdruck$upper[, "80%"],
  Lo95 = forecast_result_luftdruck$lower[, "95%"],
  Hi95 = forecast_result_luftdruck$upper[, "95%"]
)

# Runden - 1 Nachkommastelle
forecast_df_luftdruck <- round_forecast(forecast_df_luftdruck)

# Erstellen des Plots für den Luftdruck
forecast_ggplot_luftdruck <- ggplot(forecast_df_luftdruck, aes(x = Date)) +
  geom_line(aes(y = Mean), color = "red") +
  geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "darkgrey", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "yellow", alpha = 0.2) +
  ggtitle("Luftdruck-Vorhersage für Norderney") +
  xlab("Jahr") + ylab("Luftdruck in hPa") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2024-01-01", "2026-12-31")))

# Umwandeln des Forecast-Objekts in ein Data Frame - Niederschlag
niederschlag_forecast_df <- data.frame(
  Date = seq(from = start_forecast_date, by = "1 day", length.out = length(forecast_result_niederschlag$mean)),
  Mean = pmax(0, forecast_result_niederschlag$mean),  # Ersetzt negative Werte durch 0
  Lo80 = pmax(0, forecast_result_niederschlag$lower[, "80%"]), # Ersetzt negative Werte durch 0
  Hi80 = pmax(0, forecast_result_niederschlag$upper[, "80%"]), # Ersetzt negative Werte durch 0
  Lo95 = pmax(0, forecast_result_niederschlag$lower[, "95%"]), # Ersetzt negative Werte durch 0
  Hi95 = pmax(0, forecast_result_niederschlag$upper[, "95%"])  # Ersetzt negative Werte durch 0
)
# Runden - 1 Nachkommastelle
niederschlag_forecast_df <- round_forecast(niederschlag_forecast_df)

# Erstellen des Plots für den Niederschlag
forecast_ggplot_niederschlag <- ggplot(niederschlag_forecast_df, aes(x = Date)) +
  geom_line(aes(y = Mean), color = "red") +
  geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "darkgrey", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "yellow", alpha = 0.2) +
  ggtitle("Niederschlag-Vorhersage für Norderney") +
  xlab("Jahr") + ylab("Niederschlag in mm") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Umwandeln des Forecast-Objekts in ein Data Frame - Temperatur
forecast_df_temperatur <- data.frame(
  Date = seq(from = start_forecast_date, by = "1 day", length.out = length(forecast_result_temperatur$mean)),
  Mean = forecast_result_temperatur$mean,
  Lo80 = forecast_result_temperatur$lower[, "80%"],
  Hi80 = forecast_result_temperatur$upper[, "80%"],
  Lo95 = forecast_result_temperatur$lower[, "95%"],
  Hi95 = forecast_result_temperatur$upper[, "95%"]
)
# Runden - 1 Nachkommastelle
forecast_df_temperatur <- round_forecast(forecast_df_temperatur)

# Erstellen des Plots für die Temperatur
forecast_ggplot_temperatur <- ggplot(forecast_df_temperatur, aes(x = Date)) +
  geom_line(aes(y = Mean), color = "red") +
  geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "darkgrey", alpha = 0.5) +
  geom_ribbon(aes(ymin = Lo95, ymax = Hi95), fill = "yellow", alpha = 0.2) +
  ggtitle("Temperatur-Vorhersage für Norderney") +
  xlab("Jahr") + ylab("Temperatur in °C") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               limits = as.Date(c("2024-01-01", "2026-12-31")))   

# Speichern der Plots als Bilddatei
ggsave("Luftdruckvorhersage.jpg", forecast_ggplot_luftdruck, width = 10, height = 6, units = "in")
ggsave("Niederschlagvorhersage.jpg", forecast_ggplot_niederschlag, width = 10, height = 6, units = "in")
ggsave("Temperaturvorhersage.jpg", forecast_ggplot_temperatur, width = 10, height = 6, units = "in")

# Erstellen von Listen für Prognosen
luftdruck_forecast_list <- list(
  Parameter = "Luftdruck",
  Forecast = forecast_df_luftdruck
)

niederschlag_forecast_list <- list(
  Parameter = "Niederschlag",
  Forecast = niederschlag_forecast_df
)

temperatur_forecast_list <- list(
  Parameter = "Temperatur",
  Forecast = forecast_df_temperatur
)

# Speichern der Vorhersagewerte 2024 - 2026
write.table(luftdruck_forecast_list, file = "luftdruck_forecast_list.txt", quote = FALSE, row.names = FALSE, col.names = TRUE)
write.table(niederschlag_forecast_list, file = "niederschlag_forecast_list.txt", quote = FALSE, row.names = FALSE, col.names = TRUE)
write.table(temperatur_forecast_list, file = "temperatur_forecast_list.txt", quote = FALSE, row.names = FALSE, col.names = TRUE)

# Plot der Vorhersagen anzeigen
print(forecast_ggplot_luftdruck)
print(forecast_ggplot_niederschlag)
print(forecast_ggplot_temperatur)

# Falls nötig: Anzeigen der Vorhersagewerte für jeden Tag 2024 - 2026
print(luftdruck_forecast_list)
print(niederschlag_forecast_list)
print(temperatur_forecast_list)

