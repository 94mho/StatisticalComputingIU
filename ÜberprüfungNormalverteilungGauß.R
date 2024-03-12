# Lade die benötigten Bibliotheken
source('C:\\Users\\mho\\Documents\\StatisticalComputing\\BibliothekenInstallierenAufrufen.R')

libs_to_install <- c("dplyr", "ggplot2", "forecast", "zoo", "tseries", "lubridate", "gridExtra", "cowplot")
install_if_not_installed(libs_to_install)
load_libraries(libs_to_install)


# Normalverteilungsfunktion mit Mittelwert und Standardabweichung der Daten
normalverteilung <- function(data, color) {
  mean_val <- mean(data$Wert, na.rm = TRUE)
  sd_val <- sd(data$Wert, na.rm = TRUE)
  stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = color)
}

# Erstellen der Histogramme mit Normalverteilungs-Gaußkurve
hist_luftdruck <- ggplot(Luftdruck_data_OBS_DEU_P1D_P0, aes(x = Wert)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  normalverteilung(Luftdruck_data_OBS_DEU_P1D_P0, "red") +
  labs(title = "Verteilung des Luftdrucks", x = "Luftdruck in hPa", y = "Dichte") +
  theme_minimal()

hist_niederschlag <- ggplot(Niederschlag_data_OBS_DEU_P1D_RR, aes(x = Wert)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  normalverteilung(Niederschlag_data_OBS_DEU_P1D_RR, "red") +
  labs(title = "Verteilung des Niederschlags", x = "Niederschlag in mm", y = "Dichte") +
  theme_minimal()

hist_temperatur <- ggplot(Temperatur_data_OBS_DEU_P1D_T2M, aes(x = Wert)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  normalverteilung(Temperatur_data_OBS_DEU_P1D_T2M, "red") +
  labs(title = "Verteilung der Temperatur", x = "Temperatur in °C", y = "Dichte") +
  theme_minimal()


# Kombiniere die Histogramme gleiche Größe und untereinander
combined_plots <- plot_grid(
  hist_luftdruck + theme(legend.position = "none"),
  hist_niederschlag + theme(legend.position = "none"),
  hist_temperatur + theme(legend.position = "none"),
  ncol = 1, align = "v"
)

# Speichern der Grafik
ggsave("VerteilungGauß.jpg", combined_plots, width = 10, height = 15, dpi = 300)

# Ausgabe der kombinierten Grafik
print(combined_plots)