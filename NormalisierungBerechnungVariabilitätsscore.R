# Lade die benötigten Bibliotheken
source('C:\\Users\\mho\\Documents\\StatisticalComputing\\BibliothekenInstallierenAufrufen.R')

libs_to_install <- c("dplyr", "ggplot2", "forecast", "zoo", "tseries", "lubridate", "gridExtra", "cowplot")
install_if_not_installed(libs_to_install)
load_libraries(libs_to_install)

# Min-Max-Normalisierungsfunktion
min_max_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Funktion zur Normalisierung der Daten für einen Parameter
normalize_data <- function(data, parameter_name) {
  data %>%
    mutate(!!paste0(parameter_name, "_normalized") := min_max_normalize(Wert))
}

# Funktion zur Berechnung des Variabilitätsscores für einen Parameter
calculate_variability_score <- function(data, parameter_name) {
  data %>%
    group_by(Wetterstation_Name) %>%
    summarise(!!paste0(parameter_name, "_Score") := sd(!!sym(paste0(parameter_name, "_normalized")), na.rm = TRUE)) %>%
    ungroup()
}

# Daten normalisieren
Luftdruck_normalized <- normalize_data(Luftdruck_data_OBS_DEU_P1D_P0, "Luftdruck")
Niederschlag_normalized <- normalize_data(Niederschlag_data_OBS_DEU_P1D_RR, "Niederschlag")
Temperatur_normalized <- normalize_data(Temperatur_data_OBS_DEU_P1D_T2M, "Temperatur")

# Variabilitätsscores berechnen
Luftdruck_variability <- calculate_variability_score(Luftdruck_normalized, "Luftdruck")
Niederschlag_variability <- calculate_variability_score(Niederschlag_normalized, "Niederschlag")
Temperatur_variability <- calculate_variability_score(Temperatur_normalized, "Temperatur")

# Kombinierten Variabilitätsscore berechnen
combined_variability_score <- Luftdruck_variability %>%
  full_join(Niederschlag_variability, by = "Wetterstation_Name") %>%
  full_join(Temperatur_variability, by = "Wetterstation_Name") %>%
  mutate(Gesamtvariabilitätsscore = sqrt(
    Luftdruck_Score^2 +
      Niederschlag_Score^2 +
      Temperatur_Score^2
  )) %>%
  arrange(Gesamtvariabilitätsscore)

# Daten für ggplot2 vorbereiten
plot_data <- combined_variability_score %>%
  select(Wetterstation_Name, Gesamtvariabilitätsscore)

# Farbe für den kleinsten Gesamtvariabilitätsscore festlegen
min_color <- "steelblue"

# Index der kleinsten Gesamtvariabilitätsscore finden
min_index <- which.min(plot_data$Gesamtvariabilitätsscore)


# Kombinierten Variabilitätsscore speichern - hier bitte den Pfad für dich anpassen
saveRDS(combined_variability_score, file = 'C:\\Users\\mho\\Documents\\StatisticalComputing\\variablitaetsscore.rds')

