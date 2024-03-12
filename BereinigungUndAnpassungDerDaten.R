# Lade die benötigten Bibliotheken
source('C:\\Users\\mho\\Documents\\StatisticalComputing\\BibliothekenInstallierenAufrufen.R')

libs_to_install <- c("dplyr", "ggplot2", "forecast", "zoo", "tseries", "lubridate", "gridExtra", "cowplot")
install_if_not_installed(libs_to_install)
load_libraries(libs_to_install)

# Luftdruck - Spalten löschen 
Luftdruck_data_OBS_DEU_P1D_P0 <- select(Luftdruck_data_OBS_DEU_P1D_P0, -c("Produkt_Code", "Qualitaet_Byte", "Qualitaet_Niveau"))

# Niederschlag - Spalten löschen 
Niederschlag_data_OBS_DEU_P1D_RR <- select(Niederschlag_data_OBS_DEU_P1D_RR, -c("Produkt_Code", "Qualitaet_Byte", "Qualitaet_Niveau"))

# Temperatur - Spalten löschen 
Temperatur_data_OBS_DEU_P1D_T2M <- select(Temperatur_data_OBS_DEU_P1D_T2M, -c("Produkt_Code", "Qualitaet_Byte", "Qualitaet_Niveau"))

# Name für die Wetterstation hinzufügen (für jede Datei)
# Luftdruck
wetterstationen <- data.frame(
  SDO_ID = c(5839, 1766, 1270, 232, 4271, 3631, 2712, 3126, 2564, 3379),
  Wetterstation_Name = c("Emden", "Münster/Osnabrück", "Erfurt-Weimar", "Augsburg", "Rostock-Warnemünde", "Norderney", "Konstanz", "Magdeburg", "Kiel-Holtenau", "München-Stadt")
)

# Initialisiere die Spalte in Luftdruck_data_OBS_DEU_P1D_P0
Luftdruck_data_OBS_DEU_P1D_P0$Wetterstation_Name <- NA

# Füge eine Spalte für den Wetterstationsnamen hinzu
for (i in 1:nrow(wetterstationen)) {
  Luftdruck_data_OBS_DEU_P1D_P0$Wetterstation_Name[Luftdruck_data_OBS_DEU_P1D_P0$SDO_ID == wetterstationen$SDO_ID[i]] <- wetterstationen$Wetterstation_Name[i]
}

# Niederschlag
Niederschlag_data_OBS_DEU_P1D_RR$Wetterstation_Name <- NA
for (i in 1:nrow(wetterstationen)) {
  Niederschlag_data_OBS_DEU_P1D_RR$Wetterstation_Name[Niederschlag_data_OBS_DEU_P1D_RR$SDO_ID == wetterstationen$SDO_ID[i]] <- wetterstationen$Wetterstation_Name[i]
}

# Temperatur
Temperatur_data_OBS_DEU_P1D_T2M$Wetterstation_Name <- NA
for (i in 1:nrow(wetterstationen)) {
  Temperatur_data_OBS_DEU_P1D_T2M$Wetterstation_Name[Temperatur_data_OBS_DEU_P1D_T2M$SDO_ID == wetterstationen$SDO_ID[i]] <- wetterstationen$Wetterstation_Name[i]
}

