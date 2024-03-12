# Installieren aller benötigten Bibliotheken (falls noch nicht installiert)
install_if_not_installed <- function(libs) {
  for (lib in libs) {
    if (!requireNamespace(lib, quietly = TRUE)) {
      install.packages(lib, dependencies = TRUE)
      if (!requireNamespace(lib, quietly = TRUE)) {
        stop(paste("Die Installation von", lib, "war nicht erfolgreich. Überprüfen Sie die Installationsmeldung."))
      }
    }
  }
}

# Laden der Bibliotheken
load_libraries <- function(libs) {
  lapply(libs, library, character.only = TRUE)
}

# Bibliotheken installieren (falls benötigt) und laden
libs_to_install <- c("dplyr", "ggplot2", "forecast", "zoo", "tseries", "lubridate", "gridExtra", "cowplot", "knitr")
install_if_not_installed(libs_to_install)
load_libraries(libs_to_install)
