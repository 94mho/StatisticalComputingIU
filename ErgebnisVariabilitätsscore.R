# Aufrufen der gespeicherten Daten aus Min-Max-FunktionDatennormlisierung
combined_variability_score <- readRDS('C:\\Users\\mho\\Documents\\StatisticalComputing\\variablitaetsscore.rds')

# Kombinierten Variabilitätsscore als Tabelle ausgeben
kable(combined_variability_score)

# Horizontales Balkendiagramm erstellen
ggplot(plot_data, aes(x = reorder(Wetterstation_Name, -Gesamtvariabilitätsscore), y = Gesamtvariabilitätsscore, fill = Wetterstation_Name == Wetterstation_Name[min_index])) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("skyblue", min_color), guide = "none") +
  labs(title = "Gesamtvariabilitätsscores der Wetterstationen",
       x = "Wetterstation",
       y = "Gesamtvariabilitätsscore") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

# Speichern der Grafik
ggsave("GesamtvariabilitätDerWetterstationen.jpg", combined_plots, width = 10, height = 15, dpi = 300)
 