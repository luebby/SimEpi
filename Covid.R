# Vorbereitung

# Paket laden
library(tidyverse)

# Datenquelle: https://opendata.dortmund.de/Informationsportal/
# Daten einlesen. 
CovidDo <- read.csv2("~/Downloads/FB53-Coronafallzahlen.csv", na.strings = "-")

# Datum als Datum transformieren
CovidDo <- CovidDo %>%
  mutate(Datum = as.Date(Datum, format = "%d.%m.%Y")) %>%
  mutate(Anteil.Stationär =darunter.aktuell.stationär.behandelte.Personen/aktuell.erkrankte.Personen)

# Transformationsfaktor Achsenverhältnis Infizierte/Stationär
b <- 10

# Abbildung
ggplot(CovidDo, aes(Datum, aktuell.erkrankte.Personen)) +
  geom_col(alpha = 0.2, colour = "orange") +
  geom_line(aes(y = darunter.aktuell.stationär.behandelte.Personen*b), color = "blue") + 
  scale_y_continuous("Infizierte", sec.axis = sec_axis(~ (./b), name = "Stationär")) +
  theme_minimal() +
  theme(axis.line.y.right = element_line(color = "blue"), 
        axis.ticks.y.right = element_line(color = "blue"),
        axis.text.y.right = element_text(color = "blue"), 
        axis.title.y.right = element_text(color = "blue"))  +
  theme(axis.line.y.left = element_line(color = "orange"), 
        axis.ticks.y.left = element_line(color = "orange"),
        axis.text.y.left = element_text(color = "orange"), 
        axis.title.y.left = element_text(color = "orange")) +
  ggtitle("Covid-19 Entwicklung Dortmund", 
          subtitle = "Unterschiedliche Skalierung beachten!") +
  labs(caption = "Quelle: opendata.dortmund.de ")
  

