# Paket laden
library(sonify)
library(ggformula)
library(gganimate)
library(tidyverse)
library(zoo)




# Datenquelle: https://opendata.dortmund.de/Informationsportal/
# Daten einlesen. 
CovidDo <- read.csv2("~/Downloads/FB53-Coronafallzahlen.csv", na.strings = "-")

duration <- round(nrow(CovidDo)/30.4) # Dauer (in sec.) des Videos

# Datum als Datum transformieren
CovidDo <- CovidDo %>%
  mutate(datum = as.Date(Datum, format = "%d.%m.%Y")) %>%
  mutate(stationär = rollmean(darunter.aktuell.stationär.behandelte.Personen,
                              k=7, fill=NA)) %>%
  select(datum, stationär)

# Daten sonifizieren
soni_corona <- sonify(y = CovidDo$stationär, duration = duration, play = FALSE)
writeWave(soni_corona, "corona.wav")

# Daten plotten
p_corona <- gf_line(stationär~ datum , data = CovidDo, size = 2) %>%
  gf_theme(panel.background=element_blank()) +
  ggtitle("Covid-19 Entwicklung Dortmund") +
  labs(caption = "Quelle: opendata.dortmund.de ", y="Stationär behandelt (7-Tage Mittelwert)")

# Anomation erstellen
anim_corona <- p_corona + transition_reveal(along=datum)
anim_save("corona.gif", animate(anim_corona, duration = duration, fps = 10))

# gif zu mp4 konvertieren
system("ffmpeg -f gif -i corona.gif coronadummy.mp4")
# Bild und Ton zusammenführen
system("ffmpeg -i coronadummy.mp4 -i corona.wav -c:v copy -c:a aac corona.mp4")
