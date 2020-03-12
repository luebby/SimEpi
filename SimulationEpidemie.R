################
# Hinweis:
# Diese Simulation enthält eine ganze Menge vereinfachender Annahmen bzgl.
# u.a.
# - Infektionsdauer (immer gleich)
# - Anzahl Kontakte (immer gleich)
# - Übertragungwahrscheinlichkeit (immer gleich)
# - keine Übertragungcluster o.ä.
# - keine Quarantäne, keine Inkubationszeit o.ä.
# Daher spiegelt sie NICHT den wahren Verlauf einer Epidemie wieder
# Sie dient einzig einer Motivation von #flattenthecurve


################
# Vorbereitungen
################

# Zufallszahlengenerator setzen
set.seed(1896)

# mosaic laden
library(mosaic)

##########################
# Rahmenbedingungen setzen
##########################

# Hinweis:
# - die Zahlen sind rein exemplarisch, ohne Bezug zu konkreten Infektionskrańkheiten!

# Größe der Population
N <- 1000
# Anzahl Infizierte zu Beginn der Epidemie
n <- 10
# Übertragungswahrscheinlichkeit
p <- 1/100
# Laufzeit Simulation
Tm <- 100
# Dauer Infektiös
ti <- 20
# Kapiziät Gesundheitssystem
ch <- 0.2

#################
# Anzahl Kontakte
# Dies ist der entscheidene Parameter der z.B. durch Absage von Großveranstaltungen verändert wird
k <- 50

###########################
# Population initialisieren
# Matrix bereitstellen:
# 1. Spalte: Tag
# 2. Spalte: Infizierte
# 3. Spalte: Nicht-Infizierte
# 4. Spalte: Inaktive
# 5. Spalte: Neuinfizierte
# In den Zeilen: Tagesdaten

SimEpi <- matrix(0, nrow = Tm, ncol = 5)

# 1. Spalte Tage eintragen
SimEpi[,1] <- 1:Tm
# 2. Spate: 1. Tag: Anzahl Infizierte
SimEpi[1,2] <- n
# 3. Spalte: 1. Tag: Anzahl Nicht-Infizierte
SimEpi[1,3] <- N-n
# 4. Spalte: 1. Tag: Inaktive
SimEpi[1,4] <- 0
# 5. Spalte: 1. Tag: Neuinfizierte
SimEpi[1,5] <- n


# Beginn Simulation Verlauf
for (j in 2:Tm)
{
  # Anzahl infizierter des Vortags
  n <- SimEpi[(j-1),2]
  # Anzahl mögliche Neuinfektionen
  m <- SimEpi[(j-1),3]
    
  # Wenn es Infizierte am Vortag gibt, können diese Übertragen
  if(n > 0)
    # Jeder der Infizierten kann bis zu k (Anzahl Kontakte) infizieren
    for(i in 1:SimEpi[(j-1),2])
    {
      # Wahrscheinlichkeit, dass Kontakt Nicht-Infiziert ist:
      pni <- m/N
      # Simulierte Anzahl Neuinfektionen durch den Infizierten i:
      # k Kontakte, unabhänige Wahrscheinlichkeiten der Übertragung sowie dass der Kontakt noch nicht infektiös ist.
      nni <- as.numeric(rflip(k, prob = pni * p))
      # Anzahlen anpassen
      n <- n + nni # Anzahl Infizierter erhöhen um Neuinfizierte
      m <- m - nni # Anzahl nicht-infizierter entsprechend reduzieren
    }
  
  # Anzahl Neu-Inaktiver
  ng <- 0
  # Achtung: Inaktiv z.B. durch Genesung, aber auch Versterben möglich!
  # Kann es schon Inaktive geben?
  if (j > ti) 
  {
    # Wenn ja: Anzahl Neu-Inaktiver: Neuinfizierte vor "Dauer Infektiös" Tagen
    ng <- SimEpi[(j-ti),5]
  }
  
  # Daten eintragen
  SimEpi[j,2] <- n - ng # Infizierte um Neu-Inaktive verrignern
  SimEpi[j,3] <- m # Nicht-Infizierte
  SimEpi[j,4] <- SimEpi[(j-1),4] + ng # Inaktive um Neu-Inaktive erhöhen
  SimEpi[j,5] <- n - SimEpi[(j-1),2]  # Neuinfektionen: Vergleich Vortag
}

###################
# Daten aufbereiten
SimEpi <- data.frame(SimEpi)
colnames(SimEpi) <- c("Tag","Infizierte","NichtInfizierte","Inaktive","Neuinfektionen")

# Visualisieren
gf_line(Infizierte ~ Tag, data = SimEpi) %>%
  gf_labs(title=paste0("Maximale Anzahl Infizierte zu einem Zeitpunkt: ", max(~Infizierte, data = SimEpi)),
          subtitle=paste0("Bei Anzahl Kontakten: ",k)) %>%
  gf_lims(y=c(0,N)) %>%
  gf_hline(yintercept = ~(N * ch), color = "red")


