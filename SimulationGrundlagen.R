################
# Hinweis:
# Diese Simulation enthält eine ganze Menge vereinfachender Annahmen bzgl.
# u.a.
# - Infektionswahrscheinlichkeit (immer gleich)
# - Symptomwahrscheinlichkeit (immer gleich)
# Daher spiegelt sie NICHT den wahren Verteilung innerhalb einer Epidemie wieder



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
N <- 1000000

# Wahrscheinlichkeit Infiziert
p.i <- 0.01

# Wahrscheinlichkeit Symptome, wenn infiziert
p.s.i <- 0.25

# Wahrscheinlichkeit für Symptome aus anderen Gründen als Infektion
p.s.oi <- 0.05

########################
# Simulation Bevölkerung
########################

############
# Infizierte 
n.i <- as.numeric(rflip(n = N, prob = p.i))

# Infizierte mit Symptomen
n.s.i <- as.numeric(rflip(n = n.i, prob = p.s.i))

# Infizierte ohne Symptome
n.os.i <- n.i - n.s.i

##################
# Nicht Infizierte
n.oi <- N - n.i

# Nicht-Infizierte mit Symptomen
n.s.oi <- as.numeric(rflip(n = n.oi, prob = p.s.oi))

n.os.oi <- n.oi - n.s.oi

# Vektoren
Infektion <- factor(rep(c("Infiziert", "Nicht Infiziert"), times = c(n.i, n.oi)))
Symptome <- factor(c(rep(c("Symptome", "Keine Symptome"), times = c(n.s.i, n.os.i)), 
                     rep(c("Symptome", "Keine Symptome"), times = c(n.s.oi, n.os.oi))))

# Datensatz
Epi <- data.frame(Infektion, Symptome)

############
# Kennzahlen
############

#####################
# Unbedingte Analysen

# Absolute Häufigkeiten: Infektionen
tally( ~ Infektion, data = Epi)

# Anteil Infizierter
prop( ~ Infektion, success = "Infiziert", data= Epi)

# Absolute Häufigkeiten: Symptome
tally( ~ Symptome, data = Epi)

# Anteil Infizierter
prop( ~ Symptome, success = "Symptome", data= Epi)

###################
# Bedingte Analysen

##################################################
# Absolute Häufigkeiten: Infektionen bei Symptomen
tally( ~ Infektion | Symptome, data = Epi)

# Relative Häufigkeiten: Infektionen bei Symptomen
tally( ~ Infektion | Symptome, data = Epi, format = "proportion")

# Bedingen ist wie "filter": Wenn ich Symptome habe, wie hoch ist dann die W.keit, dass ich infiziert bin?
Epi %>%
  filter(Symptome == "Symptome") %>%
  prop( ~ Infektion, success = "Infiziert", data = .)

#################################################
# Absolute Häufigkeiten: Symptome bei Infektionen
tally( ~ Symptome | Infektion, data = Epi)

# Relative Häufigkeiten: Symptome bei Infektionen
tally( ~ Symptome | Infektion, data = Epi, format = "proportion")

# Bedingen ist wie "filter": Wenn ich infiziert bin habe, wie hoch ist dann die W.keit, dass ich Symotome habe?
Epi %>%
  filter(Infektion == "Infiziert") %>%
  prop( ~ Symptome, success = "Symptome", data = .)


###########
# Ergebnis:
# Die (unbedingte) Wahrscheinlichkeit infiziert zu sein
# unterscheidet sich i.d.R. 
# von der Wahrscheinlichkeit infiziert zu sein, wenn man Symptome hat
# und
# von der Wahrscheinlichkeit Symptome zu haben, wenn man infiziert ist
#
# Mathematisch: i.A. P(A) ungleich P(A|B) ungleich P(B|A)

