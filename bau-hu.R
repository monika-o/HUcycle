# Welche baulichen Maßnahmen an der HU könnten am meisten Nicht-Radfahrer
# dazu motivieren, auf das Fahrrad umzusteigen?

# relevante Zielgruppe: Nichtradfahrer, die von den eigenen Fähigkeiten (C1.) 
# und der eigenen Ausstattung (C2.) her Fahrrad fahren könnten und die auch 
# von den “weiteren Faktoren” (C5) nicht abgehalten werden

non_bike <- read.csv("non_bike.csv")
# crit_col <- c(9)
# crit_col <- c(8,9,10,11,12,13,14,15,16,32,33,34,35)
# Ohne alle diese Leute bleiben nur 7 übrig. Schärfere Kriterien:
crit_col <- c(11,33)
# Nur Entfernung und körperliche Einschränkung
pot_bike <- non_bike

for (i in crit_col)
{
  pot_bike <- pot_bike[pot_bike[i] == "Trifft nicht zu" | 
                         pot_bike[i] == "Trifft eher nicht zu", 
                      ]
  }

