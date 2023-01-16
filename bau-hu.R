# Welche baulichen Maßnahmen an der HU könnten am meisten Nicht-Radfahrer
# dazu motivieren, auf das Fahrrad umzusteigen?

library(dplyr)
library(ggplot2)

# relevante Zielgruppe: Nichtradfahrer, die von den eigenen Fähigkeiten (C1.) 
# und der eigenen Ausstattung (C2.) her Fahrrad fahren könnten und die auch 
# von den “weiteren Faktoren” (C5) nicht abgehalten werden

non_bike <- read.csv("non_bike.csv")
# crit_col <- c(9)
# crit_col <- c(8,9,10,11,12,13,14,15,16,32,33,34,35)
# Ohne alle diese Leute bleiben zu wenige übrig. Schärfere Kriterien:
crit_col <- c(11,33)
# Nur Entfernung und körperliche Einschränkung
pot_bike <- non_bike

for (i in crit_col)
{
  pot_bike <- pot_bike[pot_bike[i] == "Trifft nicht zu" | 
                         pot_bike[i] == "Trifft eher nicht zu", 
                      ]
  }

# Gruppierung nach Campi: Spalte 4

# rename relevant columns with keywords
colnames(pot_bike)[18] <- 'Spinde'
colnames(pot_bike)[19] <- 'Diebstahl'
colnames(pot_bike)[20] <- 'Dach'
colnames(pot_bike)[21] <- 'Dusche'
colnames(pot_bike)[22] <- 'Ladestation'

adl <- pot_bike[pot_bike[3] == "Adlershof",]
ms <- pot_bike[pot_bike[3] == "Süd/Mitte",]
no <- pot_bike[pot_bike[3] == "Nord",]

count_answers <- function(campus) {
  counts <- data.frame("Spinde" = table(campus$Spinde), "Diebstahl" = table(campus$Diebstahl),
                           "Dach" = table(campus$Dach), "Dusche" = table(campus$Dusche),
                           "Ladestation" = table(campus$Ladestation))
  # table(...) erzeugt den Index immer mit, deshalb werden überflüssige
  # Index-Spalten hier entfernt:
  counts <- counts %>% select(-one_of('Diebstahl.Var1', 'Dach.Var1', 'Dusche.Var1', 'Ladestation.Var1')) 
  counts <- counts[c(3, 1, 5, 2, 4), ] # Bewertungsskala in sinnvolle Reihenfolge bringen
  rownames(counts) <- counts[ ,1]
  counts <- as.data.frame(t(counts))
  counts <- counts[-c(1), ]
  counts <- cbind(Maßnahme = rownames(counts), counts)
  rownames(counts) <- 1:nrow(counts)
  return(counts)
  }

# Anpassung des Formats zum Plotten:
adl_counts <- count_answers(adl)

adl_counts$campus <- rep("Adlershof",nrow(adl_counts))
ms_counts <- count_answers(ms)
ms_counts$campus <- rep("Mitte",nrow(ms_counts))
no_counts <- count_answers(no)
no_counts$campus <- rep("Nord",nrow(no_counts))

counts_joined = rbind(adl_counts, ms_counts, no_counts)
colnames(counts_joined)[6] <- 'Trifft_zu'
colnames(counts_joined)[5] <- 'Trifft_eher_zu'

counts_joined$Trifft_zu <- as.numeric(counts_joined$Trifft_zu)
counts_joined$Trifft_eher_zu <- as.numeric(counts_joined$Trifft_eher_zu)

counts_joined$positiv <- counts_joined$Trifft_zu + counts_joined$Trifft_eher_zu

# Plotten:
ggplot(counts_joined, aes(fill=Maßnahme, y=positiv, x=campus)) + 
  geom_bar(position='dodge', stat='identity')
