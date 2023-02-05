# Welche baulichen Maßnahmen an der HU könnten am meisten Nicht-Radfahrer
# dazu motivieren, auf das Fahrrad umzusteigen?

library(dplyr)
library(ggplot2)
library("reshape2")
# library(tidyverse)

# relevante Zielgruppe: Nichtradfahrer, die von den eigenen Fähigkeiten (C1.) 
# und der eigenen Ausstattung (C2.) her Fahrrad fahren könnten und die auch 
# von den “weiteren Faktoren” (C5) nicht abgehalten werden

non_bike <- read.csv("preprocessed/non_bike_cleaned.csv")
# crit_col <- c(9)
# crit_col <- c(8,9,10,11,12,13,14,15,16,32,33,34,35)
# Ohne alle diese Leute bleiben zu wenige übrig. Schärfere Kriterien:
crit_col <- c("selbst_zu_weit", "selbst_körperlicheEinschränkung")
# Nur Entfernung und körperliche Einschränkung
pot_bike <- non_bike

for (i in crit_col)
{
  pot_bike <- pot_bike[pot_bike[i] == "Trifft nicht zu" | 
                         pot_bike[i] == "Trifft eher nicht zu", 
                      ]
  }



count_answers <- function(campusdf) {
  counts <- data.frame("Spinde" = table(campusdf$HU_Spinde), "Diebstahl" = table(campusdf$HU_Diebstahl),
                           "Dach" = table(campusdf$HU_Dach), "Dusche" = table(campusdf$HU_Dusche),
                           "Ladestation" = table(campusdf$HU_Ladestation))
  # table(...) erzeugt den Index immer mit, deshalb werden überflüssige
  # Index-Spalten hier entfernt:
  counts <- counts %>% select(-one_of('Diebstahl.Var1', 'Dach.Var1', 'Dusche.Var1', 'Ladestation.Var1')) 
  counts <- counts[c(3, 1, 5, 2, 4), ] # Bewertungsskala in sinnvolle Reihenfolge bringen
  rownames(counts) <- counts[ ,1]
  counts <- as.data.frame(t(counts))
  counts <- counts[-c(1), ]
  counts <- cbind(Maßnahme = rownames(counts), counts)
  rownames(counts) <- 1:nrow(counts)
  counts$Maßnahme <- as.character(counts$Maßnahme)
  counts$Maßnahme <- sapply(strsplit(as.character(counts$Maßnahme), ".F"), `[`, 1)
  
  return(counts)
  }


adl <- pot_bike[pot_bike["Campus"] == "Adlershof",]
ms <- pot_bike[pot_bike["Campus"] == "Süd/Mitte",]
no <- pot_bike[pot_bike["Campus"] == "Nord",]

adl_counts <- count_answers(adl)
adl_counts$campus <- rep("Adlershof",nrow(adl_counts))
ms_counts <- count_answers(ms)
ms_counts$campus <- rep("Mitte",nrow(ms_counts))
no_counts <- count_answers(no)
no_counts$campus <- rep("Nord",nrow(no_counts))

counts_joined = rbind(adl_counts, ms_counts, no_counts)


# counts_joined$positiv <- counts_joined$Trifft_zu + counts_joined$Trifft_eher_zu

# aggregoert positiv plotten:
# ggplot(counts_joined, aes(fill=Maßnahme, y=positiv, x=campus)) + 
#  geom_bar(position='dodge', stat='identity')


data_long1 <- melt(counts_joined,
                   id.vars = c("Maßnahme", "campus"))

data_long1$value <- as.numeric(data_long1$value)

ggplot(data_long1, aes(x=Maßnahme, y=value, fill=variable) ) + 
  geom_col() + facet_grid(.~campus) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values=c("#949494","#A8A8A8", "#BABABA", "#E55C19", "#E58A19"))

