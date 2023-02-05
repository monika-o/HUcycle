# Welche baulichen Maßnahmen an der HU könnten am meisten Nicht-Radfahrer
# dazu motivieren, auf das Fahrrad umzusteigen?

library(plyr)
library(ggplot2)
library("reshape2")
# library(tidyverse)


count_answers <- function(campusdf, variablelist) {
  campusdf_filtered <- campusdf[ , names(campusdf) %in% variablelist]
  counted <- ldply( campusdf_filtered, function(x) data.frame( table(x) )  )

  colnames(counted)[1] <- "Maßnahme"
  colnames(counted)[2] <- "variable"
  colnames(counted)[3] <- "value"
  return(counted)
  }

plot_grouped_stacked_barplot <- function(df, variablelist){
  # The df must contain the variables HU_Spinde, HU_Diebstahl, HU_Dach, HU_Dusche and HU_Ladestation
  # for the 3 different campi. The campi must be named in a column called "Campus".
  
  adl <- df[df["Campus"] == "Adlershof",]
  ms <- df[df["Campus"] == "Süd/Mitte",]
  no <- df[df["Campus"] == "Nord",]

  adl_counts <- count_answers(adl, variablelist)
  adl_counts$campus <- rep("Adlershof",nrow(adl_counts))
  ms_counts <- count_answers(ms, variablelist)
  ms_counts$campus <- rep("Mitte",nrow(ms_counts))
  no_counts <- count_answers(no, variablelist)
  no_counts$campus <- rep("Nord",nrow(no_counts))

  counts_joined = rbind(adl_counts, ms_counts, no_counts)

  counts_joined %>%
    mutate(variable = fct_relevel(variable, "Trifft nicht zu", "Trifft eher nicht zu", "Weiß nicht", "Trifft eher zu", "Trifft zu"),
  #         .id = fct_reorder(.id, val)
  ) %>%
  ggplot(counts_joined, mapping = aes(x=Maßnahme, y=value, fill=variable) ) + 
    geom_col() + facet_grid(.~campus) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_fill_manual(values=c("#949494","#A8A8A8", "#BABABA", "#E58A19", "#E55C19"))
  }


### Vorbereitung der Daten
# 1. relevante Zielgruppe: Nichtradfahrer, die von den eigenen Fähigkeiten (C1.) 
# und der eigenen Ausstattung (C2.) her Fahrrad fahren könnten und die auch 
# von den “weiteren Faktoren” (C5) nicht abgehalten werden.
# Problem: Dabei bleiben zu wenige übrig; deshalb habe ich die Kriterien auf
# die Entfernung und körperliche Einschränkungen reduziert.

non_bike <- read.csv("preprocessed/non_bike_cleaned.csv")
crit_col <- c("selbst_zu_weit", "selbst_körperlicheEinschränkung")
pot_bike <- non_bike

for (i in crit_col)
{
  pot_bike <- pot_bike[pot_bike[i] == "Trifft nicht zu" | 
                         pot_bike[i] == "Trifft eher nicht zu", 
  ]
}

# 2. relevante Zielgruppe: Radfahrer*innen

bike <- read.csv("preprocessed/bike_cleaned.csv")


### Verarbeitung und Plotten mittels der obigen Funktionen
# variablelist <- c("Diensträder", "Info_Werkstatt", "Umgebungsplan")
variablelist <- c("HU_Dach", "HU_Diebstahl", "HU_Duschen",  "HU_Ladestation", "HU_Spinde")
plot_grouped_stacked_barplot(pot_bike, variablelist)

# plot_grouped_stacked_barplot(bike, variablelist)
