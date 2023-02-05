# plots a horizontal stacked bar plot, with the possibility to select campus and
# status group

library(stringr)
# library(tidyverse)
library(plyr)
library(forcats)
library(ggplot2)


count_answers <- function(df, group="all", campus="all", status="all") {
  if ("group" %in% colnames(df)){
    if (group != "all"){
      df <- df[df$group == group, ]
    }
  }
  if (campus != "all"){
    df <- df[df$Campus == campus, ]
  }
  if (status != "all"){
    if (status == "Wimis"){
      df <- df[df$Status == "Hochschullehrende und akademische Mitarbeiter:innen inkl. Promovierende", ]
    }
    else if (status == "Somis"){
      df <- df[df$Status == "Mitarbeiter:innen für Service und Technik", ]
    }
    else if (status == "Studis"){
      df <- df[df$Status == "Studierende", ]
    }
  }
  stripped_data <- df[ , ! names(df) %in% c("Status", "Campus", "group")]
  counts <- ldply( stripped_data, function(x) data.frame( table(x), prop.table( table(x) ) )  )
  counts %>%
    mutate(x = fct_relevel(x, "Trifft nicht zu", "Trifft eher nicht zu", "Weiß nicht", "Trifft eher zu", "Trifft zu"),
           #         .id = fct_reorder(.id, val)
    ) %>%
    ggplot( aes(x=.id ,y=Freq, fill=x)) +
    geom_col() + 
    coord_flip() +
    labs(x="Motivationsfaktoren", y="Anzahl Antworten (Nicht-Radfahrer:innen)") +
    scale_fill_manual(values=c("#949494","#A8A8A8", "#BABABA", "#E58A19", "#E55C19"))
 # return(df)
}

### Benutzung der Funktion mit dem gesamt-DataFrame:
gesamt <- read.csv("preprocessed/gesamt.csv")

colnames(gesamt)[2] <- 'Status'
colnames(gesamt)[3] <- 'Campus'

# filter relevant columns: status (2), campus (3), group (40)
# and questions about the city (21 - 29)
stadt <- gesamt[, c(2,3,21:29,40)]

# Prefix "stadt_" in den Spaltennamen entfernen, das ist jetzt eh klar
colnames(stadt)[3:11] = sapply(strsplit(as.character(colnames(stadt)[3:11]), "_"), `[`, 2)

# count_answers(stadt, group = "bike", campus = "all", status = "all")


# gesamt-Dataframe enthält nicht die Variablen, die nur für eine der Gruppen bike oder non-nike abgefragt wurden

bike <- read.csv("preprocessed/bike_cleaned.csv")

bike_stadt <- bike[, c(3,4,14:24)]
bike_motivation <- bike[, c(3,4,40:48)]
count_answers(bike_motivation, group = "bike", campus = "all", status = "all")


non_bike <- read.csv("preprocessed/non_bike_cleaned.csv")

non_bike_stadt <- non_bike[, c(3,4,24:33)]
non_bike_selbst <- non_bike[, c(3,4,9:18,34:37)]
non_bike_motivation <- non_bike[, c(3,4,38:46)]
# count_answers(non_bike_motivation, group = "non_bike", campus = "all", status = "all")
