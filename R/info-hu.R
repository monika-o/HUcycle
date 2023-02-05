library(stringr)
#library(tidyverse)
library(plyr)
library(forcats)
library(ggplot2)


bike <- read.csv("preprocessed/bike_cleaned.csv")

# filter relevant columns: campus (4), status (3) and questions about offers (25 - 37)
bike <- bike[, c(3,4,25:37)]


bike_pure <- bike[ , ! names(bike) %in% c("Status", "Campus")]
counts <- ldply( bike_pure, function(x) data.frame( table(x), prop.table( table(x) ) )  )

counts %>%
  mutate(x = fct_relevel(x, "Trifft nicht zu", "Trifft eher nicht zu", "Weiß nicht", "Trifft eher zu", "Trifft zu"),
#         .id = fct_reorder(.id, val)
         ) %>%
  ggplot( aes(x=.id ,y=Freq.1, fill=x)) +
  geom_col() + 
  coord_flip() +
  labs(x="Angebot", y="Prozent der Befragten (Radfahrer*innen)") +
  scale_fill_manual(values=c("#949494","#A8A8A8", "#BABABA", "#E55C19", "#E58A19"))