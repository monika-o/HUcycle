library(stringr)
#library(tidyverse)
library(plyr)
library(forcats)


bike <- read.csv("preprocessed/bike.csv")

# filter relevant columns: campus (3), status (2) and questions about pffers (24 - 48)
bike <- bike[, c(2,3,24:48)]


# Remove first sentence from answer variables
colnames(bike)[3:27] = sapply(strsplit(as.character(colnames(bike)[3:27]), "gestalten..."), `[`, 2)

colname_dict <- c(
  'Zu.welcher.Gruppe.an.der.HU.gehören.Sie.' = 'Status',
  'An.welchem.Campus.sind.Sie.am.häufigsten...Für.genauere.Informationen.bezüglich.der.Einordnung.der.Campi..schauen.Sie.bitte.hier..Nord..Süd.Mitte..Adlershof..' = 'Campus',
  'Es.sollte.einen.Verleih.von.Lastenrädern.geben..da.ich.häufig.Gepäck.mitnehme..das.unpraktisch.oder.zu.schwer.zu.transportieren.ist..' = 'Verleih_Lastenrad',
  'Ich.brauche.Zugang.zu.einer.Fahrrad.Selbsthilfewerkstatt.mit.professioneller.Unterstützung..' = 'Zugang_Werkstatt',
  'Es.sollte.Angebote.für.Bike.Sharing...Leih.Fahrräder.geben.' = 'Bike_Sharing',
  'Ich.würde.gerne.an.einem.Sicherheitstraining.für.Fahradfahrer.innen.teilnehmen..' = 'Sicherheitstraining',
  'Es.sollte.Informationsangebote.für.fahrradfreundliche.Routen.rund.um.den.Campus.und.in.Berlin.geben..' = 'Info_Routen',
  'Es.sollte.einen.Umgebungsplan.mit.Informationen.zu.Fahrradstellplätzen.auf.dem.Campus.geben...' = 'Umgebungsplan',
  'Es.sollte.Informationsangebote.zum.richtigen.Verhalten.im.Straßenverkehr.geben..' = 'Info_Verhalten',
  'Es.sollte.Kurse.geben..in.denen.man.das.Fahrradfahren.erlernen.und.verbessern.kann...' = 'Fahrradkurs',
  'Es.sollte.Informationsangebote.zu.Neuerungen.im.Radverkehrsnetz.in.Berlin.und.auf.dem.Campus.geben..' = 'Info_Neuerungen_Netz',
  'Es.sollte.Informationsangebote.zu.Fahrradreparaturmöglichkeiten.und.Selbsthilfewerkstätten.an.der.HU.geben...' = 'Info_Werkstatt',
  'Es.sollten.Tipps.zum.Fahrradkauf.in.Berlin.bereitgestellt.werden..' = 'Tipps_Fahrradkauf',
  'Es.sollten.Informationen.zu.Fahrradkampagnen.und..aktionen.in.Berlin..z.B..Stadtradeln..bereitgestellt.werden...' = 'Info_Kampagnen',
  'Es.sollte.Angebote.für.Dienstfahrräder.geben..' = 'Diensträder'
)

for (i in colnames(bike)){
  print(i)
  j = gsub('.1', '', i)
#  print(j)
  if (endsWith(i, '1') == TRUE)
    {
   # print(colname_dict[j])
    colnames(bike)[colnames(bike) == i] = str_replace(i, i, paste(colname_dict[j], "1", sep="_"))
  }
  else {
    print(j)
    #print(colname_dict[i])
    colnames(bike)[colnames(bike) == i] = str_replace(i, i, colname_dict[i])
    
  }
}


# joinen von Spalten, die die gleichen Maßnahmen betreffen

for (i in colnames(bike)){
  if (endsWith(i, '1') == TRUE)
  {
    j = gsub('_1', '', i)
    bike[, j][!is.na(bike[,i])] = bike[,i][!is.na(bike[,i])]
    bike <- bike[ , ! names(bike) %in% c(i)]      
  }
}

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