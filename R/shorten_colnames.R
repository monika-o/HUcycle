library(stringr)


bike <- read.csv("preprocessed/bike.csv")
non_bike <- read.csv("preprocessed/non_bike.csv")

colnames(bike)[8:50] = sapply(strsplit(as.character(colnames(bike)[8:50]), "gestalten..."), `[`, 2)
colnames(bike)[51:59] = sapply(strsplit(as.character(colnames(bike)[51:59]), "Fahrradfahren..."), `[`, 2)

colnames(non_bike)[8:36] = sapply(strsplit(as.character(colnames(non_bike)[8:36]), "öfter.mit.dem.Fahrrad.zur.Uni.zu.fahren..."), `[`, 2)
colnames(non_bike)[37:45] = sapply(strsplit(as.character(colnames(non_bike)[37:45]), "motivieren..."), `[`, 2)

colname_dict <- c(
  'Zu.welcher.Gruppe.an.der.HU.gehören.Sie.' = 'Status',
  'An.welchem.Campus.sind.Sie.am.häufigsten...Für.genauere.Informationen.bezüglich.der.Einordnung.der.Campi..schauen.Sie.bitte.hier..Nord..Süd.Mitte..Adlershof..' = 'Campus',
  'An.welchem.Campus.sind.Sie.am.häufigsten...Für.genauere.Informationen.bezüglich.der.Einordnung.der.Campi..schauen.Sie.bitte.hier..Nord..Süd.Mitte..Adlershof....Sonstiges.' = 'Campus_Sonstiges',
  'Wie.weit.ist.Ihr.Hauptcampus.von.Ihrem.Wohnsitz.entfernt.' = 'Entfernung',
  'Wie.wahrscheinlich.wäre.es.auf.einer.Skala.von.0.bis.10..dass.Sie.bei.perfektem.Wetter..z.B..regen..und.windfreier.Frühlingstag..mit.dem.Fahrrad.zu.Ihrem.Campus.fahren....' = 'WahrscheinlichkeitWetter',
  'Was.ist.Ihr.Hauptverkehrsmittel.auf.dem.Weg.zur.HU...Als.Hauptverkehrsmittel.verstehen.wir.hier.das.Verkehrsmittel..welches.Sie.mindestens.die.Hälfte.der.Zeit.auf.Ihrem.Weg.zur.Uni.nutzen...' = 'Hauptverkehrsmittel',
  'Es.sollte.mehr.Spinde.in.allen.Gebäuden.geben..da.ich.häufig.Gepäck.mitnehme..das.unpraktisch.oder.zu.schwer.zu.transportieren.ist..' = 'HU_Spinde',
  'Es.sollte.mehr.diebstahlgeschützte.Radabstellplätze.an.der.Uni.geben..' = 'HU_Diebstahl',
  'Es.sollte.mehr.überdachte.Radabstellplätze.an.der.Uni.geben..' = 'HU_Dach',
  'Es.sollte.mehr.Duschmöglichkeiten.in.der.Uni.geben..' = 'HU_Dusche',
  'Es.sollte.Aufladestationen.für.eigene.E.Bike.Akkus.an.der.Uni.geben...' = "HU_Ladestation",
  '.Es.sollte.mehr.Radwege.auf.der.Strecke.zur.Uni.geben..' = 'Stadt_mehrRadwege',
  '.Im.Winter.sollten.die.Radwege.besser.geräumt.sein..' = 'Stadt_bessereRäumungWinter',
  '.Die.Radwege.sollten.besser.beleuchtet.sein...' = 'Stadt_bessereBeleuchtung',
  '.Die.Radwege.sollten.weniger.durch.Autos.behindert.sein..' = 'Stadt_wenigerAutobehinderung',
  '.Es.sollte.mehr.von.der.Straße.getrennte.Radwege.geben..' = 'Stadt_separateRadwege',
  '.Es.sollte.mehr.Platz.auf.den.Radwegen.sein..damit.sich.Radfahrer.innen.nicht.gegenseitig.behindern..' = 'Stadt_mehrPlatzRadwege',
  '.Es.sollte.mehr.diebstahlgeschützte.Radabstellplätze.in.der.Stadt.geben..' = 'Stadt_Diebstahl',
  '.Es.sollte.mehr.überdachte.Radabstellplätze.in.der.Stadt.geben..' = 'Stadt_überdacht',
  '.Die.Strecke.von.Zuhause.zur.Uni.sollte.weniger.Unterbrechungen..z.B..Ampeln..Überqueren.verkehrsreicher.Straßen..haben..' = 'Stadt_wenigerUnterbrechung',
  '.Es.sollte.Räum..und.Streudienste.ab.dem.frühen.Morgen.überall.geben..wo.Radfahrer.innen.fahren..insbesondere.bei.schlechten.Wetterverhältnissen...' = 'Stadt_bessereRäumung',
  '.Es.sollte.genug.Aufladestationen.für.eigene.E.Bike.Akkus.in.der.Stadt.geben..' = 'Stadt_Ladestation',
  'Ich.müsste.bei.schlechten.Wetterverhältnissen.besser.ausgestattet.sein..' = 'AusstattungWetter',
  'Ich.müsste.mehr.Zeit.haben...' = 'Zeit',
  'So.komme.ich.schneller.zur.Uni...' = 'motivation_schneller',
  'So.treibe.ich.mehr.Sport.im.Alltag...' = 'motivation_Sport',
  'So.verbringe.ich.mehr.Zeit.an.der.frischen.Luft..' = 'motivation_Luft',
  'So.bin.ich.flexibler.und.unabhängiger.unterwegs..' = 'motivation_flexibel',
  'So.tue.ich.etwas..um.die.Umwelt.zu.schützen..' = 'motivation_Umwelt',
  'So.spare.ich.Geld..' = 'motivation_Geld',
  'So.habe.ich.mehr.Spaß.oder.eine.Abwechslung.vom.Alltag..während.ich.mich.fortbewege..' = 'motivation_Abwechslung',
  'So.kann.ich.ein.Vorbild.sein..' = 'motivation_Vorbild',
  'So.lerne.ich.meine.Umgebung.besser.kennen..' = 'motivation_UmgebungKennenlernen',
  'Welchem.Geschlecht.ordnen.Sie.sich.zu.' = 'Geschlecht',
  'Welchem.Geschlecht.ordnen.Sie.sich.zu...Sonstiges.' = 'Geschlecht_Sonstiges',
  'Leben.Sie.gemeinsam.mit.einem.Kind.im.Haushalt..für.das.Sie.Verantwortung.tragen.' = 'Kind',
  # ab hier spezifisch für non_bike
  'Ich.kann.nicht.Fahrrad.fahren...' = 'selbst_Können',
  'Mit.dem.Fahrrad.zur.Uni.zu.fahren.ist.für.mich.körperlich.zu.anstrengend..' = 'selbst_anstrengend',
  'Ich.traue.es.mir.aufgrund.meiner.Fähigkeiten.und.oder.Kenntnisse..z.B..von.Verkehrsregeln..nicht.zu...' = 'selbst_Kenntnisse',
  'Die.Strecke.zur.Uni.ist.mir.zu.weit...' = 'selbst_zu_weit',
  'Ich.habe.kein.Fahrrad..' = 'selbst_kein_Fahrrad',
  'Mein.Fahrrad.ist.nicht.funktionstüchtig..' = 'selbst_kaputtes_Fahrrad',
  'Ich.muss.Gepäck.mitnehmen..das.unpraktisch.oder.zu.schwer.zu.transportieren.ist...' = 'selbst_Gepäck',
  'Meine.Kleidung.eignet.sich.nicht.zu.Fahrradfahren..' = 'selbst_Kleidung',
  'Ich.kann.mir.kein.Fahrrad.leisten..' = 'selbst_Fahrrad_zu_teuer',
  'Ich.kann.mir.die.Ausstattung.zum.Fahrradfahren.bei.kalten.oder.nassen.Wetterverhältnissen.nicht.leisten..' = 'selbst_schlechtWetter_zu_teuer',
  'Es.gibt.nicht.genügend.Spinde.in.der.Uni..um.mein.Gepäck.zu.verstauen..' = 'HU_Spinde',
  'Es.gibt.nicht.genügend.diebstahlgeschützte.Radabstellplätze.an.der.Uni..' = 'HU_Diebstahl',
  'Es.gibt.nicht.genügend.überdachte.Radabstellplätze.an.der.Uni...' = 'HU_Dach',
  'Mir.fehlen.Duschmöglichkeiten.an.der.Uni..' = 'HU_Duschen',
  'Es.gibt.keine.Aufladestationen.für.eigene.E.Bike.Akkus.an.der.Uni..' = 'HU_Ladestation',
  '.Die.Strecke.zur.Uni.beinhaltet.Stellen..die.ich.als.zu.gefährlich.einstufe..' = 'Stadt_Gefahr',
  '.Es.gibt.zu.wenige.Radwege..' = 'Stadt_mehrRadwege',
  '.Die.Radwege.sind.mir.zu.eng..und.ich.fühle.mich.durch.andere.Radfahrer.innen.behindert..' = 'Stadt_mehrPlatzRadwege',
  '.Im.Winter.sind.die.vorhandenen.Radwege.nicht.geräumt..' = 'Stadt_bessereRäumung',
  '.Die.Radwege.sind.schlecht.beleuchtet..' = 'Stadt_bessereBeleuchtung',
  '.Es.gibt.nicht.genügend.diebstahlgeschützte.Radabstellplätze.in.der.Stadt..' = 'Stadt_Diebstahl',
  '.Es.gibt.nicht.genügend.überdachte.Radabstellplätze.in.der.Stadt...' = 'Stadt_Dach',
  '.Autos.behindern.meinen.Weg..z.B..parken.auf.dem.Radweg...' = 'Stadt_wenigerAutobehinderung',
  '.Die.Strecke.von.Zuhause.zur.Uni.beinhaltet.zu.viele.Unterbrechungen..z.B..Ampeln..Überqueren.verkehrsreicher.Straßen...' = 'Stadt_wenigerUnterbrechung',
  '.Es.gibt.nicht.genügend.Aufladestationen.für.eigene.E.Bike.Akkus.in.der.Stadt...' = 'Stadt_Ladestation',
  '.Ich.habe.körperliche.Einschränkungen..die.es.mir.nicht.erlauben..mit.dem.Fahrrad.zur.HU.zu.fahren..' = 'selbst_körperlicheEinschränkung',
  '.Mir.fehlen.das.Interesse.und.die.Lust..' = 'selbst_Lust',
  '.Ich.finde.keine.Zeit..mit.dem.Fahrrad.zur.Uni.zu.fahren..' = 'selbst_keineZeit',
  '.Ich.fahre.bei.schlechtem.Wetter.nicht.Fahrrad...' = 'selbst_schlechtesWetter',
  'Wenn.ich.so.schneller.zur.Uni.käme..' = 'motivation_schneller',
  'Wenn.ich.so.mehr.Sport.im.Alltag.treiben.würde..' = 'motivation_Sport',
  'Wenn.ich.so.mehr.Zeit.an.der.frischen.Luft.verbringen.würde..' = 'motivation_Luft',
  'Wenn.ich.so.flexibler.und.unabhängiger.unterwegs.wäre...' = 'motivation_flexibel',
  'Wenn.ich.so.etwas.für.die.Umwelt.tun.kann..' = 'motivation_Umwelt',
  'Wenn.ich.so.mehr.Geld.spare...' = 'motivation_Geld',
  'Wenn.ich.so.mehr.Spaß.oder.eine.Abwechslung.vom.Alltag.habe..während.ich.mich.fortbewege..' = 'motivation_Abwechslung',
  'Wenn.ich.so.ein.Vorbild.sein.kann...' = 'motivation_Vorbild',
  'Wenn.ich.so.meine.Umgebung.besser.kennenlernen.kann..' = 'motivation_UmgebungKennenlernen'
  
)

for (i in colnames(bike)){
  if (i %in% names(colname_dict)){
    colnames(bike)[colnames(bike) == i] = str_replace(i, i, colname_dict[i])
  }
}

# Für Angebote / Infos gibt es jeweils immer zwei Spalten für die gleiche Variable, wobei eine davon mit dem Suffix "1" versehen ist.
# Im Folgenden werden beide zusammengeführt.

info_colname_dict <- c(
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
    colnames(bike)[colnames(bike) == i] = str_replace(i, i, paste(info_colname_dict[j], "1", sep="_"))
  }
  else {
    print(j)
    #print(colname_dict[i])
    if (i %in% names(info_colname_dict)){
      colnames(bike)[colnames(bike) == i] = str_replace(i, i, info_colname_dict[i])
    }
    
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

### non_bike

for (i in colnames(non_bike)){
  if (i %in% names(colname_dict)){
    colnames(non_bike)[colnames(non_bike) == i] = str_replace(i, i, colname_dict[i])
  }
}

write.csv(bike, "preprocessed/bike_cleaned.csv", row.names=TRUE)
write.csv(non_bike, "preprocessed/non_bike_cleaned.csv", row.names=TRUE)

