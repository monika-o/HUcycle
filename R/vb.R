library(ggplot2)
data<-read.csv2("Umfragedaten.csv",sep=",")
data<-data[data[,3] =="32", ] # benutze nur vollständig ausgefüllte Fragebögen
data<-data[is.na(data[,11])==FALSE | is.na(data[,16])==FALSE, ] # entferne mysteriöse NA-Zeilen 
dat_work <- data[,-c(1:7)] # entferne irrelevante Spalten, erstelle Arbeitsdatensatz datwork

dat_work$Fak = dat_work[,9]  #führe Fakultät zusammen
dat_work$Fak[dat_work[,4]!=""] = dat_work[,4][dat_work[,4]!=""]  

dat_work$SoWi = dat_work[,10]  # führe Institute von SoWi zusammen
dat_work$SoWi[dat_work[,5]!=""] = dat_work[,5][dat_work[,5]!=""]  

dat_work$Philo = dat_work[,11]  
dat_work$Philo[dat_work[,6]!=""] = dat_work[,6][dat_work[,6]!=""] 

dat_work$Nawi = dat_work[,12]  
dat_work$Nawi[dat_work[,7]!=""] = dat_work[,7][dat_work[,7]!=""] 

dat_work$Life = dat_work[,13]  
dat_work$Life[dat_work[,8]!=""] = dat_work[,8][dat_work[,8]!=""]  

rows_slf <- c(495,498,430,439,463,429,441,446,449,470,471,473,479,480,483,489) # Zeilenindizes von Menschen von der SLF-Fak
rows_chari<-c(66,85,98,103,370) # Zeilenindizes von Menschen von der Charite
dat_work[rownames(dat_work) %in% rows_slf,122 ] <-"Sprach-und Literaturwissenschaftliche Fakultät" # erstelle SLF als mögliche Fakultät
dat_work[rownames(dat_work) %in% rows_chari,122]<-"Charite" # erstelle Charite als mögliche Fakultät
dat_work[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,25,118,119,120,122,123,124,125,126)] <- lapply(dat_work[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,25,118,119,120,122,123,124,125,126)], as.factor) # ändere Datentyp von Character in Faktor
dat_work[,14]<-factor(dat_work[,14],levels = c("< 5 km","5 - 10 km","11 - 15 km","16 - 20 km", "> 20 km"), ordered = TRUE) # ändere character typed Entfernungsvariable in geordnete Faktoren
dat_work[,15]<-factor(dat_work[,15],levels = 0:10, labels = as.character(0:10), ordered = TRUE) # ändere integer-Skalen in ordinal-skalen
dat_work[,27:116]<-lapply(dat_work[,c(27:116)], function(x) factor(x,levels = c("Trifft nicht zu","Trifft eher nicht zu","Weiß nicht","Trifft eher zu","Trifft zu"), ordered = TRUE)) # ändere von character in geordnete Skala


levels(dat_work[,122])[levels(dat_work[,122])=='Philosophie Fakultät']<-"Philosophische Fakultät" # Philosophie Fak = Philosophische Fak
levels(dat_work[,122])[levels(dat_work[,122])==''] <- NA # Führe bei den Institutsvariablen NAs ein, wenn Menschen, gar nicht an der Fakultät studieren 
levels(dat_work[,123])[levels(dat_work[,123])==''] <- NA
levels(dat_work[,124])[levels(dat_work[,124])==''] <- NA
levels(dat_work[,125])[levels(dat_work[,125])==''] <- NA
levels(dat_work[,126])[levels(dat_work[,126])==''] <- NA

levels(dat_work[,119])[levels(dat_work[,119])==''] <- NA # NAs für Geschlecht: Sonstiges (Freifeld)
levels(dat_work[,3])[levels(dat_work[,3])==''] <- NA # NAs für Fakultät: Andere (Freifeld)

dat_work<-dat_work[,-c(4:13,16:24)] #entferne ursprüngliche Fakultäts und Institutsvariablen


bike<-dat_work[factor(dat_work[,6])=="Fahrrad" |factor(dat_work[,6])=="Fahrrad und ÖPNV" ,c(1:6,37:97,99:101,103:107)] #erstelle Datensatz bike und inkludiere nur relevante Variablen
non_bike<-dat_work[factor(dat_work[,6])!="Fahrrad" & factor(dat_work[,6])!="Fahrrad und ÖPNV",c(1:6,8:36,80:88,99:101,103:107)] #erstelle Datensatz non_bike und inkludiere nur relevante Variablen



####joinen für gesamtdatensatz (führe Variablen zusammen, die die gleichen Maßnamen betreffen)

##HU
dat_work$HU_Spinde<-dat_work[,18]  
dat_work$HU_Spinde[!is.na(dat_work[,37])] = dat_work[,37][!is.na(dat_work[,37])]  

dat_work$HU_diebstahlsicher<-dat_work[,19]  
dat_work$HU_diebstahlsicher[!is.na(dat_work[,38])] = dat_work[,38][!is.na(dat_work[,38])]  


dat_work$HU_ueberdacht<-dat_work[,20]  
dat_work$HU_ueberdacht[!is.na(dat_work[,39])] = dat_work[,39][!is.na(dat_work[,39])]  

dat_work$HU_duschen<-dat_work[,21]  
dat_work$HU_duschen[!is.na(dat_work[,40])] = dat_work[,40][!is.na(dat_work[,40])]  

dat_work$HU_aufladen<-dat_work[,22]  
dat_work$HU_aufladen[!is.na(dat_work[,41])] = dat_work[,41][!is.na(dat_work[,41])]  

##STADT

dat_work$stadt_mehrRadwege<-dat_work[,24]  
dat_work$stadt_mehrRadwege[!is.na(dat_work[,42])] = dat_work[,42][!is.na(dat_work[,42])]  

dat_work$stadt_bessereRäumung<-dat_work[,26]  
dat_work$stadt_bessereRäumung[!is.na(dat_work[,43])] = dat_work[,43][!is.na(dat_work[,43])] 

dat_work$stadt_diebstahlsicher<-dat_work[,28]  
dat_work$stadt_diebstahlsicher[!is.na(dat_work[,48])] = dat_work[,48][!is.na(dat_work[,48])] 

dat_work$stadt_ueberdacht<-dat_work[,29]  
dat_work$stadt_ueberdacht[!is.na(dat_work[,49])] = dat_work[,49][!is.na(dat_work[,49])] 

dat_work$stadt_bessereBeleuchtung<-dat_work[,27]  
dat_work$stadt_bessereBeleuchtung[!is.na(dat_work[,44])] = dat_work[,44][!is.na(dat_work[,44])] 

dat_work$stadt_wenigerAutobehinderung<-dat_work[,30]  
dat_work$stadt_wenigerAutobehinderung[!is.na(dat_work[,45])] = dat_work[,45][!is.na(dat_work[,45])] 

dat_work$stadt_mehrPlatzRadwege<-dat_work[,25]  
dat_work$stadt_mehrPlatzRadwege[!is.na(dat_work[,47])] = dat_work[,47][!is.na(dat_work[,47])] 

dat_work$stadt_wenigerUnterbrechung<-dat_work[,31]  
dat_work$stadt_wenigerUnterbrechung[!is.na(dat_work[,50])] = dat_work[,50][!is.na(dat_work[,50])] 

dat_work$stadt_aufladen<-dat_work[,32]  
dat_work$stadt_aufladen[!is.na(dat_work[,52])] = dat_work[,52][!is.na(dat_work[,52])] 

##MOTIVATION

dat_work$motivation_schneller<-dat_work[,80]  
dat_work$motivation_schneller[!is.na(dat_work[,89])] = dat_work[,89][!is.na(dat_work[,89])] 

dat_work$motivation_sport<-dat_work[,81]  
dat_work$motivation_sport[!is.na(dat_work[,90])] = dat_work[,90][!is.na(dat_work[,90])]

dat_work$motivation_frischeLuft<-dat_work[,82]  
dat_work$motivation_frischeLuft[!is.na(dat_work[,91])] = dat_work[,91][!is.na(dat_work[,91])]

dat_work$motivation_flexibler<-dat_work[,83]  
dat_work$motivation_flexibler[!is.na(dat_work[,92])] = dat_work[,92][!is.na(dat_work[,92])]

dat_work$motivation_umwelt<-dat_work[,84]  
dat_work$motivation_umwelt[!is.na(dat_work[,93])] = dat_work[,93][!is.na(dat_work[,93])]

dat_work$motivation_geld<-dat_work[,85]  
dat_work$motivation_geld[!is.na(dat_work[,94])] = dat_work[,94][!is.na(dat_work[,94])]

dat_work$motivation_abwechslung<-dat_work[,86]  
dat_work$motivation_abwechslung[!is.na(dat_work[,95])] = dat_work[,95][!is.na(dat_work[,95])]

dat_work$motivation_vorbild<-dat_work[,87]  
dat_work$motivation_vorbild[!is.na(dat_work[,96])] = dat_work[,96][!is.na(dat_work[,96])]

dat_work$motivation_umgebungKennenlernen<-dat_work[,88]  
dat_work$motivation_umgebungKennenlernen[!is.na(dat_work[,97])] = dat_work[,97][!is.na(dat_work[,97])]

##WEITERES
dat_work$braucheMehrZeit<-dat_work[,35]  
dat_work$braucheMehrZeit[!is.na(dat_work[,79])] = dat_work[,79][!is.na(dat_work[,79])]





####joinen in bike (führt eigentlich gleiche Variablen zusammen, die aus irgendeinem Grund als zwei verschiedene gespeichert worden sind)

bike[!is.na(bike[,35]),23] = bike[,35][!is.na(bike[,35])]
bike[!is.na(bike[,36]),24] = bike[,36][!is.na(bike[,36])]
bike[!is.na(bike[,37]),25] = bike[,37][!is.na(bike[,37])]
bike[!is.na(bike[,38]),26] = bike[,38][!is.na(bike[,38])]
bike[!is.na(bike[,39]),27] = bike[,39][!is.na(bike[,39])]
bike[!is.na(bike[,40]),28] = bike[,40][!is.na(bike[,40])]
bike[!is.na(bike[,41]),29] = bike[,41][!is.na(bike[,41])]
bike[!is.na(bike[,42]),30] = bike[,42][!is.na(bike[,42])]
bike[!is.na(bike[,43]),31] = bike[,43][!is.na(bike[,43])]
bike[!is.na(bike[,44]),32] = bike[,44][!is.na(bike[,44])]
bike[!is.na(bike[,45]),33] = bike[,45][!is.na(bike[,45])]
bike[!is.na(bike[,46]),34] = bike[,46][!is.na(bike[,46])]

bike<-bike[,-c(35:46)] # entferne überflüssige Variablen aus bike
bike<-bike[,-c(38:46)] # entferne überflüssige Variablen aus bike
gesamt<-dat_work[,c(1:6,99:101,103:131)] # mache gesamt-datensatz mit allen Variablen, die alle Personen beantwortet haben

gesamt$group<-"bike" # füge gesamt datensatz variable "gruppe" mit bike und non_bike hinzu
gesamt$group[factor(gesamt[,6])!="Fahrrad"& factor(gesamt[,6])!="Fahrrad und ÖPNV"]<-"non_bike"
