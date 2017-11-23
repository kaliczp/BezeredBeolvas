## Téli idõszámítás kilövéséhez kell
Sys.setenv(TZ='UTC')
library(xts)

## Kiindulási adatok Readme.R
beolvaso.zoo <- function(file, channel=1, object=NULL){
  require(zoo)
  if(is.null(object)){
    rec <- smartbe(file, channel=channel)
    print(file)
    out <- zoo(rec[,2],as.POSIXct(strptime(rec[,1], "%Y.%m.%d %T")))
    file <- file[-1]
  } else {
    out <- object
  }
  ## Zooba beolvasó fo ciklus. Létezo objektum esetén ez dolgozik.
  for(current.file in file){
    rec <- smartbe(current.file, channel=channel)
    print(current.file)
    rec.zoo <- zoo(rec[,2],as.POSIXct(strptime(rec[,1], "%Y.%m.%d %T")))
    out <- c(out,rec.zoo)
  }
  out
}

## Folyamatostámop zárójelentés Bükkhát alapján
smartbe <- function(file, channel=1) {
  headsep <- which(readLines(file) == "========================================================")
  channelsep <- which(readLines(file) == "--------------------------------------------------------")
  head.length <- 8
  if(channel == 1){
    ch.start <- headsep[1]+head.length
    ch.end <- channelsep[1]-(headsep[1]+head.length+1)
  } else {
    ch.start <- channelsep[1]+head.length
    ch.end <- channelsep[2] - (ch.start + 1)
  }
  data <- scan(file, what=list(NULL, character(), NULL, NULL, NULL, NULL, numeric()),
               skip= ch.start , nlines=ch.end, sep="\t",
               fill=T,fileEncoding="latin1", na.string="?")
  data.frame(DateTime = data[[2]], Measure = data[[7]], stringsAsFactors=FALSE)
}

beolvhoz <- scan("beolvhoz.txt", character())

######################################################################
## Beolvasás
######################################################################
Bez1 <- grep("Bezered1", beolvhoz)
Bez2 <- grep("Bezered2", beolvhoz)
Bez3 <- grep("Bezered3", beolvhoz)
Bez4 <- grep("Bezered4", beolvhoz)
Bez5 <- grep("Bezered5", beolvhoz)

Bezered1 <- beolvaso.zoo(beolvhoz[Bez1[1]])
for(tti in 2:length(Bez1)) Bezered1 <- beolvaso.zoo(beolvhoz[Bez1[tti]], object=Bezered1)

Bezered2 <- beolvaso.zoo(beolvhoz[Bez2[1]])
for(tti in 2:length(Bez2)) Bezered2 <- beolvaso.zoo(beolvhoz[Bez2[tti]], object=Bezered2)

Bezered3 <- beolvaso.zoo(beolvhoz[Bez3[1]], channel=2)
for(tti in 2:length(Bez3)) Bezered3 <- beolvaso.zoo(beolvhoz[Bez3[tti]], object=Bezered3, channel=2)

Bezered4 <- beolvaso.zoo(beolvhoz[Bez4[1]])
for(tti in 2:length(Bez4)) Bezered4 <- beolvaso.zoo(beolvhoz[Bez4[tti]], object=Bezered4)

Bezered5 <- beolvaso.zoo(beolvhoz[Bez5[1]], channel=2)
for(tti in 2:length(Bez5)) Bezered5 <- beolvaso.zoo(beolvhoz[Bez5[tti]], object=Bezered5, channel=2)

## Egyenközusítés
for(tti in 1:5) {
  ttname <- paste0("Bezered",tti)
  ttdata <- get(ttname)
  ttbound <- index(ttdata)[c(1,length(ttdata))]
  ttido <- seq(ttbound[1], ttbound[2], "min") # A folytonos ido eloállítása
  ttdummy <- zoo(NA, ttido)
  ttjav <- merge(ttdata, ttdummy)
  colnames(ttjav) <- c("Ori","h")
  ttjav[is.na(ttjav$Ori)] <- 0
  ttjav$h <- ttjav$Ori
  assign(ttname, ttjav)
}

## Teszt
##plot(as.xts(Bezered1[,1])['2017-09-19 05:00/2017-09-21 19:00'])

##plot(as.xts(Bezered2[,1])['2017-09-19 05:00/2017-09-21 19:00'])
##plot(as.xts(Bezered2[,1])['2017-09-19 03:00/2017-09-21 19:00'])

##plot(as.xts(Bezered5[,1])['2017-09-19 03:00/2017-09-21 19:00'])
#axis(2,at=0.026, tck=1) # akkor mért 0.0188
#0.026-0.0188 = 0

#axis(2,at=0.011, tck=1)

## Adatrendezés
## A bukókhoz az átbukási pont, akkorra kellene a szonda értékét 0-ra állítanunk
## Thomson: 1.343*h^(5/2)

#------------------------------------------------
## Bezered2

##szonda adatsorbol levonandó: 0.0785 m
Bezered2$h <- Bezered2$Ori - 0.0785 #bukóél felett a vízszint
Bezered2[Bezered2$h < 0,'h'] <- 0

##ugrálás kiszurésére és javítására minta
##plot(Bezered2$h, ylim=c(0,0.001)) #van-e valahol még ugrálás 0 felett?
##plot(as.xts(Bezered2)['2017-09-19/2017-09-20','h'], ylim=c(0,0.001)) #van-e valahol még ugrálás 0 felett?
# ha van ugrálás:
## B2ugralas <- c(1:100,102:110) # Adatkivételek sorszámai, ezek példák
##Bezered2[B2ugralas,'h'] <- 0

##plot(as.xts(Bezered2[,2]))
##plot(as.xts(Bezered2)['2017-09-19 13:00/2017-09-19 13:15',2])
##plot(as.xts(Bezered2)['2017-09-19 13:00/2017-09-19 13:15',2]) # Adatkivét
#which(index(Bezered2) == '2017-09-19 13:05') # Sorszám lekérdezés
B2adatki <- 57250:57255 # Adatkivételek sorszámai
Bezered2[B2adatki,'h'] <- NA
Bezered2[,'h'] <- na.approx(Bezered2[,'h']) #NA-k-hoz interpolálás
Bezered2$Q <- 1.343*as.numeric(Bezered2[,'h'])^(5/2) #Hozam bukóképlettel
##plot(as.xts(Bezered2[,3]))
##plot(as.xts(Bezered2)['2017-09-17/2017-09-20',3])
##plot(as.xts(Bezered2)['2017-09-19/2017-09-20',3])
##plot(as.xts(Bezered2)['2017-09-19',3])
Bezered2$hmBf <- 153.325 + Bezered2$h #Szintezésbol

Bezered2$h_2 <- Bezered2$Ori - 0.0785 + 0.105 #mederben a vízszint. Meder alja a bukóél alatt 0.105 m-re van.
Bezered2[Bezered2$h_2 < 0.06,'h_2'] <- 0
Bezered2[B2adatki,'h_2'] <- NA
#which(index(Bezered2) == '2017-10-23 01:17') #105502
#which(index(Bezered2) == '2017-10-23 07:32') #105877
B2_h_2_hiany <- 105502:105877
Bezered2[B2_h_2_hiany,'h_2'] <- NA
#which(index(Bezered2) == '2017-11-19 23:56') #145741
#which(index(Bezered2) == '2017-11-20 00:08') #145753
B2_h_2_hiany2 <- 145741:145753
Bezered2[B2_h_2_hiany2,'h_2'] <- NA
Bezered2[,'h_2'] <- na.approx(Bezered2[,'h_2']) #NA-k-hoz interpolálás
Bezered2$h_2_mBf <- 153.22 + Bezered2$h_2 #A mederszinthez viszonyított tengerszint feletti vízállás.

Bezered2.xts <- as.xts(Bezered2)
#Bezered2.xts["2017-09-19 03:40/2017-09-20 05:00"]

#par(mfrow = c(2,1))
#plot(as.xts(Bezered2$h))
#plot(as.xts(Bezered2$h_2))

#plot(as.xts(Bezered2)['2017-09-19',5])

colnames(Bezered2) <- c("Ori", "h", "Q", "h absz.", "h_2", "h_2_absz.")
## Add comment
Bez2mj <- data.frame(Tol=57250,Ig=57255,Mj="Adathiany adatkivetel miatt, interpolalt adat", stringsAsFactor=FALSE)
head(Bezered2)
tail(Bezered2)
#Megjegyzés oszlop neve?

#------------------------------------------------
## Bezered3

##eredeti adatsorból levonandó: 0.157 m
head(Bezered3)
#plot(as.xts(Bezered3[,2]))
Bezered3$h <- Bezered3$Ori - 0.157
Bezered3[Bezered3$h < 0,'h'] <- 0
##plot(Bezered3$h, ylim=c(0,0.001)) ##van-e ugrálás
#plot(as.xts(Bezered3)['2017-09-17/2017-09-20',2])
Bezered3$Q <- 1.343*as.numeric(Bezered3[,'h'])^(5/2) #Hozam bukóképlettel
#plot(as.xts(Bezered3[,3]))
#plot(as.xts(Bezered3)['2017-09-19 19:00/2017-09-19 23:00',3])
Bezered3$hmBf <- 163.522+ Bezered3$h

Bezered3$h_2 <- Bezered3$Ori - 0.157 + 0.082 #mederben a vízszint. Meder alja a bukóél alatt 0.082 m-re van.
Bezered3[Bezered3$h_2 < 0,'h_2'] <- 0
#plot(as.xts(Bezered3)['2017-09-19 11:28/2017-09-19 11:40',5])
#which(index(Bezered3) == '2017-09-19 11:30') #38769
#which(index(Bezered3) == '2017-09-19 11:32') #38771
B3adatki <- 38769:38771 # Adatkivételek sorszámai
Bezered3[B3adatki,'h_2'] <- NA
Bezered3[,'h_2'] <- na.approx(Bezered3[,'h_2']) #NA-k-hoz interpolálás
Bezered3$h_2_mBf <- 163.44 + Bezered3$h_2 #A mederszinthez viszonyított tengerszint feletti vízállás.

Bezered3.xts <- as.xts(Bezered3)
#Bezered3.xts["2017-09-19 06:40/2017-09-20 05:00"]

colnames(Bezered3) <- c("Ori", "h", "Q","h absz.", "h_2", "h_2_absz.")
#plot(as.xts(Bezered3[,4]))
#plot(as.xts(Bezered3)['2017-09-17/2017-09-20',4])

## Nincs megjegyzés
Bez3mj <- data.frame()

head(Bezered3)
tail(Bezered3)

#par(mfrow = c(2,1))
#plot(as.xts(Bezered3$h))
#plot(as.xts(Bezered3$h_2))

#------------------------------------------------
## Bezered5

##eredeti adatsorból levonandó: 0.0072 m
head(Bezered5)
##plot(as.xts(Bezered5[,1]))
Bezered5$h <- Bezered5$Ori - 0.0072
Bezered5[Bezered5$h < 0,'h'] <- 0
##plot(Bezered5$h, ylim=c(0,0.001))  ##van-e ugrálás
##plot(as.xts(Bezered5)['2017-09-19/2017-09-20','h'], ylim=c(0,0.001))  ##van-e ugrálás
##adatkivétel adathiány
#plot(as.xts(Bezered5)['2017-09-19 14:00/2017-09-19 14:20','h']) # Adatkivét
#which(index(Bezered5) == '2017-09-19 14:04') # Sorszám lekérdezés
#which(index(Bezered5) == '2017-09-19 14:12')
B5adatki <- 19929:19937 # Adatkivételek sorszámai
Bezered5[B5adatki,'h'] <- NA
#which(index(Bezered5) == '2017-11-03 13:40') #84705
#which(index(Bezered5) == '2017-11-03 13:47') #84712
B5adatki2 <- 84705:84712 # Adatkivételek sorszámai
Bezered5[B5adatki2,'h'] <- NA
#which(index(Bezered5) == '2017-11-10 11:34') #94659
#which(index(Bezered5) == '2017-11-10 11:39') #94664
B5adatki3 <- 94659:94664 # Adatkivételek sorszámai
Bezered5[B5adatki3,'h'] <- NA
#which(index(Bezered5) == '2017-10-24 14:15') #70340
B5fura <-70340
Bezered5[B5fura,'h'] <- NA
Bezered5[,'h'] <- na.approx(Bezered5[,'h']) #NA-k-hoz interpolálás
#plot(as.xts(Bezered5[,'h']))
##plot(as.xts(Bezered5)['2017-09-18 10:00/2017-09-18 14:00','h']) #érdekes, 09.17. után itt ekkor jelenhetett meg
Bezered5$Q <- 1.343*as.numeric(Bezered5[,'h'])^(5/2) #Hozam bukóképlettel
#plot(as.xts(Bezered5[,3]))
##plot(as.xts(Bezered5)['2017-09-18 10:00/2017-09-18 14:00',3]) ##pici átbukás
Bezered5$hmBf <- 156.358 + Bezered5$h #szintezésbol
#plot(as.xts(Bezered5[,3]))

Bezered5$h_2 <- Bezered5$Ori - 0.0072 + 0.057 #mederben a vízszint. Meder alja a bukóél alatt 0.082 m-re van.
#adatkivetelek itt is
Bezered5[B5adatki,'h_2'] <- NA
Bezered5[B5adatki2,'h_2'] <- NA
Bezered5[B5adatki3,'h_2'] <- NA
Bezered5[B5fura,'h_2'] <- NA
Bezered5[,'h_2'] <- na.approx(Bezered5[,'h_2']) #NA-k-hoz interpolálás
#plot(as.xts(Bezered5[,5]))
#
Bezered5[Bezered5$h_2 < 0.053,'h_2'] <- 0
Bezered5$h_2_mBf <- 156.301 + Bezered5$h_2 #A mederszinthez viszonyított tengerszint feletti vízállás.

colnames(Bezered5) <- c("Ori", "h", "Q", "h absz.", "h_2", "h_2_absz.")

Bez5mj <- data.frame(Tol=19929,Ig=19937,Mj="Adathiany adatkivetel miatt, interpolalt adat", stringsAsFactor=FALSE)
Bezered5.xts <- as.xts(Bezered5)
## A feltöltött meder miatt hibás adatok innentõl
#which(index(Bezered5) == '2017-10-23 08:00') #68525
Bezered5[68525:nrow(Bezered5),"Q"] <- NA
Bez5mj <- rbind(Bez5mj,data.frame(Tol=68525,Ig=nrow(Bezered5),Mj="Hibas h adat a feltoltott meder miatt", stringsAsFactor=FALSE))

head(Bezered5)
tail(Bezered5)

#par(mfrow = c(2,1))
#plot(as.xts(Bezered5$h))
#plot(as.xts(Bezered5$h_2))

#------------------------------------------------
## Bezered4

#plot(Bezered4)
Bezered4$h <- 2.295 + Bezered4$Ori
Bezered4[Bezered4$h > 2.29,2] <- NA
Bezered4 <- as.xts(Bezered4)
#plot(Bezered4$h)
Bezered4['2017-08-10 16:42:00','h'] <- 0.001
Bezered4['2017-09-05 14:12:00','h'] <- 0.006
##Bezered4['2017-09-15 14:17:00','h'] <- 0.012 #ezeknél mért a szonda
##Bezered4['2017-09-19 15:09:00','h'] <- 0.022 #ezeknél mért a szonda
Bezered4['2017-09-29 11:10:00','h'] <- 0.004
Bezered4['2017-10-06 09:05:00','h'] <- 0.007
Bezered4['2017-10-06 14:16:00','h'] <- 0.007 #Az eso elott
##plot(as.xts(Bezered4)['2017-10-06 14:00/2017-10-06 15:50',2])
Bezered4['2017-10-06 16:06:00','h'] <- 0.007 #Az eso után
Bezered4['2017-10-12 10:55:00','h'] <- 0.007
Bezered4['2017-10-20 11:46:00','h'] <- 0.01 ## Igazából 55

#which(index(Bezered4) == '2017-10-19 14:16') #100655
#which(index(Bezered4) == '2017-10-19 14:29') #100668
B4fura <-100655:100668
Bezered4[B4fura,'h'] <- NA
#Bezered4$h <- na.approx(Bezered4$h)
Bezered4[,'h'] <- na.approx(Bezered4[,'h'])
Bezered4$hmBf <- 167.286 + Bezered4$h
colnames(Bezered4) <- c("Ori", "h", "h absz.")
#plot(round(Bezered4[,2],3))
#axis(2,at=0, tck=1)

#megjegyzeshez sorszam lekerdezes:

# Sorszám lekérdezés 
#which(index(Bezered4) == '2017-08-10 16:42') #1 mért
#which(index(Bezered4) == '2017-09-05 14:12:00') #37291  mért
#which(index(Bezered4) == '2017-09-29 11:10:00') #71669  mért
#which(index(Bezered4) == '2017-10-06 09:05:00') #81624  mért
#which(index(Bezered4) == '2017-10-06 14:16:00') #81935
#which(index(Bezered4) == '2017-10-06 16:06:00') #82045
#which(index(Bezered4) == '2017-10-12 10:55:00') #90374  mért
#which(index(Bezered4) == '2017-10-20 11:46:00') #101945 mért

##Bez4mj <- data.frame(Tol=c(1,2,37291,37292),
#                     Ig=c(1,37290,37291,37300),
#                    Mj=c("Mert adat","Interpolalt adat","Mert adat","Interpolalt adat"), stringsAsFactors=FALSE)


Bez4mj <- data.frame(Tol=c(1,37291,71669,81624,90374,101945),Ig=c(1,37291,71669,81624,90374,101945),Mj="Manualisan mert vizallas", stringsAsFactor=FALSE)

#plot(Bezered4$h)
head(Bezered4)
tail(Bezered4)

#------------------------------------------------
##Bezered1

#plot(Bezered1)

Bezered1 <- as.xts(Bezered1)

#Eltömodés idoszakára adatpótlás
#plot(Bezered1)['2017-09-20 15:00/2017-09-20 18:00','h']
# itt még jó: 2017-09-20 15:29:00	0.077	0.077
# itt már van adat: 2017-09-29 15:24:00	0.077
# a fenti ketto közé kellene pótolni.
Bezered1['2017-09-20 15:30:00/2017-09-29 15:23:00','h'] <- 0.077

##adatkivétel:
##2017-09-19 10:23:00/2017-09-19 10:26:00
Bezered1['2017-09-19 10:23:00/2017-09-19 10:26:00','h'] <- NA
##2017-10-06 12:32:00/2017-10-06 12:35:00
Bezered1['2017-10-06 12:32:00/2017-10-06 12:35:00','h'] <- NA
##2017-10-12 13:55:00/2017-10-12 13:59:00
Bezered1['2017-10-12 13:55:00/2017-10-12 13:59:00','h'] <- NA
##2017-10-20 07:32:00/2017-10-20 07:36:00
Bezered1['2017-10-20 07:32:00/2017-10-20 07:36:00','h'] <- NA
##2017-10-27 13:39:00/2017-10-27 13:50:00
Bezered1['2017-10-27 13:39:00/2017-10-27 13:50:00','h'] <- NA
##2017-11-03 10:27:00/2017-11-03 10:43:00
Bezered1['2017-11-03 10:27:00/2017-11-03 10:43:00','h'] <- NA
##2017-11-10 09:28:00/2017-11-10 09:36:00
Bezered1['2017-11-10 09:28:00/2017-11-10 09:36:00','h'] <- NA

##Adatkivétel
#which(index(Bezered1) == '2017-09-19 10:23:00') #57024
#which(index(Bezered1) == '2017-09-19 10:26:00') #57027
#which(index(Bezered1) == '2017-10-06 12:32:00') #81633
#which(index(Bezered1) == '2017-10-06 12:35:00') #81636
#which(index(Bezered1) == '2017-10-12 13:55:00') #90356
#which(index(Bezered1) == '2017-10-12 13:59:00') #90360
#which(index(Bezered1) == '2017-10-20 07:32:00') #101493
#which(index(Bezered1) == '2017-10-20 07:36:00') #101497
#which(index(Bezered1) == '2017-10-27 13:39:00') #111940
#which(index(Bezered1) == '2017-10-27 13:50:00') #111951
#which(index(Bezered1) == '2017-11-03 10:27:00') #121828
#which(index(Bezered1) == '2017-11-03 10:43:00') #121844
#which(index(Bezered1) == '2017-11-10 09:28:00') #131849
#which(index(Bezered1) == '2017-11-10 09:36:00') #131857

##Megjegyzés oszlopba:eltömodött a szonda, pótolt adat.
##Amikor ott voltunk:  Megj.: áll a víz a mederben, de nem folyik
##2017-09-29 15:24:00
#which(index(Bezered1) == '2017-09-29 15:24:00') #71725 mért
##2017-10-06 12:15:00
#which(index(Bezered1) == '2017-10-06 12:15:00') #81616 mért
##2017-10-12 13:50:00
#which(index(Bezered1) == '2017-10-12 13:50:00') #90351 mért
##Amikor ott voltunk: Megj.: a víz a mederben csörgedezik.
##2017-10-20 07:30:00
#which(index(Bezered1) == '2017-10-20 07:30:00') #101491 mért

Bez1mj <- data.frame(
  Tol=c(71725,81616,90351,101491,57024,81633,90356,101493,111940,121828,131849),
  Ig =c(71725,81616,90351,101491,57027,81636,90360,101497,111951,121844,131857),
  Mj=c(rep("all a viz a mederben, de nem folyik",3),"a viz a mederben csorgedezik",
       rep("Adathiany adatkivetel miatt, interpolalt adat",7)),
  stringsAsFactor=FALSE)

Bezered1$hmBf <- 142.656 + Bezered1$h
colnames(Bezered1) <- c("Ori", "h", "h absz.")

#plot(Bezered1$h)
head(Bezered1)
tail(Bezered1)

#------------------------------------------------

#kiírás

#------------------------------------------------
#Mentés 1 és 4 esetében
for(tti in c(1,4)){
  ttmp <- as.data.frame(get(paste0("Bezered",tti))[,-1])
  ttmp$h <- round(ttmp$h,3)
  ttmp[,'h absz.'] <- round(ttmp[,'h absz.'],3)
  if(ncol(ttmp) > 2)
    ttmp$Q <- round(ttmp$Q,4)
  ttmj <- get(paste0("Bez",tti,"mj"))
  if(nrow(ttmj) > 0) {
    ttmp$Megj <- NA
    for(ttmjsor in 1:nrow(ttmj)) {
      ttsorok <- ttmj[ttmjsor,'Tol']:ttmj[ttmjsor,'Ig']
      ttmp[ttsorok, 'Megj'] <- as.character(ttmj[ttmjsor,'Mj'])
    }
  }
  write.table(ttmp, paste0("Bezered",tti,".txt"), sep="\t", quot=FALSE, col.names = NA, row.names = TRUE, na="")
}

#------------------------------------------------
#Mentés 2,3 és 5 esetében
for(tti in c(2:3,5)){
  ttmp <- as.data.frame(get(paste0("Bezered",tti))[,-1])
  ttmp$h <- round(ttmp$h,3)
  ttmp[,'h absz.'] <- round(ttmp[,'h absz.'],3)
  if(ncol(ttmp) > 2)
    ttmp$Q <- round(ttmp$Q,4)
  ttmp[,'h_2'] <- round(ttmp[,'h_2'],3)
  ttmp[,'h_2_absz.'] <- round(ttmp[,'h_2_absz.'],3)
   ttmj <- get(paste0("Bez",tti,"mj"))
  if(nrow(ttmj) > 0) {
    ttmp$Megj <- NA
    for(ttmjsor in 1:nrow(ttmj)) {
      ttsorok <- ttmj[ttmjsor,'Tol']:ttmj[ttmjsor,'Ig']
      ttmp[ttsorok, 'Megj'] <- as.character(ttmj[ttmjsor,'Mj'])
    }
  }
  write.table(ttmp, paste0("Bezered",tti,".txt"), sep="\t", quot=FALSE, col.names = NA, row.names = TRUE, na="")
}
