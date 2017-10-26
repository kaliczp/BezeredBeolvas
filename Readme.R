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
    ## Zooba beolvasó fő ciklus. Létező objektum esetén ez dolgozik.
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

## Egyenközűsítés
for(tti in 1:5) {
    ttname <- paste0("Bezered",tti)
    ttdata <- get(ttname)
    ttbound <- index(ttdata)[c(1,length(ttdata))]
    ttido <- seq(ttbound[1], ttbound[2], "min")
    ttdummy <- zoo(NA, ttido)
    ttjav <- merge(ttdata, ttdummy)
    colnames(ttjav) <- c("Ori","h")
    ttjav[is.na(ttjav$Ori)] <- 0
    ttjav$h <- ttjav$Ori
    assign(ttname, ttjav)
}

## Teszt
plot(as.xts(Bezered1[,1])['2017-09-19 05:00/2017-09-21 19:00'])

plot(as.xts(Bezered2[,1])['2017-09-19 05:00/2017-09-21 19:00'])
plot(as.xts(Bezered2[,1])['2017-09-19 03:00/2017-09-21 19:00'])

plot(as.xts(Bezered5[,1])['2017-09-19 03:00/2017-09-21 19:00'])
axis(2,at=0.026, tck=1) # akkor mért 0.0188
#0.026-0.0188 = 0

axis(2,at=0.011, tck=1)

## Adatrendezés
## A bukókhoz az átbukási pont, akkorra kellene a szonda értékét 0-ra állítanunk
## Thomson: 1.343*h^(5/2)
#------------------------------------------------
## Bezered2 szonda adatsorbol levonandó: 0.0785 m
Bezered2$h <- Bezered2$Ori - 0.0785
Bezered2[Bezered2$h < 0,'h'] <- 0

##ugrálás kiszűrésére és javítására minta
##plot(Bezered2$h, ylim=c(0,0.001)) #van-e valahol még ugrálás 0 felett?
##plot(as.xts(Bezered2)['2017-09-19/2017-09-20','h'], ylim=c(0,0.001)) #van-e valahol még ugrálás 0 felett?
# ha van ugrálás:
## B2ugralas <- c(1:100,102:110) # Adatkivételek sorszámai, ezek példák
##Bezered2[B2ugralas,'h'] <- 0

##plot(as.xts(Bezered2[,2]))
##plot(as.xts(Bezered2)['2017-09-19 13:00/2017-09-19 13:15',2])
##plot(as.xts(Bezered2)['2017-09-19 13:00/2017-09-19 13:15',2]) # Adatkivét
which(index(Bezered2) == '2017-09-19 13:05') # Sorszám lekérdezés
B2adatki <- 57250:57255 # Adatkivételek sorszámai
Bezered2[B2adatki,'h'] <- NA
Bezered2[,'h'] <- na.approx(Bezered2[,'h']) #NA-k-hoz interpolálás
Bezered2$Q <- 1.343*as.numeric(Bezered2[,'h'])^(5/2) #Hozam bukóképlettel
##plot(as.xts(Bezered2[,3]))
##plot(as.xts(Bezered2)['2017-09-17/2017-09-20',3])
##plot(as.xts(Bezered2)['2017-09-19/2017-09-20',3])
##plot(as.xts(Bezered2)['2017-09-19',3])
Bezered2$hmBf <- 153.137+ Bezered2$h #ez nagyon bizonytalan! szintezésből rendes adat?
colnames(Bezered2) <- c("Ori", "h", "Q", "h absz.")
## Add comment
Bez2mj <- data.frame(Tol=57250,Ig=57255,Mj="Adathiány adatkivétel miatt, interpolált adat", stringsAsFactor=FALSE)
head(Bezered2)
#Megjegyzés oszlop neve?

#------------------------------------------------
## Bezered3 eredeti adatsorból levonandó: 0.157 m
head(Bezered3)
plot(as.xts(Bezered3[,2]))
Bezered3$h <- Bezered3$Ori - 0.157
Bezered3[Bezered3$h < 0,'h'] <- 0
##plot(Bezered3$h, ylim=c(0,0.001)) ##van-e ugrálás
plot(as.xts(Bezered3)['2017-09-17/2017-09-20',2])
Bezered3$Q <- 1.343*as.numeric(Bezered3[,'h'])^(5/2) #Hozam bukóképlettel
plot(as.xts(Bezered3[,3]))
plot(as.xts(Bezered3)['2017-09-19 19:00/2017-09-19 23:00',3])
Bezered3$hmBf <- 163.522+ Bezered3$h
colnames(Bezered3) <- c("Ori", "h", "Q","h absz.")
plot(as.xts(Bezered3[,4]))
plot(as.xts(Bezered3)['2017-09-17/2017-09-20',4])

#------------------------------------------------
## Bezered5 eredeti adatsorból levonandó: 0.0072 m
head(Bezered5)
##plot(as.xts(Bezered5[,1]))
Bezered5$h <- Bezered5$Ori - 0.0072
Bezered5[Bezered5$h < 0,'h'] <- 0
##plot(Bezered5$h, ylim=c(0,0.001))  ##van-e ugrálás
##plot(as.xts(Bezered5)['2017-09-19/2017-09-20','h'], ylim=c(0,0.001))  ##van-e ugrálás
##adatkivétel adathiány
plot(as.xts(Bezered5)['2017-09-19 14:00/2017-09-19 14:20','h']) # Adatkivét
which(index(Bezered5) == '2017-09-19 14:04') # Sorszám lekérdezés
which(index(Bezered5) == '2017-09-19 14:12')
B5adatki <- 19929:19937 # Adatkivételek sorszámai
Bezered5[B5adatki,'h'] <- NA
Bezered5[,'h'] <- na.approx(Bezered5[,'h']) #NA-k-hoz interpolálás
plot(as.xts(Bezered5[,'h']))
##plot(as.xts(Bezered5)['2017-09-18 10:00/2017-09-18 14:00','h']) #érdekes, 09.17. után itt ekkor jelenhetett meg
Bezered5$Q <- 1.343*as.numeric(Bezered5[,'h'])^(5/2) #Hozam bukóképlettel
plot(as.xts(Bezered5[,3]))
##plot(as.xts(Bezered5)['2017-09-18 10:00/2017-09-18 14:00',3]) ##pici átbukás
Bezered5$hmBf <- + Bezered5$h #ezt szintezésből?
colnames(Bezered2) <- c("Ori", "h", "Q", "h absz.")
colnames(Bezered5) <- c("Ori", "h", "Q")
Bez5mj <- data.frame(Tol=19929,Ig=19937,Mj="Adathiány adatkivétel miatt, interpolált adat", stringsAsFactor=FALSE)

#------------------------------------------------
## Bezered4
plot(Bezered4)
Bezered4$h <- 2.295 + Bezered4$Ori
Bezered4[Bezered4$h > 2.29,2] <- NA
Bezered4 <- as.xts(Bezered4)
plot(as.xts(Bezered4$h))
Bezered4['2017-08-10 16:42:00','h'] <- 0.001
Bezered4['2017-09-05 14:12:00','h'] <- 0.006
##Bezered4['2017-09-15 14:17:00','h'] <- 0.012 #ezeknél mért a szonda
##Bezered4['2017-09-19 15:09:00','h'] <- 0.022 #ezeknél mért a szonda
Bezered4['2017-09-29 11:10:00','h'] <- 0.004
Bezered4['2017-10-06 09:05:00','h'] <- 0.007
Bezered4['2017-10-06 14:16:00','h'] <- 0.007 #Az eső előtt
##plot(as.xts(Bezered4)['2017-10-06 14:00/2017-10-06 15:50',2])
Bezered4['2017-10-06 16:06:00','h'] <- 0.007 #Az eső után
Bezered4['2017-10-12 10:55:00','h'] <- 0.007
Bezered4['2017-10-20 11:55:00','h'] <- 0.01 
Bezered4$h <- na.approx(Bezered4$h)
Bezered4[,'h'] <- na.approx(Bezered4[,'h'])
Bezered4$hmBf <- 167.286 + Bezered4$h
colnames(Bezered4) <- c("Ori", "h", "h absz.")
plot(round(Bezered4[,2],3))
axis(2,at=0, tck=1)

#megjegyzeshez sorszam lekerdezes:

# Sorszám lekérdezés 
which(index(Bezered4) == '2017-08-10 16:42') #1 mért
which(index(Bezered4) == '2017-09-05 14:12:00') #37291  mért
which(index(Bezered4) == '2017-09-29 11:10:00') #71669  mért
which(index(Bezered4) == '2017-10-06 09:05:00') #81624  mért
which(index(Bezered4) == '2017-10-06 14:16:00') #81935
which(index(Bezered4) == '2017-10-06 16:06:00') #82045
which(index(Bezered4) == '2017-10-12 10:55:00') #90374  mért
which(index(Bezered4) == '2017-10-20 11:46:00') #101945 mért

##Bez4mj <- data.frame(Tol=c(1,2,37291,37292),
#                     Ig=c(1,37290,37291,37300),
#                    Mj=c("Mért adat","Interpolált adat","Mért adat","Interpolált adat"), stringsAsFactors=FALSE)

#szerintem így nem jó, mert van ahol 1-2-t mér a szonda, aztán 1-2-t nem...
#nem lehet egyesével kigyűjögetni. 
#Most csak azokat tüntetem fel

Bez4mj <- Bezered4[c(1,37291,71669,81624,90374,101945),Mj="Manuálisan mért vízállás", stringsAsFactor=FALSE]

#------------------------------------------------
##Bezered1
plot(Bezered1)

#adatkivétel:
#2017-09-19 10:23:00/2017-09-19 10:26:00
#2017-10-06 12:32:00/2017-10-06 12:35:00
#2017-10-12 13:55:00/2017-10-12 13:59:00

Bezered1 <- as.xts(Bezered1)

#Eltömődés időszakára adatpótlás
plot(as.xts(Bezered1)['2017-09-20 15:00/2017-09-20 18:00','h'])
# itt még jó: 2017-09-20 15:29:00	0.077	0.077
# itt már van adat: 2017-09-29 15:24:00	0.077
# a fenti kettő közé kellene pótolni.
Bezered1['2017-09-20 15:30:00/2017-09-29 15:23:00','h'] <- 0.077
#Megjegyzés oszlopba:eltömődött a szonda, pótolt adat.
#Amikor ott voltunk: Megj.: áll a víz a mederben, csörgedezik.
  #2017-10-20 07:30:00
#Amikor ott voltunk:  Megj.: áll a víz a mederben, de nem folyik
  #2017-09-29 15:24:00
  #2017-10-06 12:15:00
  #2017-10-12 13:50:00

Bezered1$hmBf <- 142.656 + Bezered1$h
colnames(Bezered1) <- c("Ori", "h", "h absz.")

#------------------------------------------------
#mBf oszlop létrehozása
#kiírás

#------------------------------------------------
#Mentés
plot(Bezered1)
for(tti in 1:5){
    ttmp <- as.data.frame(get(paste0("Bezered",tti))[,-1])
    ttmj <- get(paste0("Bez",tti,"mj"))
    ttmp$h <- round(ttmp$h,3)
    if(ncol(ttmp) > 1)
        ttmp$Q <- round(ttmp$Q,4)
    ttmp$Megj <- NA
    for(ttmjsor in 1:nrow(ttmj)) {
        ttsorok <- ttmj[ttmjsor,'Tol']:ttmj[ttmjsor,'Ig']
        ttmp[ttsorok, 'Megj'] <- as.character(ttmj[ttmjsor,'Mj'])
    }
    write.table(ttmp, paste0("Bezered",tti,".txt"), sep="\t", quot=FALSE, col.names = NA, row.names = TRUE)
}
