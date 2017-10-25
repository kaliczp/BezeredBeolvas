## Kiindulás Városi adatok Readme.R
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
    colnames(ttjav) <- c("Ori","Correct")
    ttjav[is.na(ttjav$Ori)] <- 0
    ttjav$Correct <- ttjav$Ori
    assign(ttname, ttjav)
}

Bezered1 <- 

## Teszt és mentés
plot(Bezered1)
for(tti in 1:5)
    write.zoo(get(paste0("Bezered",tti)), paste0("Bezered",tti,".txt"), sep="\t")

plot(as.xts(Bezered1[,1])['2017-09-19 05:00/2017-09-21 19:00'])

plot(as.xts(Bezered2[,1])['2017-09-19 05:00/2017-09-21 19:00'])
plot(as.xts(Bezered2[,1])['2017-09-19 03:00/2017-09-21 19:00'])

Bezered4$Correct <- 2.291 + Bezered4$Ori
Bezered4[Bezered4$Correct > 2.2,2]=NA
plot(Bezered4[,2],ylim=c(0,0.06))
axis(2,at=0, tck=1)

plot(as.xts(Bezered5[,1])['2017-09-19 03:00/2017-09-21 19:00'])
axis(2,at=0.024, tck=1) # akkor mért 0.015
#0.026-0.015 = 0

axis(2,at=0.011, tck=1)

## Adatrendezés
## Akkor a bukókhoz az átbukási pont, ekkorra kellene a szonda értékét 0-ra állítanunk
## Thomson: 1.343*h^(5/2)
## Bezered2: 0.0521 m
## Bezered3: 0.157 m
## Bezered5: 0.015 m
