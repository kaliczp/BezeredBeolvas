## Kiindulás Városi adatok Readme.R
beolvaso.zoo <- function(file, channel=1, object=NULL){
    require(zoo)
    if(is.null(object)){
        rec <- smartbe(file, channel=channel)
        print(file)
        out <- zoo(rec[,2],strptime(rec[,1], "%Y.%m.%d %T"))
        file <- file[-1]
    } else {
        out <- object
    }
    ## Zooba beolvasó fő ciklus. Létező objektum esetén ez dolgozik.
    for(current.file in file){
        rec <- smartbe(current.file, channel=channel)
        print(file)
        rec.zoo <- zoo(rec[,2],strptime(rec[,1], "%Y.%m.%d %T"))
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
        ch.start <- headsep+head.length
        ch.end <- channelsep[1]-(headsep+head.length+1)
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
grep("Bezered1", beolvhoz)
grep("Bezered2", beolvhoz)
grep("Bezered3", beolvhoz)
grep("Bezered4", beolvhoz)
grep("Bezered5", beolvhoz)
Bezered1 <- beolvaso.zoo(beolvhoz[1])
Bezered2 <- beolvaso.zoo(beolvhoz[2])
Bezered3 <- beolvaso.zoo(beolvhoz[9], channel=2)
Bezered4 <- beolvaso.zoo(beolvhoz[3])
Bezered5 <- beolvaso.zoo(beolvhoz[15], channel=2)
