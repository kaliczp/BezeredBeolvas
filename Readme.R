## Kiindulás Városi adatok Readme.R
beolvaso.zoo <- function(file, channel=1, object=NULL){
    require(zoo)
    if(is.null(object)){
        rec <- read.table(file, sep="\t",skip=12,dec=".",fill=TRUE,na=c("--------------------------------------------------------"))
        print(file)
        out <- zoo(rec[-nrow(rec),7],strptime(as.character(rec[-nrow(rec),2]),"%Y.%m.%d %T"))
        datecode <- datecode[-1]
    } else {
        out <- object
    }
    ## Zooba beolvasó fő ciklus. Létező objektum esetén ez dolgozik.
    for(current.datecode in datecode){
        rec <- read.table(file, sep="\t",skip=12,dec=".",fill=TRUE,na=c("--------------------------------------------------------"))
        print(fname)
        rec.zoo <- zoo(rec[-nrow(rec),7],strptime(as.character(rec[-nrow(rec),2]),"%Y.%m.%d %T"))
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
    data <- scan(file, what=list(NULL,"",NULL,NULL,NULL,NULL,numeric()),
                 skip= ch.start , nlines=ch.end, sep="\t",
                 fill=T,fileEncoding="latin1", na.string="?")
    data.frame(DateTime = data[[2]], Measure = data[[7]])
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
Bezered1 <- smartbe(beolvhoz[1])
Bezered2 <- smartbe(beolvhoz[2])
Bezered3 <- smartbe(beolvhoz[9])
Bezered4 <- smartbe(beolvhoz[3])
Bezered5 <- smartbe(beolvhoz[15])
