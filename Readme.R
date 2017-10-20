beolvaso.zoo <- function(datecode,placecode="818/Bezered1_",object=NULL){
    require(zoo)
    if(is.null(object)){
        fname <- paste0(placecode,datecode[1],".TXT")
        rec <- read.table(fname, sep="\t",skip=12,dec=".",fill=TRUE,na=c("--------------------------------------------------------"))
        print(fname)
        out <- zoo(rec[-nrow(rec),7],strptime(as.character(rec[-nrow(rec),2]),"%Y.%m.%d %T"))
        datecode <- datecode[-1]
    } else {
        out <- object
    }
    ## Zooba beolvasó fő ciklus. Létező objektum esetén ez dolgozik.
    for(current.datecode in datecode){
        fname <- paste(placecode,current.datecode,".txt",sep="")
        rec <- read.table(fname, sep="\t",skip=12,dec=".",fill=TRUE,na=c("--------------------------------------------------------"))
        print(fname)
        rec.zoo <- zoo(rec[-nrow(rec),7],strptime(as.character(rec[-nrow(rec),2]),"%Y.%m.%d %T"))
    out <- c(out,rec.zoo)
    }
    out
}

######################################################################
## Beolvasás
######################################################################
Bezered1 <- beolvaso.zoo(818,"818/Bezered1_")
Bezered2 <- beolvaso.zoo(818,"818/Bezered2_")
Bezered3 <- beolvaso.zoo(818,"818/Bezered4_")
Bezered4 <- b4818[-length(b4818)]
