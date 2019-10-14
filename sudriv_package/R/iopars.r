sudriv.writepars <- function(file, pars, npar=NA, infoStr="This parameter set is special because...", version="1.1"){

    if(is.na(npar[1])) npar = length(pars)
    if(is.na(infoStr[1])) infoStr = "This parameter set is special because..."
    write(x = c(
              "BATEAU_PAR_FILE_V1.1",
              "RunTag='Esch_Lum_Str03_flow_Tr1'; Inferenz='BASIC'[10]",
              paste("[[date='20160826' and time='145650.999']]; infoStr='",infoStr,"'", sep = ""),
              "---------------------------------------",
              paste("Parameters [nPar=",npar,"][modelID=0,1003]", sep = ""),
              "'(1x,es22.14e3,2x,a,i4,a)'",
              paste(as.numeric(pars),names(pars), sep="\t"),
              "---------------------------------------"),

          file = file,
          ncolumns = 1
          )




}

sudriv.readpars.withnames <- function(parfile, skip=6){

    con <- file(parfile, open = "r")
    pars <- list()
    nmes <- list()
    i <- 1
    l <- 1
    while(length(oneLine <- readLines(con, n=1, warn=FALSE)) > 0){
        if(i > skip){
            if(!is.na(suppressWarnings(as.numeric(oneLine)))){
                if(length(grep("---", oneLine))==0){
                    pars[[l]] <- suppressWarnings(as.numeric(oneLine))
                    l = l + 1
                }
            }else{
                nospace <- sub("^\\s+", "", oneLine) #trim leading whitespace
                if(length(grep("---", oneLine))==0){
                    splt <- strsplit(nospace, "\\s")
                    num <- splt[[1]][1]
                    nme <- splt[[1]][2]
                    pars[[l]] <- suppressWarnings(as.numeric(num[1]))
                    nmes[[l]] <- as.character(nme[1])
                    l = l + 1
                }
            }
        }
        i <- i + 1
    }
    close(con)
    pars <- unlist(pars)
    names(pars) <- unlist(nmes)
    return(pars)
}

sudriv.readpars <- function(parfile, skip=6){

    con <- file(parfile, open = "r")
    pars <- list()
    i <- 1
    l <- 1
    while(length(oneLine <- readLines(con, n=1, warn=FALSE)) > 0){
        if(i > skip){
            if(!is.na(suppressWarnings(as.numeric(oneLine)))){
                if(length(grep("---", oneLine))==0){
                    pars[[l]] <- as.numeric(suppressWarnings(oneLine))
                    l = l + 1
                }
            }else{
                nospace <- sub("^\\s+", "", oneLine) #trim leading whitespace
                num <- strsplit(nospace, "\\s")[[1]]
                if(length(num)>=2){
                    pars[[l]] <- as.numeric(suppressWarnings(num[1]))
                    l = l + 1
                }
            }
        }
        i <- i + 1
    }
    close(con)
    return(unlist(pars))
}

sudriv.writeHRUs <- function(file, hru.frac, rout.mat=NA, version="1.0"){
    write(x=c(
              paste("FLEX_HRU_CHARACTERISTICS_FILE_V",version,sep=""),
              "! Description: HRUs characterisics (-1=NA, depending on mask Unit Presence Matrix)",
              "!========",
              "AREAS (impervious connected drained condrain disconnected)"),
          file=file,
          ncolumns=1)
    write(x=t(hru.frac),
          file=file,
          ncolumns=ncol(hru.frac),
          append=TRUE)
    write(x=c("!========",
              "SLOPE (%) (first column used to scale the routing within the subcatchments)"),
          file=file,
          ncolumns=1,
          append=TRUE)
    if(is.na(c(rout.mat)[1])){
        rout.mat <- matrix(sqrt(rowSums(hru.frac)), nrow=nrow(hru.frac), ncol=ncol(hru.frac))
    }
    write(x=t(rout.mat),
          file=file,
          ncolumns=ncol(hru.frac),
          append=TRUE)
    write(x=c("!========",
              "STD_SLOPE (%) (not used)"),
          file=file,
          ncolumns=1,
          append=TRUE)
    write(x=matrix(1, nrow(hru.frac), ncol(hru.frac)),
          file=file,
          ncolumns=ncol(hru.frac),
          append=TRUE)
}

sudriv.readHRUs <- function(f.path.hru, nsub=1){
    hru.areas <- as.matrix(read.table(file=f.path.hru, header=FALSE, sep="", skip=4, nrows=nsub))
    return(hru.areas)
}

sudriv.readtransK <- function(f.path.transK){
    tk <- read.table(file=f.path.transK, header=FALSE, sep="", row.names=1)
    transK <- as.list(t(tk))
    names(transK) <- rownames(tk)
    return(transK)
}

read.app.hru.areas <- function(sudriv=NA, compare.models=NA, tag.mult=NA){
    flux.atra.tot <- list()
    flux.terb.tot <- list()
    app.hru.areas <- list()
    for(model.name in names(compare.models)){
        if(is.na(sudriv)) su <- sudriv.setup(settings=compare.models[model.name])
        path <- paste0("../output/", su$settings$subcatchment, su$settings$structure, "/", tag.mult[model.name])
        ## Read the total loads for the maximum likelihood case
        loadsA <- read.table(paste0(path, "/states_fluxA_Eapp_",tag.mult[model.name], "loads.txt"), header=TRUE)
        loadsA[,"hru"] <- gsub("Shortcut", "Connected", loadsA[,"hru"])
        loadsA[,"hru"] <- gsub("SC and Drained", "Connected and Drained", loadsA[,"hru"])
        loadsA[,"hru"] <- gsub("Rest", "Unconnected", loadsA[,"hru"])
        atr <- loadsA[,"load"]
        names(atr) <- loadsA[,"hru"]
        loadsT <- read.table(paste0(path, "/states_fluxT_Eapp_",tag.mult[model.name], "loads.txt"), header=TRUE)
        loadsT[,"hru"] <- gsub("Shortcut", "Connected", loadsT[,"hru"])
        loadsT[,"hru"] <- gsub("SC and Drained", "Connected and Drained", loadsT[,"hru"])
        loadsT[,"hru"] <- gsub("Rest", "Unconnected", loadsT[,"hru"])
        ter <- loadsT[,"load"]
        names(ter) <- loadsT[,"hru"]
        flux.atra.tot[[model.name]] = atr
        flux.terb.tot[[model.name]] = ter
	loads.det <- list(flux.atra.tot=flux.atra.tot, flux.terb.tot=flux.terb.tot)
        if(model.name=="MexpH4"){
            earl <- read.table("../output/modelcomparison/tt_early_eros0wtns0.txt", header=TRUE)
            late <- read.table("../output/modelcomparison/tt_late_eros0wtns0.txt", header=TRUE)
            app.hru.areas[["atra"]][[model.name]] <- c("Impervious"=earl["A1","AtraImperv"],
                                                       "Connected"=earl["A1","AtraConn"],
                                                       "Drained"=earl["A1","AtraDrng"],
                                                       "Connected and Drained"=earl["A1","AtraConnDrng"],
                                                       "Unconnected"=earl["A1","AtraDisconn"])
            app.hru.areas[["terb"]][[model.name]][["first"]] <- c("Impervious"=earl["A1","TerbImperv"],
                                                                  "Connected"=earl["A1","TerbConn"],
                                                                  "Drained"=earl["A1","TerbDrng"],
                                                                  "Connected and Drained"=earl["A1","TerbConnDrng"],
                                                                  "Unconnected"=earl["A1","TerbDisconn"])
            app.hru.areas[["terb"]][[model.name]][["second"]] <- c("Impervious"=late["A1","TerbImperv"],
                                                                   "Connected"=late["A1","TerbConn"],
                                                                   "Drained"=late["A1","TerbDrng"],
                                                                   "Connected and Drained"=earl["A1","TerbConnDrng"],
                                                                   "Unconnected"=late["A1","TerbDisconn"])
        }else if(model.name=="MprxH4"){
            earl <- read.table("../output/modelcomparison/tt_early_proxim2.txt", header=TRUE)
            late <- read.table("../output/modelcomparison/tt_late_proxim2.txt", header=TRUE)
            app.hru.areas[["atra"]][[model.name]] <- c("Impervious"=earl["A1","AtraImperv"],
                                                       "Connected"=earl["A1","AtraNear"],
                                                       "Drained"=earl["A1","AtraFar"],
                                                       "Unconnected"=earl["A1","AtraDisconn"])
            app.hru.areas[["terb"]][[model.name]][["first"]] <- c("Impervious"=earl["A1","TerbImperv"],
                                                                  "Connected"=earl["A1","TerbNear"],
                                                                  "Drained"=earl["A1","TerbFar"],
                                                                  "Unconnected"=earl["A1","TerbDisconn"])
            app.hru.areas[["terb"]][[model.name]][["second"]] <- c("Impervious"=late["A1","TerbImperv"],
                                                                   "Connected"=late["A1","TerbNear"],
                                                                   "Drained"=late["A1","TerbFar"],
                                                                   "Unconnected"=late["A1","TerbDisconn"])
        }else{
            app.hru.areas[["atra"]][[model.name]] <- NA
            app.hru.areas[["terb"]][[model.name]][["first"]] <- NA
            app.hru.areas[["terb"]][[model.name]][["second"]] <- NA
        }
    }
    return(list(loads.det=loads.det, app.hru.areas=app.hru.areas))
}
