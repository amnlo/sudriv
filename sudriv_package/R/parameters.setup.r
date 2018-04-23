parameters.setup <-
function(sudriv, with.names=FALSE, settings = "settings.json", replace.param=FALSE,
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings) != "list"){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    if(is.null(settings$file.par)){## get the paramters from the flexConfig file
        ##parameters <- info[["parDef"]]
        ## log-transformation
        ##parameters[which(info$parTran==1)] <- pmax(log(parameters[which(info$parTran==1)]), -20)
        stop("at the moment it is not yet possible to get the parameters from the flexConfig file...")
    }else{## alternatively, load parameters from file
        if(with.names){
            parameters <- sudriv.readpars.withnames(parfile=paste(settings$dir.input, "/", settings$file.par, sep=""))
        }else{
            parameters <- sudriv.readpars(parfile=paste(settings$dir.input, "/", settings$file.par, sep=""))
        }
    }
    ## use parameter names
    parnames <- read.table("parnames.txt", sep=",")
    units    <- sub("^\\s+", "", parnames[,2]) #trim leading whitespace and save units
    ## create vector that contains info about whether and how parameters are time-related
    time <- grepl("t", units)
    per.time <- grepl("/t", units)
    time[per.time] <- FALSE
    par.time <- rep(0, nrow(parnames))
    par.time[time] <- 1
    par.time[per.time] <- -1 # 0=not time-related, 1=..t, -1=../t

    parnames <- sub("^\\s+", "", parnames[,1]) #trim leading whitespace
    if(with.names){
        if(any(names(parameters) != parnames)){
            print(names(parameters))
            print(parnames)
            stop("different parameters / -names expected")
        }
    }
    names(parameters) <- parnames

    ## ==========================
    ## add parameters to model object:
    if(replace.param | is.null(sudriv$model$parameters))    sudriv$model$parameters <- parameters
    sudriv$model$par.units  <- units
    sudriv$model$par.time   <- par.time
    return(sudriv)
}
