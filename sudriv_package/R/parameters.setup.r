parameters.setup <-
function(sudriv, with.names=FALSE, settings = "settings.json",
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
    parnames <- read.table("parnames.txt", sep=",")
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
    sudriv$model$parameters <- parameters
    return(sudriv)
}
