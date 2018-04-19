optimizer.setup <-
function(sudriv, settings = "settings.json",
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings != "list")){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    ## extract list of options for the optimizer
    mle <- grep("OPT.opts.", names(settings))
    l <- settings[mle]
    names(l) <- sub("OPT.opts.", "", names(settings)[mle])

    ## ==========================
    ## update sudriv object:
    sudriv$settings$OPT.opts <- l
    sudriv$settings$OPT.bounded <- settings$OPT.bounded
    sudriv$settings$OPT.time.tot <- settings$OPT.time.tot
    return(sudriv)
}
