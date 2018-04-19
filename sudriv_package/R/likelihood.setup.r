likelihood.setup <-
function(sudriv, settings = "settings.json", replace.param=FALSE,
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings != "list")){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    par_likeli <- read.table(paste(settings$dir.input, "/par_likeli.txt", sep=""), header=TRUE)
    fit        <- as.numeric(par_likeli[,"fit"])

    par_likeli[,"fit"] <- NULL
    tran <- par_likeli[,"tran"]
    par_likeli[as.logical(tran),c("value","lb","ub")] <- log(par_likeli[as.logical(tran), c("value", "lb", "ub")])
    parameters <- par_likeli[,"value"]
    names(parameters) <- paste(par_likeli[,"var"], par_likeli[,"par"], "lik", sep="_")

    lb <- par_likeli[,"lb"]
    ub <- par_likeli[,"ub"]

    ## ==========================
    ## update sudriv object:
    sudriv$likelihood$f.likeli <- get(settings$f.likeli)
    sudriv$likelihood$f.sample <- get(settings$f.sample)
    if(replace.param | is.null(sudriv$likelihood$parameters))    sudriv$likelihood$parameters <- parameters
    sudriv$likelihood$par.fit<- fit
    sudriv$likelihood$lb     <- lb
    sudriv$likelihood$ub     <- ub
    sudriv$likelihood$tran   <- tran
    return(sudriv)
}
