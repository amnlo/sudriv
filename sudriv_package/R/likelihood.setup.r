likelihood.setup <-
function(sudriv, settings = "settings.json", replace.param=FALSE,
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings != "list")){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    par_likeli <- read.table(paste(settings$dir.input, "/plik_", settings$subcatchment, settings$tracer, "_", settings$par.likeli.tag, ".txt", sep=""), header=TRUE)
    # if layout has more variables than par_likeli, repeat the values of par_likeli for all variables
    n.var.layout <- length(unique(sudriv$layout$layout$var))
    n.var.pars   <- length(unique(par_likeli$var[!grepl("GLOB_", par_likeli$var)]))
    if(n.var.pars < n.var.layout){
        warning("layout contains more variables than likelihood parameter file. Repeating those for all varialbes ...")
        if(n.var.layout %% n.var.pars !=0) stop("variables in layout not a multiple of variables in parameter file")
        par_likeli_long <- par_likeli
        for(i in 2:(n.var.layout/n.var.pars)){
            pars.curr <- par_likeli[!grepl("GLOB_", par_likeli$var),]
            pars.curr$var <- unique(sudriv$layout$layout$var)[i]
            par_likeli_long <- rbind(par_likeli_long, pars.curr)
        }
        par_likeli <- par_likeli_long
    }
    fit        <- as.numeric(par_likeli[,"fit"])

    par_likeli[,"fit"] <- NULL
    tran <- par_likeli[,"tran"]
    par_likeli[as.logical(tran),c("value","lb","ub")] <- log(par_likeli[as.logical(tran), c("value", "lb", "ub")])
    parameters <- par_likeli[,"value"]
    names(parameters) <- paste(par_likeli[,"var"], par_likeli[,"par"], "lik", sep="_")

    lb <- par_likeli[,"lb"]
    ub <- par_likeli[,"ub"]
    units <- par_likeli[,"units"]
    ## create vector that contains info about whether and how parameters are time-related
    time <- grepl("t", units)
    per.time <- grepl("/t", units)
    time[per.time] <- FALSE
    par.time <- rep(0, nrow(par_likeli))
    par.time[time] <- 1
    par.time[per.time] <- -1 # 0=not time-related, 1=..t, -1=../t

    ## ==========================
    ## update sudriv object:
    sudriv$likelihood$f.likeli <- get(settings$f.likeli)
    sudriv$likelihood$f.sample <- get(settings$f.sample)
    if(replace.param | is.null(sudriv$likelihood$parameters))    sudriv$likelihood$parameters <- parameters
    sudriv$likelihood$par.fit<- fit
    sudriv$likelihood$lb     <- lb
    sudriv$likelihood$ub     <- ub
    sudriv$likelihood$tran   <- tran
    sudriv$likelihood$par.units   <- units
    sudriv$likelihood$par.time<- par.time
    return(sudriv)
}
