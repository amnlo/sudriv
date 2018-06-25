likelihood.setup <-
function(sudriv, settings = "settings.json", replace.param=FALSE,
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings != "list")){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    par_likeli <- read.table(paste(settings$dir.input, "/", settings$file.par.lik, sep=""), header=TRUE)
    # if layout has more variables than par_likeli, take the values for the first variable and repeat them for all the others:
    var.layout <- unique(sudriv$layout$layout$var)
    var.pars   <- unique(par_likeli$var[!grepl("GLOB_", par_likeli$var)])
    warning("layout contains more variables than likelihood parameter file. Repeating those for all varialbes ...")
    first.var <- subset(par_likeli, var=="C1Wv_Qstream") # select par likeli of first variable (hard coded)
    par_likeli_long <- do.call("rbind", replicate(length(var.layout), first.var, simplify=FALSE))
    par_likeli_long$var <- rep(var.layout, each=nrow(first.var))
    ## use additional likelihood parameters of other variables, if specified:
    if(length(var.pars) > 1){
        other.entries <- subset(par_likeli, var!="C1Wv_Qstream" & !grepl("GLOB", var))
        for(z in 1:nrow(other.entries)){
            slot.replace <- par_likeli_long[,"var"]==other.entries[z,"var"] & par_likeli_long[,"par"]==other.entries[z,"par"]
            if(sum(slot.replace)==0) stop("variable or parameter in likelihood parameter file not found")
            par_likeli_long[slot.replace,] <- other.entries[z,]
        }
    }
    ## add global multipliers again:
    par_likeli <- rbind(subset(par_likeli, grepl("GLOB_", var)), par_likeli_long)
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
