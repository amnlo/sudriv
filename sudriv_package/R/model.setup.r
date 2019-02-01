model.setup <-
function(sudriv, settings = "settings.json",writeO=TRUE,f.path.hru=NA,f.path.transK=NA,
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings) != "list"){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    ## ==================================================================
    ## Load DLL. Needs 'path.to.dll'
    dyn.load(settings$path.to.dll)
    info      <- prepare_dll(settings$modelID, writeO=writeO)
    outnames <- as.character(read.table("outnames.txt")[,1])
    outnames <- gsub("%", "_", outnames)
    outnames <- gsub("\\[", "", outnames)
    outnames <- gsub("\\]", "", outnames)
    parnames <- as.character(read.table("parnames.txt")[,1])
    parnames <- gsub(",", "", parnames)
    ##    run.engine <- get(settings$name.runengine.dll)
    parLo <- ifelse(as.logical(info[["parTran"]]), log(info[["parLo"]]), info[["parLo"]])
    parHi <- ifelse(as.logical(info[["parTran"]]), log(info[["parHi"]]), info[["parHi"]])
    ## ==================================================================
    ## Prepare arguments. Needs 'name.runmodel.dll', 'modelID'
    args <- list(   stateDef     =info[["stateDef"]],
                    parTran      =info[["parTran"]],
                    parLo        =parLo,
                    parHi        =parHi,
                    npar.det     =info[["npar"]],
                    nout         =as.integer(info[["nout"]]),
                    modelID      =settings[["modelID"]],
                    outnames     =info[["outnames"]])

    ## ==================================================================
    ## read HRU areas
    if(!is.na(f.path.hru)){
        hru.areas <- sudriv.readHRUs(f.path.hru)
    }else{
        hru.areas <- NULL
    }

    ## ==================================================================
    ## read info for transformation of K and alpha for better identifiability
    if(!is.na(f.path.transK)){
        transK <- sudriv.readtransK(f.path.transK)
    }else{
        transK <- NULL
    }

    ## ==========================
    ## construct model object:
    model <- list()
    model$run.model <- run.model
    model$run.engine <- run.engine
    model$args   <- args
    ##model$par.fit.psbl <- info[["parFit"]]
    if(is.null(sudriv$model$par.fit)){
        model$par.fit <- info[["parFit"]]
    }else{
        model$par.fit <- sudriv$model$par.fit
    }
    if(is.null(sudriv$model$outnames)){
        model$outnames <- outnames
    }else{
        model$outnames <- outnames
    }
    if(is.null(sudriv$model$parnames)){
        model$parnames <- parnames
    }else{
        model$parnames <- sudriv$model$parnames
    }
    model$parameters <- sudriv$model$parameters ## inherit existing model parameters (if present)
    model$prior      <- sudriv$model$prior      ## inherit existing prior for model parameters (if present)
    model$hru.areas  <- hru.areas
    model$transK     <- transK
    class(model) <- "model"
    sudriv$model <- model
    return(sudriv)
}
