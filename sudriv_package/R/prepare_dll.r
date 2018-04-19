#' Load the DLL for Superflex and prepare the model
#'
#' @path.to.dll The path to the DLL file
#' @return The list of information provided by the model info function of Superflex
#'
prepare_dll <- function(modelID, writeO=TRUE){
    ## Prepare the model
    cat("preparing model...\n")
    junk <- model.prepare("preparemodel", modelID=modelID)
    junk <- NULL
    ## Get dimensions of model
    cat("getting model dimensions...\n")
    dimensions <- model.getdimensions("getdimensions", modelID=modelID)
    ## Read parameter info
    cat("gettting model info...\n")
    info <- model.getmodelinfo("getmodelinfo", modelID=modelID, ninp=dimensions[["ninp"]], npar=dimensions[["npar"]], nout=dimensions[["nout"]], writeO=as.integer(writeO))
    return(info)
}

model.prepare <- function(function.name, modelID){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), err=as.integer(0))
    return(result)
}


model.getdimensions <- function(function.name, modelID){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), ninp=as.integer(1), npar=as.integer(1), nout=as.integer(1))
    return(result)
}


model.getmodelinfo <- function(function.name, modelID, ninp, npar, nout, writeO=as.integer(1)){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), ninp=as.integer(ninp), npar=as.integer(npar), nout=as.integer(nout), parLo=vector("numeric", npar), parHi=vector("numeric", npar), stateDef=vector("numeric", nout), parDef=vector("numeric", npar), parTran=vector("integer", npar), parFit=vector("integer", npar), writeO=as.integer(writeO))##, outnames="nothing")
    result$parFit <- abs(result$parFit)
    ## subu1 <- gsub("U1", ";U1", result$outnames)
    ## subc1 <- gsub("C1", ";C1", subu1)
    ## subsp <- gsub(" ", "", subc1)
    ## sub01 <- gsub("][^;]*", "]", subsp)
    ## outnames <- unlist(strsplit(sub01, split=";"))
    ## add.brack <- unlist(strsplit(outnames[length(outnames)],split=""))
    ## if(add.brack[length(add.brack)] != "]"){
    ##     add.brack <- paste(outnames[length(outnames)],"]",sep="")
    ## }
    ## outnames[length(outnames)] <- add.brack
    ## result$outnames <- outnames[-1]
    return(result)
}
