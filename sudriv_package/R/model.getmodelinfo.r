model.getmodelinfo <- function(function.name, modelID, ninp, npar, nout){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), ninp=as.integer(ninp), npar=as.integer(npar), nout=as.integer(nout), parLo=vector("numeric", npar), parHi=vector("numeric", npar), stateDef=vector("numeric", nout), parDef=vector("numeric", npar), parTran=vector("integer", npar), parFit=vector("integer", npar))##, outnames="nothing")
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
