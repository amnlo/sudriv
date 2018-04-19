model.getdimensions <- function(function.name, modelID){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), ninp=as.integer(1), npar=as.integer(1), nout=as.integer(1))
    return(result)
}
