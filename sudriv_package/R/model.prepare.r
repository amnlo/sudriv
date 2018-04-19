model.prepare <- function(function.name, modelID){
    if(!is.loaded(function.name)) stop("function not found")
    result <- .Fortran(function.name, modelID=as.integer(modelID), err=as.integer(0))
    return(result)
}
