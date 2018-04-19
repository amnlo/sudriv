run.seq.MLE <- function(par.ini, su, lib.path){
    .libPaths(lib.path)
    print(lib.path)
    print(.libPaths())
    library(rjson)
    library(nloptr)
    source("source.functions.r")
    wrk.dir <- getwd()
    source.functions(wrk.dir)

    ##if(!is.na(par.fit[1])) su$model$par.fit <- par.fit
    par.ini.model <- par.ini[1:length(su$model$parameters)]
    par.ini.likeli<- par.ini[(length(su$model$parameters)+1):length(par.ini)]
    su$model$parameters[as.logical(su$model$par.fit)] <- par.ini.model[as.logical(su$model$par.fit)]
    su$likelihood$parameters[as.logical(su$likelihood$par.fit)] <- par.ini.likeli[as.logical(su$likelihood$par.fit)]
    su <- model.setup(su, settings="settings.json", writeO=FALSE)
    su <- MLE.sudriv(su)
    su <- select.best.MLE(su)
    res <- c(su$MLE$opt$objective, su$MLE$opt$iterations, su$MLE$opt$status, su$model$parameters, su$likelihood$parameters)
    return(as.numeric(res))
    ## develop: run on one processor was successful. Implement a parallel version where one MLE is run on each processor. We could vary the (number of) parameters to be inferred. Also: start with global optimization and once this is sucessfully finished, refine the result with local optimization.
}
