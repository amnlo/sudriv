select.best.MLE <- function(su){
    if(class(su$MLE$opt) == "nloptr"){ ## If the optimum is a single result of a refining process
        su$model$parameters[as.logical(su$model$par.fit)] <- su$MLE$opt$solution[1:sum(su$model$par.fit)]
        su$likelihood$parameters[as.logical(su$likelihood$par.fit)] <- su$MLE$opt$solution[(sum(su$model$par.fit)+1):length(su$MLE$opt$solution)]
    }else{
        if(is.null(su$MLE$objective)){
            cat("No MLE found ...\n")
            return(su)
        }
        max.obj <- match(min(su$MLE$objective), su$MLE$objective)## ATTENTION: this assumes that the objective contains the NEGATIVE likelihood (thus min() in stead of max())
        su$MLE$opt$objective <- min(su$MLE$objective) ## ATTENTION: see comment above
        su$MLE$opt$solution <- su$MLE$solution[max.obj,]
        par.fit <- rep(0, length(su$model$parameters))
        par.fit[su$MLE$fitted.pars[max.obj,]] <- 1
        su$MLE$opt$par.fit <- par.fit
        ## ATTENTION: model parameters are updated to the optimal ones. Initial parameters are lost...
        su$model$parameters[su$MLE$fitted.pars[max.obj,]] <- su$MLE$opt$solution
        ## So are the likelihood parameters
        su$likelihood$parameters[as.logical(su$likelihood$par.fit)] <- su$MLE$solution.psi
    }
    return(su)
}
