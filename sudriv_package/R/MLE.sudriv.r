MLE.sudriv <- function(sudriv, prior=TRUE){ ## performs maximum likelihood estimation on a sudriv object through optimization.
    fit         <- c(sudriv$model$par.fit, sudriv$likelihood$par.fit)
    par.ini     <- as.numeric(c(sudriv$model$parameters, sudriv$likelihood$parameters))[as.logical(fit)] ## initial parameters
    lb <- rep(-Inf, length(par.ini))
    ub <- rep(Inf, length(par.ini))
    if(sudriv$settings$OPT.bounded){
        lb <- c(sudriv$model$args$parLo, sudriv$likelihood$lb)[as.logical(fit)]
        ub <- c(sudriv$model$args$parHi, sudriv$likelihood$ub)[as.logical(fit)]
    }
    on.lb <- par.ini <= lb
    if(sum(on.lb) > 0){
        par.ini[on.lb] <- par.ini[on.lb] + 1e-9
    }
    on.ub <- par.ini >= ub
    if(sum(on.ub) > 0){
        par.ini[on.ub] <- par.ini[on.ub] - 1e-9
    }
    if(any(par.ini <= lb | par.ini >= ub)){
        vb <- which(par.ini <= lb | par.ini >= ub)
        cat("parameters ", vb, " are out of bounds \n")
        cat("par values:\n")
        cat(par.ini[vb], "\n")
        cat("upper bounds:\n")
        cat(ub[vb], "\n")
        cat("lower bounds: \n")
        cat(lb[vb], "\n")
    }
    optimized <- nloptr(x0               = par.ini,
                        eval_f           = logposterior.neg,
                        lb               = lb,
                        ub               = ub,
                        opts             = sudriv$settings$OPT.opts,
                        sudriv           = sudriv,
                        prior            = prior)
    sudriv$MLE$opt <- optimized
    return(sudriv)
}
