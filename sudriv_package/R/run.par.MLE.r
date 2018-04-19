run.par.MLE <- function(nparallel, par.fit.psbl, npar.fit, fn.seq,  MLE.per.proc, su, ...){
##    .libPaths(lib.path)
    library(parallel)
    library(utils)
    ## n.MLE.tot <- MLE.per.proc * nparallel
    ## f <- which(as.logical(par.fit.psbl))
    ## npar.fit <- min(npar.fit, sum(par.fit.psbl))
    ## f.all <- combn(f, m=npar.fit)
    ## n.comb.psbl <- ncol(f.all)
    ## n.try.comb <- min(n.MLE.tot, n.comb.psbl)
    ## if(n.try.comb == 0){
    ##     cat("not enough time to run optimization once. Not doing anything...\n")
    ##     return(su)
    ## }
    ## cat("Running global inference for ", n.try.comb, " of ", n.comb.psbl, " possible cases.\n")
    ## cat("That's ", n.try.comb/n.comb.psbl*100, " % ...\n")
    ## shuffle <- sample(1:n.comb.psbl, size=n.comb.psbl, replace=FALSE)
    ## if(length(shuffle)>1){
    ##     f.all <- f.all[,shuffle]
    ##     f.all <- f.all[,1:n.try.comb]
    ## }
    ## par.fit.sample <- apply(f.all, 2, function(x) {a=rep(0,length(par.fit.psbl));a[x]<-1;return(a)})
    ## par.fit.sample <- as.list(as.data.frame(par.fit.sample))
    nsamp <- nparallel * MLE.per.proc # number of samples to draw
    samps <- sample.prior(su, sampsize=nsamp)
    if(any(is.na(samps))) stop("sampled NA as parameter")
    cat("initializing clusters...\n")
    cl = makeCluster(nparallel, methods = FALSE)
    cat("parallel calculation...\n")
    clusterSetRNGStream(cl = cl, iseed = NULL) # Random number generator on the cluster
    opts <- parApply(cl, samps, 1, fn.seq, su, ...) # This returns an array
    cat("parallel calculation terminated.\n")
    stopCluster(cl)
    cat("cluster closed.\n")
    opts <- t(opts)
    colnames(opts) <- c("Objective", "Niter", "Status", names(su$model$parameters), names(su$likelihood$parameters))
    return(opts)

    ## Gather results
    ## objective <- vector("numeric", length(sus))
    ## theta.opt <- matrix(nrow=length(sus), ncol=npar.fit)
    ## psi.opt   <- matrix(nrow=length(sus), ncol=sum(su$likelihood$par.fit))
    ## mssg      <- vector("character", length(sus))
    ## j <- 1
    ## for(su.curr in sus){
    ##     objective[j]  <- su.curr$MLE$opt$objective
    ##     mssg[j]       <- su.curr$MLE$opt$message
    ##     theta.opt[j,] <- su.curr$MLE$opt$solution[1:npar.fit]
    ##     if(sum(su$likelihood$par.fit) > 0) psi.opt[j,] <- su.curr$MLE$opt$solution[(npar.fit+1):length(su.curr$MLE$opt$solution)]
    ##     j <- j + 1
    ## }
    ## su$MLE$objective   <- objective
    ## su$MLE$mssg        <- mssg
    ## su$MLE$solution    <- theta.opt
    ## su$MLE$fitted.pars <- t(f.all)
    ## su$MLE$solution.psi <- psi.opt
    ## return(su)
}
