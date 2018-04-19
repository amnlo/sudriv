multistart.get <- function(fnames){
    i <- 1
    for(f.curr in fnames){
        ta.new <- read.table(f.curr)
        colnames(ta.new) <- as.character(t(ta.new[1,]))
        ta.new <- ta.new[-1,]
        if(i == 1){
            ta <- ta.new
        }else{
            ta <- rbind(ta, ta.new)
        }
        i <- i + 1
    }
    ta <- t(apply(ta, 1, as.numeric))
    ta <- data.frame(ta)
    colnames(ta) <- colnames(ta.new)
    ta <- ta[,-c(2,3)]
    ## sort according to objective
    ta <- ta[order(ta[,1]),]
    return(ta)
}

make.gg2.df <- function(ms, fitted=NA){
    ## data frame for ggplot:
    if(!is.na(fitted[1])) ms <- ms[,c(TRUE,fitted)]
    ta <- data.frame(obj = rep(as.numeric(ms[,1]), ncol(ms)-1), p.value = as.numeric(unlist(ms[,2:ncol(ms)])), p.name = rep(colnames(ms)[2:ncol(ms)], each=nrow(ms)))
    return(ta)
}

select.pars.multistart <- function(sudriv, ms, index=NA){
    if(is.na(index)){
        ind.opt <- which.min(ms$Objective)
    }else{
        ind.opt <- index
    }
    opt.pars <- ms[ind.opt,]
    opt.pars <- as.numeric(t(opt.pars))
    names(opt.pars) <- colnames(ms)
    opt.pars <- opt.pars[2:length(opt.pars)]
    npar.mod <- sudriv$model$args$npar.det
    npar.lik <- length(sudriv$likelihood$parameters)
    if(length(opt.pars) != npar.mod + npar.lik) stop("object 'sudriv' and result 'ms' have inconsistent parameter dimensions")
    sudriv$model$parameters <- opt.pars[1:npar.mod]
    sudriv$likelihood$parameters <- opt.pars[(npar.mod+1):length(opt.pars)]
    return(sudriv)
}

find.special.pars <- function(ms, index=NA){
    pars <- ms[,which(colnames(ms) == "Glo%Cmlt_P"):ncol(ms)]
    if(is.na(index)){
        ind.opt <- which.min(ms$Objective)
    }else{
        ind.opt <- index
    }
    opt.pars <- pars[ind.opt,]
    opt.pars <- as.numeric(t(opt.pars))
    names(opt.pars) <- colnames(pars)
    ## construct empirical cumulative distribution functions
    empirical.cdfs <- apply(pars, 2, ecdf)
    ## insert the best parameter values into the respective epirical cdfs
    cval <- rep(NA, ncol(pars))
    for(i in 1:length(cval)){
        cval[i] <- empirical.cdfs[[i]](opt.pars[i])
    }
    names(cval) <- colnames(pars)
    return(cval)
}
