## This script contains functions for some likelihoods suited for hydrological problems

logposterior.neg <- function(x0, sudriv, prior=TRUE){
    ## =======================================================
    ## update the likelihood parameters with the ones from x0
    flp <- sudriv$likelihood$par.fit
    l.fit.lik <- sum(flp)
    if(l.fit.lik > 0){
        par.lik.fit <- x0[(length(x0)-l.fit.lik+1):length(x0)]
        ## make sure they are within the bounds
        if(!sudriv$settings$OPT.bounded){
            lower_bound <- sudriv$likelihood$lb[as.logical(flp)]
            upper_bound <- sudriv$likelihood$ub[as.logical(flp)]
            par.lik.fit <- constrain_parameters(par.lik.fit, lower_bound, upper_bound)
        }
        ## update likelihood parameters
        sudriv$likelihood$parameters[which(flp != 0)] <- par.lik.fit
    }

    ## =======================================================
    ## update model parameters with the ones from x0
    fmp <- sudriv$model$par.fit
    par.mod.fit <- x0[1:(length(x0)-l.fit.lik)]
    ## make sure they are within the bounds
    if(!sudriv$settings$OPT.bounded){
        lower_bound <- sudriv$model$args$parLo[as.logical(fmp)]
        upper_bound <- sudriv$model$args$parHi[as.logical(fmp)]
        par.mod.fit <- constrain_parameters(par.mod.fit, lower_bound, upper_bound)
    }
    ## update model parameters
    sudriv$model$parameters[which(fmp != 0)] <- par.mod.fit

    ## =======================================================
    ## prepare arguments for the likelihood
    likeli.args           <- list()
    likeli.args$par.model <- sudriv$model$parameters
    likeli.args$run.model <- run.model
    likeli.args$layout    <- sudriv$layout
    likeli.args$y.obs     <- sudriv$observations
    likeli.args$par.likeli<- ifelse(as.logical(sudriv$likelihood$tran), exp(sudriv$likelihood$parameters), sudriv$likelihood$parameters)
    names(likeli.args$par.likeli) <- names(sudriv$likelihood$parameters)
    likeli.args$sudriv    <- sudriv
    f.likeli <- sudriv$likelihood$f.likeli

    ## =======================================================
    ## calculate loglikelihood
    loglikeli <- do.call(f.likeli, likeli.args)

    ## =======================================================
    ## calculate logprior
    if(prior){
        args.pdf.model       <- c(list(z=as.numeric(sudriv$model$parameters)), sudriv$model$prior)
        logpri.modelpar      <- do.call(calcpdf_mv, args.pdf.model)
        args.pdf.likeli      <- c(list(z=as.numeric(sudriv$likelihood$parameters)), sudriv$likelihood$prior)
        logpri.likelipar     <- do.call(calcpdf_mv, args.pdf.likeli)
    }else{
        logpri.likelipar <- 0
        logpri.modelpar  <- 0
    }

    ## =======================================================
    ## calculate logposterior
    logpost <- loglikeli + logpri.likelipar + logpri.modelpar
    return(-1*logpost)
}



Loglikeli.ols <- function(par, model, layout, lump, y.obs, engine, result_index_var, result2layout, args.engine, log.var=FALSE, fixed.variance=NA){
    ## This is the sum of squared residual function
    y.mod <- model(par=par, layout=layout, lump=lump, engine=engine, result_index_var=result_index_var, result2layout=result2layout, args.engine=args.engine)
    na.y.obs <- is.na(y.obs)
    vars <- unique(layout[,1])
    loglikeli <- numeric(length=nrow(layout))
    for(var.curr in vars){
        if(var.curr %in% names(fixed.variance)){
            var.eps <- fixed.variance[[var.curr]]
            if(log.var) var.eps <- exp(var.eps)
        }else{
            vname <- paste("var_eps_",var.curr,sep="")
            var.eps <- par[[vname]]
            if(log.var) var.eps <- exp(var.eps)
        }
        if(var.eps <= 0){
            loglikeli <- -Inf
        }else{
            ind.var <- layout[,1]==var.curr
            sr <- (sqrt(y.obs[ind.var]) - sqrt(y.mod[ind.var]))^2 / var.eps
            loglikeli[ind.var] <- -0.5*log(2*pi*var.eps) - 0.5*sr
        }
    }
    loglikeli[na.y.obs] <- 0
    cat("loglikeli: ", sum(loglikeli), "\n")
    loglikeli <- -1*sum(loglikeli)
    return(loglikeli)
}

Loglikeli.wls <- function(par.model, run.model, layout, y.obs, par.likeli, ...){
    L = layout$layout
    y.mod <- as.numeric(run.model(par=par.model, layout=layout, ...)$incld.lmpd)
    if(any(is.na(y.mod))) stop("y.mod contains nas")
    na.y.obs <- is.na(y.obs)
    vars <- unique(L[,1])
    loglikeli <- numeric(length=nrow(L))
    const <- log(1/sqrt(2*pi))
    for(var.curr in vars){
        d  <- par.likeli[paste(var.curr, "_d_lik", sep = "")] * par.likeli["GLOB_MULT_d_lik"]
        e  <- par.likeli[paste(var.curr, "_e_lik", sep = "")]
        c  <- par.likeli[paste(var.curr, "_c_lik", sep = "")]
        ind.var <- L[,1]==var.curr
        dt <- diff(as.numeric(L[ind.var,2]))
        dt <- c(dt[1], dt)
        weights <- pmin(dt, quantile(dt, 0.9))
        sds <- d*(e + y.mod[ind.var])^c
        if(grepl("C.*Wv_", var.curr)){
            loglikeli[ind.var] <- weights/sum(weights) * (const + log(1/sds) - (y.mod[ind.var]-y.obs[ind.var])^2/(2*sds^2))
        }else{
            loglikeli[ind.var] <- 1/sum(ind.var) * (const + log(1/sds) - (y.obs[ind.var]-y.mod[ind.var])^2/(2*sds^2))
        }
    }
    loglikeli[na.y.obs] <- 0
    cat("loglikelis:\n")
    print(tapply(loglikeli, L[,1], sum))
    return(sum(loglikeli))
}

Loglikeli.wls2 <- function(par.model, run.model, layout, y.obs, par.likeli, ...){
    L = layout$layout
    y.mod <- as.numeric(run.model(par=par.model, layout=layout, ...)$incld.lmpd)
    if(any(is.na(y.mod))) stop("y.mod contains nas")
    na.y.obs <- is.na(y.obs)
    vars <- unique(L[,1])
    loglikeli <- numeric(length=nrow(L))
    const <- log(1/sqrt(2*pi))
    dt <- diff(as.numeric(L[,2]))
    dt <- c(dt[1], dt)
    weights <- pmin(dt, quantile(dt, 0.9))
    for(var.curr in vars){
        alpha <- par.likeli[paste(var.curr, "_alpha", sep = "")]
        beta  <- par.likeli[paste(var.curr, "_beta", sep = "")]
        gamma <- par.likeli[paste(var.curr, "_gamma", sep = "")]
        ind.var <- L[,1]==var.curr
        sds <- (alpha + beta*y.mod[ind.var])^gamma
        if(var.curr == "C1Wv_Qstream"){
            loglikeli[ind.var] <- weights[ind.var]/sum(weights[ind.var]) * (y.mod[ind.var] - y.obs[ind.var])/nothing
        }else{
            loglikeli[ind.var] <- 1/sum(ind.var) * -1 * ((y.mod[ind.var] - y.obs[ind.var])^2/sds^2)
        }
    }
    loglikeli[na.y.obs] <- 0
    return(sum(loglikeli))
}

## Peters Likelihood:
LogLikelihoodHydrologyMV <- function(par, model, layout, lump, y.obs, psi, engine, result_index_var, result2layout, LogLikelihoodHydrology.fast, args.engine, fit.psi=NA){
    variables <- unique(layout[,1])
    loglik <- vector("numeric",length=length(variables))
##    par.engine <- as.numeric(par.all[grep("^p[0-9]*", names(par.all))])
    y.mod <- model(par=par, layout=layout, lump=lump, engine=engine, result_index_var=result_index_var, result2layout=result2layout, args.engine=args.engine)
    i = 1
    for(var.curr in variables){
        ind.var <- layout[,1]==var.curr
        if(!is.na(fit.psi[1])){
            if("alpha" %in% fit.psi) psi[[var.curr]]["alpha"] <- par[paste(var.curr,"_alpha",sep="")]
            if("beta"  %in% fit.psi) psi[[var.curr]]["beta"]  <- par[paste(var.curr,"_beta",sep="")]
            if("gamma" %in% fit.psi) psi[[var.curr]]["gamma"] <- par[paste(var.curr,"_gamma",sep="")]
        }
        loglik[i] <- LogLikelihoodHydrology.fast(Q.obs=y.obs[ind.var],Q.det=y.mod[ind.var],t=layout[ind.var,2],psi=psi[[var.curr]])
        i = i + 1
    }
    cat("loglik: ", sum(loglik), "\n")
   return(-1*sum(loglik))
}


LogLikelihoodHydrology <- function(Q.obs,Q.det,t,psi)
{
  # consistency checks:

  if ( length(t) != length(Q.det) ) { cat("*** length of Q.det not equal to length of t\n"); return(NA) }
  if ( length(t) != length(Q.obs) ) { cat("*** length of Q.obs not equal to length of t\n"); return(NA) }
  if ( length(psi) != 4 ) { cat("*** length of psi not equal to 4: need tau, alpha, beta, gamma\n"); return(NA) }

  # improve readability by assigning parameter names:

  n       <- length(Q.det)
  tau     <- psi[1]
  alpha   <- psi[2]
  beta    <- psi[3]
  gamma   <- psi[4]

  # evaluate lognormal-based log likelihood:

  mean    <- Q.det
  sd      <- alpha*(Q.det+beta)^gamma

  z       <- 1+sd*sd/(mean*mean)
  meanlog <- log(mean) - 0.5*log(z)
  sdlog   <- sqrt(log(z))

  eta <- rep(NA,n)
  for ( i in 1:n )
  {
    eta[i] <- qnorm(plnorm(Q.obs[i],meanlog=meanlog[i],sdlog=sdlog[i]),mean=0,sd=1)
  }

  loglikeli <- dlnorm(Q.obs[1],meanlog=meanlog[1],sdlog=sdlog[1],log=TRUE)
  if ( n > 1 )
  {
    for ( i in 2:n )
    {
      dt.tau <- (t[i]-t[i-1])/tau
      loglikeli <- loglikeli +
                     dlnorm(Q.obs[i],meanlog=meanlog[i],sdlog=sdlog[i],log=TRUE) +
                     dnorm(eta[i],mean=eta[i-1]*exp(-dt.tau),sd=sqrt(1-exp(-2*dt.tau)),log=TRUE) -
                     dnorm(eta[i],mean=0,sd=1,log=TRUE)
    }
  }
  return(loglikeli)
}

LogLikelihoodHydrology.fast <- function(Q.obs,Q.det,t,psi)
{
  # consistency checks:

  if ( length(t) != length(Q.det) ) { cat("*** length of Q.det not equal to length of t\n"); return(NA) }
  if ( length(t) != length(Q.obs) ) { cat("*** length of Q.obs not equal to length of t\n"); return(NA) }
  if ( length(psi) != 6) { cat("*** length of psi not equal to 6: need tau, alpha, beta, gamma, m.min, sd.min\n"); return(NA) }
  ##if(any(Q.det == 0)) return(-Inf)
    if(any(is.na(Q.det))) stop("Q.det contains NAs")
  # improve readability by assigning parameter names:

    n       <- length(Q.det)
    tau     <- psi[1]
    alpha   <- psi[2]
    beta    <- psi[3]
    gamma   <- psi[4]
    m.min   <- psi[5]
    sd.min  <- psi[6]

    nas.obs <- is.na(Q.obs)

                                        # evaluate lognormal-based log likelihood:
    m       <- Q.det ##pmax(Q.det, m.min) ##+ m.min*exp(-1*(Q.det/m.min))
    sd      <- alpha*(Q.det+beta)^gamma ##pmax(alpha*(Q.det+beta)^gamma, sd.min)
    z       <- 1+sd*sd/(m*m)
    meanlog <- log(m) - 0.5*log(z)
    sdlog   <- sqrt(log(z))
    eta     <- qnorm(plnorm(Q.obs,meanlog=meanlog,sdlog=sdlog),mean=0,sd=1)
    loglikeli <- dlnorm(Q.obs[1],meanlog=meanlog[1],sdlog=sdlog[1],log=TRUE)
    if(nas.obs[1]) loglikeli <- 0
  if ( n > 1 )
  {
      dt.tau <- diff(t)/tau
      dobs  <- dlnorm(Q.obs[2:n], meanlog=meanlog[2:n], sdlog=sdlog[2:n], log=TRUE)
      dres1 <- dnorm(eta[2:n], mean=eta[1:(n-1)]*exp(-dt.tau), sd=sqrt(1-exp(-2*dt.tau)), log=TRUE)
      dres2 <- dnorm(eta[2:n], mean=0, sd=1, log=TRUE)
      dtot  <- dobs + dres1 - dres2
      dtot[nas.obs[2:n]] <- 0
      infs <- is.na(dtot) | is.infinite(dtot)
      dtot[is.na(dtot)] <- -1.0e+5
      ##      dtot[is.infinite(dtot)] <- -1000
      dtot <- pmax(dtot, -1.0e+5)
      cat("number of infs: ", sum(infs), "\n")
      loglikeli <- ifelse(is.infinite(loglikeli), -1e+20, loglikeli) + sum(dtot)
  }
  return(loglikeli)
}

LogLikelihoodHydrology.fast2 <- function(Q.obs,Q.det,t,psi2)
{
  # consistency checks:

  if ( length(t) != length(Q.det) ) { cat("*** length of Q.det not equal to length of t\n"); return(NA) }
  if ( length(t) != length(Q.obs) ) { cat("*** length of Q.obs not equal to length of t\n"); return(NA) }
  if ( length(psi) != 4 ) { cat("*** length of psi not equal to 4: need tau, alpha, beta, gamma\n"); return(NA) }

  # improve readability by assigning parameter names:

  n       <- length(Q.det)
  tau     <- psi[1]
  alpha   <- psi[2]
  beta    <- psi[3]
  gamma   <- psi[4]
                                        # evaluate lognormal-based log likelihood:
    mu    <- Q.det
    sigma <- alpha*(Q.det+beta)^gamma
    nu <- 1/mu
    eta <- qnorm(pSEP(Q.obs,mu=mu,sigma=sigma,nu=nu,tau=tau),mean=0,sd=1)
    if(any(is.infinite(eta) | is.na(eta))){
        warning("machine presicion lead to infinite eta")
        infi <- is.infinite(eta) | is.na(eta)
        eta[infi] <- 5
        ##return(-Inf)
    }
  loglikeli <- dSEP(Q.obs[1],mu=mu[1],sigma=sigma[1],log=TRUE)
  if ( n > 1 )
  {
      dt.tau <- diff(t)/tau
      loglikeli <- loglikeli + sum(dSEP(Q.obs[2:n], mu=mu[2:n], sigma=sigma[2:n], log=TRUE) +
                                   dnorm(eta[2:n], mean=eta[1:(n-1)]*exp(-dt.tau), sd=sqrt(1-exp(-2*dt.tau)), log=TRUE) -
                                   dnorm(eta[2:n], mean=0, sd=1, log=TRUE), na.rm=TRUE)
  }
  return(loglikeli)
}
