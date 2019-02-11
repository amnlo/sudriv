#' Construct a prior distribution for the parameters of the model
#'
#' @param sudriv Object of class \code{sudriv} to which to append the prior definition
#' @param dist  Distribution type: "Normal", "Lognormal" or "Indep".
#' @param mean  vector of means
#' @param sd    vector of standard deviations
#' @param cor   correlation matrix of the distribution
#' @param cor.inv inverse of correlation matrix of the distribution (alternative input to cor, saves computation time for repeated calls)
#' @param log   if TRUE returns log of pdf, otherwise pdf
#' @param distdef distribution definition for independent 1d distributions
#' @return An object of class \code{sudriv}

## prior.setup <- function(sudriv,dist="normal",mean=0,sd=1,cor=0,
##                         cor.inv=NA,log=TRUE,distdef=NA,file=NA){

##     sudriv <- prior.model.setup(sudriv,dist=dist,mean=mean,sd=sd,cor=cor,
##                                 cor.inv=cor.inv,log=log,distdef=distdef,file=file)
##     sudriv <- prior.likeli.setup(sudriv,dist=dist,mean=mean,sd=sd,cor=cor,
##                           cor.inv=cor.inv,log=log,distdef=distdef,file=file)
##     return(sudriv)
## }

prior.model.setup <- function(sudriv, dist="normal",mean=0,sd=1,cor=0,
                          cor.inv=NA,log=TRUE,distdef=NA,file=NA){
    ## Checks and preparation ===================================================
    if(is.null(sudriv$model)) stop("prior.model.setup: cannot set up priors if no model is defined...")
    npar <- sudriv$model$args$npar.det # nuber of model parameters
    mean <- as.vector(mean)
    sd <- as.vector(sd)
    if (length(sd) != length(mean)) stop("setup.prior.model: illegal dimension of standard deviations")
    R <- diag(rep(1,npar))
    if ( is.matrix(cor) ) R <- cor
    if ( nrow(R) != npar || ncol(R) != npar ) stop("setup.prior.model: illegal dimension of correlation matrix")

    ## Add prior definition to sudriv object
    sudriv$model$prior$dist <- dist
    sudriv$model$prior$mean <- mean
    sudriv$model$prior$sd   <- sd
    sudriv$model$prior$cor  <- cor
    sudriv$model$prior$cor.inv <- cor.inv
    sudriv$model$prior$log  <- log
    sudriv$model$prior$distdef <- distdef
    sudriv$model$prior$file <- file
    return(sudriv)
}

prior.likeli.setup <- function(sudriv, dist="normal",mean=0,sd=1,cor=0,
                          cor.inv=NA,log=TRUE,distdef=NA,file=NA){
    ## Checks and preparation ===================================================
    if(is.null(sudriv$likeli)) stop("prior.likeli.setup: cannot set up priors if no likelihood is defined...")
    npar <- length(sudriv$likelihood$parameters) # nuber of likelihood parameters
    mean <- as.vector(mean)
    sd <- as.vector(sd)
    if (length(sd) != length(mean)) stop("prior.likeli.setup: illegal dimension of standard deviations")
    R <- diag(rep(1,npar))
    if ( is.matrix(cor) ) R <- cor
    if ( nrow(R) != npar || ncol(R) != npar ) stop("prior.likeli.setup: illegal dimension of correlation matrix")

    ## Add prior definition to sudriv object
    sudriv$likelihood$prior$dist <- dist
    sudriv$likelihood$prior$mean <- mean
    sudriv$likelihood$prior$sd   <- sd
    sudriv$likelihood$prior$cor  <- cor
    sudriv$likelihood$prior$cor.inv <- cor.inv
    sudriv$likelihood$prior$log  <- log
    sudriv$likelihood$prior$distdef <- distdef
    sudriv$likelihood$prior$file <- file
    return(sudriv)
}

prior.hyper.setup <- function(sudriv, dist="normal",mean=0,sd=1,cor=0,
                          cor.inv=NA,log=TRUE,distdef=NA,file=NA){
    ## Checks and preparation ===================================================
    mean <- as.vector(mean)
    sd <- as.vector(sd)
    if (length(sd) != length(mean)) stop("prior.likeli.setup: illegal dimension of standard deviations")

    ## Add prior definition to sudriv object
    sudriv$hyperparameters$prior$dist <- dist
    sudriv$hyperparameters$prior$mean <- mean
    sudriv$hyperparameters$prior$sd   <- sd
    sudriv$hyperparameters$prior$log  <- log
    sudriv$hyperparameters$prior$distdef <- distdef
    sudriv$hyperparameters$prior$file <- file
    return(sudriv)
}
