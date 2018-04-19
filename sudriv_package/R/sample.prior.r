# Generate a random sample from the prior of a sudirv object

#' @param sudriv Object of class \code{sudriv} to which to append the prior definition
#' @return A numeric vector of random samples of the prior of the sudriv object

sample.prior <- function(sudriv, sampsize=1){
    nms <- c("dist", "mean", "sd", "cor", "distdef", "file")
    tol <- 1e-10
    samp.mod <- do.call(ransamp, args=c(sampsize=sampsize, sudriv$model$prior[nms]))$sample
    if(any(is.na(samp.mod))) stop(paste("sampled NA of parameter", which(is.na(samp.mod),arr.ind=TRUE)))
    samp.mod <- t(apply(samp.mod, 1, function(x) pmin(pmax(x, sudriv$model$args$parLo), sudriv$model$args$parHi)))
    samp.lik <- do.call(ransamp, args=c(sampsize=sampsize, sudriv$likelihood$prior[nms]))$sample
    if(any(is.na(samp.lik))) stop(paste("sampled NA of parameter", which(is.na(samp.lik),arr.ind=TRUE)))
    samp.lik <- t(apply(samp.lik, 1, function(x) pmin(pmax(x, sudriv$likelihood$lb), sudriv$likelihood$ub)))
    return(cbind(samp.mod, samp.lik))
}
