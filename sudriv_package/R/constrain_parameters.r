#' Transform unbounded values to bounded values. This is useful to turn a
#' constrained optimization problem into an unconstrained one.
#'
#' @param uncon Numeric vector of unconstrained values
#' @param lower_bound Numeric vector of lower bounds. Is recycled if shorter than \code{uncon}.
#' @param upper_bound Numeric vector of upper bounds. Is recycled if shorter than \code{uncon}.
#' @param trans Type of transformation to use.
#' @return A numerical vector of \code{uncon} transformed to be constrained by \code{lower_bound} and \code{upper_bound}
#'
constrain_parameters <- function(uncon, lower_bound, upper_bound, trans=c("logit")){
    if(trans[1] == "logit"){
        uncon.trns <- (uncon - (upper_bound + lower_bound)/2) / ((upper_bound-lower_bound)/2)
        par <- lower_bound + (upper_bound - lower_bound)/(1+exp(-uncon.trns))
    }
    return(par)
}
