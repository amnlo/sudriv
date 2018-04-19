#' Interpolate the output of Superflex to fit a certain layout
#'
#' @param res.sup Output from Superflex run (numeric vector)
#' @param ind.var Output from \code{result_index_var}, giving the index of the variables of interest.
#' @param layout Layout for which the interpolation should be made
#' @return A numerical vector containing the results of the Superflex run linearly interpolated to the times in \code{layout}.
result2layout <- function(res.sup, ind.var, layout){
    L <- layout$layout
    lump <- layout$lump
    ## This is the vector of outputs considering the lumped measurements; it can be compared to the observations
    y.out <- vector("numeric", length=nrow(L))
    ## This is the vector of original model outputs; it can be used for plotting
    y.orig <- y.out
    for(var.curr in names(ind.var)){
        ind.L <- L[,1]==var.curr
        bounds.curr <- ind.var[[var.curr]]
        lump.curr <- lump[ind.L]
        res.var <- res.sup[["y"]][as.numeric(bounds.curr)[1]:as.numeric(bounds.curr)[2]]
        res.time <- res.sup[["time"]]
        y.out.curr  <- as.numeric(approx(x=res.time, y=res.var, xout=L[ind.L,2])$y) ## ATTENTION: Superflex returns the average of the streamflow during the most recent time step (t-dt,t), not the point value at t. If point values at t are required (e.g. because measurements are point values at t), this has to be interpolated here.
        y.orig.curr <- y.out.curr
        if(!is.na(lump.curr[1])){
            y.out.red <- tapply(y.out.curr, lump.curr, mean)
            ##            y.out.curr <- rep(y.out.red, times=as.numeric(tabulate(lump.curr)))
            y.out.curr <- as.numeric(y.out.red[lump.curr])
        }
        if(any(is.na(y.out.curr))) stop(paste("result2layout produced NAs for", var.curr))
        y.out[which(ind.L)]  <- as.numeric(y.out.curr)
        y.orig[which(ind.L)] <- as.numeric(y.orig.curr)
    }
    y.out <- list(incld.lmpd = y.out,
                  original   = y.orig)
    return(y.out)
}
