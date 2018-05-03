#' Interpolate the output of Superflex to fit a certain layout
#'
#' @param res.sup Output from Superflex run (numeric vector)
#' @param ind.var Output from \code{result_index_var}, giving the index of the variables of interest.
#' @param layout Layout for which the interpolation should be made
#' @return A numerical vector containing the results of the Superflex run linearly interpolated to the times in \code{layout}.
result2layout <- function(res.sup, ind.var, layout, meas.point.val=FALSE){
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
        if(meas.point.val){# interpolate to the measured points in time, assuming that the model output was constant during the previous time step
            dt <- res.time[2]-res.time[1]
            res.time.ext <- c(res.time[1]-0.999*dt, c(rbind(res.time, res.time+0.001*dt)))
            res.time.ext <- res.time.ext[-length(res.time.ext)]
            res.var.ext  <- c(res.var[1], c(rbind(res.var, c(res.var[2:length(res.var)], NA))))
            res.var.ext <- res.var.ext[-length(res.var.ext)]
            y.out.curr  <- as.numeric(approx(x=res.time.ext, y=res.var.ext, xout=L[ind.L,2])$y)
        }else{
            y.out.curr  <- as.numeric(approx(x=res.time, y=res.var, xout=L[ind.L,2])$y) ## ATTENTION: Superflex returns the average of the streamflow during the most recent time step (t-dt,t), not the point value at t. If point values at t are required (e.g. because measurements are point values at t), this has to be interpolated here.
        }
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
