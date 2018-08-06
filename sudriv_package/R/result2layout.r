#' Interpolate the output of Superflex to fit a certain layout
#'
#' @param res.sup Output from Superflex run (numeric vector)
#' @param ind.var Output from \code{result_index_var}, giving the index of the variables of interest.
#' @param layout Layout for which the interpolation should be made
#' @return A numerical vector containing the results of the Superflex run linearly interpolated to the times in \code{layout}.
result2layout <- function(res.sup, ind.var, layout, meas.point.val=FALSE, lump=TRUE){
    L <- layout$layout
    ## This is the vector of outputs considering the lumped measurements; it can be compared to the observations
    y.out <- vector("numeric", length=nrow(L))
    if(meas.point.val){
            dt <- res.sup[["time"]][2]-res.sup[["time"]][1]
            res.time.ext <- c(res.sup[["time"]][1]-0.999*dt, c(rbind(res.sup[["time"]], res.sup[["time"]]+0.001*dt)))
    }
    for(var.curr in names(ind.var)){
        ind.L <- L[,1]==var.curr
        bounds.curr <- ind.var[[var.curr]]
        lump.curr <- layout$lump[ind.L]
        if(meas.point.val){# interpolate to the measured points in time, assuming that the model output was constant during the previous time step
            res.var.ext  <- c(res.sup[["y"]][as.numeric(bounds.curr)[1]], c(rbind(res.sup[["y"]][as.numeric(bounds.curr)[1]:as.numeric(bounds.curr)[2]], c(res.sup[["y"]][(as.numeric(bounds.curr)[1]+1):as.numeric(bounds.curr)[2]], NA))))
            y.out.curr  <- as.numeric(approx(x=res.time.ext[-length(res.time.ext)], y=res.var.ext[-length(res.var.ext)], xout=L[ind.L,2])$y)
        }else{
            y.out.curr  <- as.numeric(approx(x=res.sup[["time"]], y=res.sup[["y"]][as.numeric(bounds.curr)[1]:as.numeric(bounds.curr)[2]], xout=L[ind.L,2])$y) ## ATTENTION: Superflex returns the average of the streamflow during the most recent time step (t-dt,t), not the point value at t. If point values at t are required (e.g. because measurements are point values at t), this has to be interpolated here.
        }
        if(!is.na(lump.curr[1]) & lump){
            y.out.curr <- tapply(y.out.curr, lump.curr, mean)
            ##            y.out.curr <- rep(y.out.red, times=as.numeric(tabulate(lump.curr)))
            y.out.curr <- as.numeric(y.out.curr[lump.curr])
        }
        if(any(is.na(y.out.curr))) stop(paste("result2layout produced NAs for", var.curr))
        y.out[which(ind.L)]  <- as.numeric(y.out.curr)
    }
    y.out <- list(nme = y.out)
    names(y.out) <- ifelse(lump, "incld.lmpd", "original")
    return(y.out)
}
