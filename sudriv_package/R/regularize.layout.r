regularize.layout <- function(sudriv, threshold=50000, max.dt=0.5, min.dt=0.2, remove=NA){
    ## ATTENTION, this function cannot properly deal with su$layout$calib. It just assigns calibration to all the regularized layout sections and assumes that there is no prediction.
    vars <- as.character(unique(sudriv$layout$layout$var))
    layout.new <- list()
    obs.new <- numeric()
    lump.new <- numeric()
    lump.old <- as.character(sudriv$layout$lump)
    calib.new <- numeric()
    j <- 1
    i <- 1
    for(var.curr in vars){
        ind.var <- sudriv$layout$layout$var==var.curr
        l.curr <- sudriv$layout$layout[ind.var,]
        if(nrow(l.curr)>threshold){
            dts <- table(diff(l.curr$time))
            dts <- dts[as.numeric(names(dts))<=max.dt & as.numeric(names(dts))>=min.dt]
            if(length(dts)<1) stop(paste0("no time step size matches specification in variable ", var.curr))
            if(any(!is.na(lump.old[ind.var]))) stop(paste0("regularize.layout cannot deal with lump in variable ", var.curr))
            cover <- as.numeric(names(dts))*as.numeric(dts)
            best.dt <- as.numeric(names(dts)[which.max(cover)])
            t.old <- l.curr$time
            t.new <- seq(from=t.old[1], to=t.old[length(t.old)], by=best.dt)
            disfun <- function(x,t.new,t.old){
                sum(unlist(lapply(t.new, function(y) min(abs(x+y  - t.old)))))
            }
            ##shift <- optim(par=0, fn=disfun, t.new=t.new, t.old=t.old, method="L-BFGS-B", lower=-0.5*best.dt, upper=0.5*best.dt)$par
            shift <- 0
            t.new <- t.new + shift
            t.new <- t.new[t.new >= t.old[1]]
            t.new <- t.new[t.new <= t.old[length(t.old)]]
            obs.curr <- approx(t.old, sudriv$observations[ind.var], xout=t.new)$y #ATTENTION: this is only correct if the observations are instantaneous measurements and not averaged over timestep.
            lump.curr <- rep(NA, length(t.new))
            calib.curr <- i:(i+length(t.new)-1)
        }else{
            t.new <- l.curr$time
            obs.curr <- sudriv$observations[ind.var]
            lump.curr <- lump.old[ind.var]
            calib.curr <- (i:(i+length(t.new)-1))[(1:nrow(sudriv$layout$layout) %in% sudriv$layout$calib)[ind.var]]
        }
        layout.new[[j]] <- t.new
        obs.new <- c(obs.new, obs.curr)
        lump.new <- c(lump.new, lump.curr)
        calib.new <- c(calib.new, calib.curr)
        j <- j + 1
        i <- i+length(t.new)
    }
    names(layout.new) <- vars
    tmes=lapply(layout.new, length)
    layout.new <- data.frame(var=rep(names(layout.new), times=tmes), time=unlist(layout.new))
    rownames(layout.new) <- NULL
    sudriv$layout$layout <- layout.new
    sudriv$observations <- obs.new
    sudriv$layout$lump <- as.factor(lump.new)
    sudriv$layout$calib <- calib.new[!(calib.new %in% remove)]## ATTENTION this only works if calib does not contain NAs
    return(sudriv)
}
