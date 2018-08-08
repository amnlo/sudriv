regularize.layout <- function(sudriv, threshold=50000, max.dt=0.5, min.dt=0.2){
    vars <- as.character(unique(sudriv$layout$layout$var))
    layout.new <- list()
    obs.new <- numeric()
    j <- 1
    for(var.curr in vars){
        l.curr <- subset(sudriv$layout$layout, var==var.curr)
        if(nrow(l.curr)>threshold){
            dts <- table(diff(l.curr$time))
            dts <- dts[as.numeric(names(dts))<=max.dt & as.numeric(names(dts))>=min.dt]
            if(length(dts)<1) stop(paste0("no time step size matches specification in variable ", var.curr))
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
            obs.curr <- approx(t.old, su$observations[su$layout$layout$var == var.curr], xout=t.new)$y
        }else{
            t.new <- l.curr$time
            obs.curr <- su$observations[su$layout$layout$var==var.curr]
        }
        layout.new[[j]] <- t.new
        obs.new <- c(obs.new, obs.curr)
        j <- j + 1
    }
    names(layout.new) <- vars
    tmes=lapply(layout.new, length)
    layout.new <- data.frame(var=rep(names(layout.new), times=tmes), time=unlist(layout.new))
    rownames(layout.new) <- NULL
    print(head(layout.new))
    sudriv$layout$layout <- layout.new
    sudriv$observations <- obs.new
    return(sudriv)
}
