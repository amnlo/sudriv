regularize.layout <- function(sudriv, threshold=50000, max.dt=0.5, min.dt=0.2){
    vars <- as.character(unique(sudriv$layout$layout$var))
    layout.new <- data.frame(var=character(), time=numeric())
    obs.new <- numeric()
    for(var.curr in vars){
        print(var.curr)
        l.curr <- subset(sudriv$layout$layout, var==var.curr)
        if(nrow(l.curr)>threshold){
            dts <- table(diff(l.curr$time))
            dts <- dts[as.numeric(names(dts))<=max.dt & as.numeric(names(dts))>=min.dt]
            print(dts)
            if(length(dts)<1) stop(paste0("no time step size matches specification in variable ", var.curr))
            cover <- as.numeric(names(dts))*as.numeric(dts)
            best.dt <- as.numeric(names(dts)[which.max(cover)])
            print(best.dt)
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
            print(head(l.curr))
            l.curr <- data.frame(var=var.curr, time=t.new)
            obs.curr <- approx(t.old, su$observations[su$layout$layout$var == var.curr], xout=t.new)$y
        }else{
            obs.curr <- su$observations[su$layout$layout$var==var.curr]
        }
        layout.new <- rbind(layout.new, l.curr)
        obs.new <- c(obs.new, obs.curr)
    }
    sudriv$layout$layout <- layout.new
    sudriv$observations <- obs.new
    return(sudriv)
}
