check.layout <- function(layout,rel.tol=1e-3){
    ##remove time points from calibration with non-positive timestep
    dt <- c(diff(layout$layout$time[layout$calib]),Inf)
    for(var.curr in unique(layout$layout$var)){
        ind.curr <- which(layout$layout$var[layout$calib]==var.curr)
        tol <- rel.tol * quantile(abs(dt[ind.curr]), 0.1, na.rm=TRUE)
        rmve <- ind.curr[which(dt[ind.curr[-length(ind.curr)]]<=tol)]
        while(length(rmve)>0){
            print(paste("Removing",length(rmve),"entries from calib and valid for variable",var.curr))
            print(rmve)
            print(dt[rmve])
            layout$calib <- layout$calib[-rmve]
            dt <- c(diff(layout$layout$time[layout$calib]),Inf)
            ind.curr <- which(layout$layout$var[layout$calib]==var.curr)
            rmve <- ind.curr[which(dt[ind.curr[-length(ind.curr)]]<=tol)]
        }
    }
    return(layout)
}
