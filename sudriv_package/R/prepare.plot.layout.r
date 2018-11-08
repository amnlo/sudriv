prepare.plot.layout <- function(sudriv, var.obs, var.mod=c()){
    ## This function prepares the layout of the observed and modelled variables (var.obs) and the modelled variables (var.mod)
    precip=FALSE
    if("P" %in% var.obs){var.obs <- var.obs[var.obs != "P"]; precip=TRUE}
    if(!all(var.obs %in% unique(sudriv$layout$layout$var))) stop("strange var.obs supplied")
    su.plot <- sudriv
    nq <- nrow(su.plot$layout$layout)
    l.mod <- do.call(rbind, rep(list(su.plot$layout$layout), length(var.mod)))
    l.mod$var <- rep(var.mod, each = nq)
    su.plot$layout$layout <- rbind(su.plot$layout$layout, l.mod)
    res.sup <- run.engine(su.plot)
    ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=c(var.obs,var.mod), outnames=sudriv$model$outnames)
    ## prepare layout for output times of model
    l.out <- data.frame(var=rep(c(var.obs, var.mod), each=nrow(sudriv$input$inputobs)), time=rep(sudriv$input$inputobs[,1], length(c(var.obs, var.mod))))
    y.mod   <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=list(layout=l.out, lump=NA), lump=FALSE)$original
    y.obs   <- rep(NA, nrow(su.plot$layout$layout))
    y.obs[1:length(sudriv$observations)] <- sudriv$observations
    su.plot$layout$layout$time <- su.plot$layout$layout$time/24
    l.out$time <- l.out$time/24
    if(precip){
        layout.obs <- data.frame(var="P", time=sudriv$input$inputobs[,1]/24)
        layout.obs <- rbind(su.plot$layout$layout,layout.obs)
	y.obs <- c(y.obs, su$input$inputobs[,2])
    }else{
        layout.obs <- su.plot$layout$layout
    }
    return(list(layout.mod = list(layout=l.out),
                layout.obs = layout.obs,
                y.mod = y.mod,
                y.obs = y.obs))
}
