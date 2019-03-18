prepare.plot.layout <- function(sudriv, var.obs, var.mod=c(), vary=list()){
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
    ## include the hyperstates (similar to run.model...)
    var <- c(var.obs, var.mod)
    outnames <- su.plot$model$outnames
    if(any(grepl("HYPERSTATE", var))){#dealing with hyperstates that are not directly calculated by the model, but they are calculated here based on the output of the model
        if(is.null(sudriv$model$hyperfun)) stop("hyperfun not defined, but found hyperstates")
        hyps <- unique(var[grepl("HYPERSTATE", var)])
        j <- 1
        for(hyp.curr in hyps){
            result.hyp <- sudriv$model$hyperfun[[j]](res.sup, su.plot)
            res.sup$y <- c(res.sup$y, result.hyp) # add the calculated output to the result vector, as if it was calculated by superflex
            outnames <- c(outnames, hyp.curr) # adapt the outnames accordingly
            j <- j + 1
        }
    }
    ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=var, outnames=outnames)
    ## prepare layout for output times of model
    l.out <- data.frame(var=rep(c(var.obs, var.mod), each=nrow(sudriv$input$inputobs)), time=rep(sudriv$input$inputobs[,1], length(c(var.obs, var.mod))))
    l.out.multi <- data.frame(var=rep(var, each=nrow(sudriv$input$inputobs), times=1+length(unlist(vary))), time=rep(sudriv$input$inputobs[,1], times=length(var)*(1+length(unlist(vary)))))
    l.out.multi$vary <- rep(0:length(unlist(vary)), each=nrow(l.out))
    y.mod.multi <- rep(NA, nrow(l.out.multi))
    y.mod.multi[1:nrow(l.out)]   <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=list(layout=l.out, lump=NA), lump=FALSE)$original
    i <- 1
    for(vary.var in names(vary)){
        for(valu in vary[[vary.var]]){
            pr.tmp <- which(grepl(vary.var, names(su.plot$model$parameters)))
            if(length(pr.tmp)>1) warning(paste0("ambiguous parameter name ", vary.var, " supplied..."))
            if(length(pr.tmp)<1) warning(paste0("could not find any parameter for ", vary.var))
            su.plot$model$parameters[pr.tmp] <- valu
            res.sup <- run.engine(su.plot)
            ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=var, outnames=sudriv$model$outnames)
            y.mod.multi[(i*nrow(l.out)+1):((i+1)*nrow(l.out))] <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=list(layout=l.out, lump=NA), lump=FALSE)$original
            i <- i + 1
        }

    }
    y.obs   <- rep(NA, nrow(su.plot$layout$layout))
    y.obs[1:length(sudriv$observations)] <- sudriv$observations
    su.plot$layout$layout$time <- su.plot$layout$layout$time/24
    l.out.multi$time <- l.out.multi$time/24
    if(precip){
        layout.obs <- data.frame(var="P", time=sudriv$input$inputobs[,1]/24)
        layout.obs <- rbind(su.plot$layout$layout,layout.obs)
	y.obs <- c(y.obs, su$input$inputobs[,2])
    }else{
        layout.obs <- su.plot$layout$layout
    }
    vary2 <- vary
    if(length(vary)>0){
        vary2[[1]] <- c(sudriv$model$parameters[pr.tmp[1]], vary2[[1]]) ##ATTENTION: using pr.tmp here means that this function can only be applied if length(vary)<=1
        if(as.logical(sudriv$model$args$parTran)[pr.tmp[1]]) vary2[[1]] <- exp(vary2[[1]])
    }
    return(list(layout.mod = list(layout=l.out.multi),
                layout.obs = layout.obs,
                y.mod = y.mod.multi,
                y.obs = y.obs,
                vary = vary2))
}
