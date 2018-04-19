## This function defines some distributions

construct.prior <- function(su, file=NA){

    distdef.model <- list()
    for(p in 1:length(su$model$parameters)){
        distdef.model[[names(su$model$parameters)[p]]] <- c("uniform", su$model$args$parLo[p], su$model$args$parHi[p])
    }
    distdef.likeli <- list()
    for(p in 1:length(su$likelihood$parameters)){
        distdef.likeli[[names(su$likelihood$parameters)[p]]] <- c("uniform", su$likelihood$lb[p], su$likelihood$ub[p])
    }

    if(!is.na(file)){
        distdef <- read.table(file=file, sep="")
        colnames(distdef) <- distdef[1,]
        distdef <- distdef[-1,]
        for(p.curr in 1:nrow(distdef)){
            def.curr <- distdef[p.curr,2:ncol(distdef)]
            def.curr <- def.curr[!is.na(def.curr)]
            if(distdef[p.curr,"Parameter"] %in% names(su$model$parameters)){
                distdef.model[[distdef[p.curr, "Parameter"]]] <- def.curr
            }else if(distdef[p.curr,"Parameter"] %in% names(su$likelihood$parameters)){
                distdef.likeli[[distdef[p.curr, "Parameter"]]] <- def.curr
            }else{
                stop(paste("parameter", distdef[p.curr,"Parameter"], "not found"))
            }
        }
    }


    su <- prior.model.setup(su, dist="indep", distdef=distdef.model)
    su <- prior.likeli.setup(su, dist="indep", distdef=distdef.likeli)

    return(su)
}
