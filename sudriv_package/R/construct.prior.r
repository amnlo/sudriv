## This function defines some distributions

construct.prior <- function(sudriv, file=NA, rep.var=FALSE){

    distdef.model <- list()
    for(p in 1:length(sudriv$model$parameters)){
        distdef.model[[names(sudriv$model$parameters)[p]]] <- c("uniform", sudriv$model$args$parLo[p], sudriv$model$args$parHi[p])
    }
    distdef.likeli <- list()
    for(p in 1:length(sudriv$likelihood$parameters)){
        distdef.likeli[[names(sudriv$likelihood$parameters)[p]]] <- c("uniform", sudriv$likelihood$lb[p], sudriv$likelihood$ub[p])
    }

    if(!is.na(file)){
        distdef <- read.table(file=file, sep="", header=TRUE)
        ## colnames(distdef) <- distdef[1,]
        ## distdef <- distdef[-1,]
        ## n.var.layout <- length(unique(sudriv$layout$layout$var))
        ## ## repeat distribution definitions for different variables if necessary
        ## if(rep.var & n.var.layout > 1){
        ##     warning("layout contains more variables than likelihood parameter file. Repeating those for all varialbes ...")
        ##     ## repeat each prior distribution definition for all the variables
        ##     for(p.curr in names(sudriv$model$prior$distdef)){
        ##         mygrepl <- function(x,pat,...){grepl(pat,x,...)}
        ##         lapply(mygrepl, )
        ##         if((grepl("Glo", p.curr)))
        ##     }
        ## }
        for(p.curr in 1:nrow(distdef)){
            def.curr <- distdef[p.curr,2:ncol(distdef)]
            def.curr <- def.curr[!is.na(def.curr)]
            if(distdef[p.curr,"Parameter"] %in% names(sudriv$model$parameters)){
                distdef.model[[distdef[p.curr, "Parameter"]]] <- def.curr
            }else if(distdef[p.curr,"Parameter"] %in% names(sudriv$likelihood$parameters)){
                distdef.likeli[[distdef[p.curr, "Parameter"]]] <- def.curr
            }else{
                stop(paste("parameter", distdef[p.curr,"Parameter"], "not found"))
            }
        }
    }


    sudriv <- prior.model.setup(sudriv, dist="indep", distdef=distdef.model)
    sudriv <- prior.likeli.setup(sudriv, dist="indep", distdef=distdef.likeli)

    return(sudriv)
}
