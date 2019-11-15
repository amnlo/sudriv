run.model <- function(layout, sudriv, lump=TRUE){ ## par and layout are just dummy arguments to keep the function compatible with the general structure of the calling likelihood function. They are not used. Instead, the parameters and layout are taken from the sudriv object.
    res.sup <- run.engine(sudriv)
    var <- unique(layout$layout[,1])
    outnames <- sudriv$model$outnames
    if(any(grepl("HYPERSTATE", var))){#dealing with hyperstates that are not directly calculated by the model, but they are calculated here based on the output of the model
        if(is.null(sudriv$model$hyperfun)) stop("hyperfun not defined, but found hyperstates")
        hyps <- unique(var[grepl("HYPERSTATE", var)])
        result.hyp <- unlist(lapply(X=sudriv$model$hyperfun, FUN = function(f,a,b) f(a,b), a=res.sup,b=sudriv))
        outnames <- c(outnames, hyps) # adapt the outnames accordingly
        res.sup$y <- c(res.sup$y, result.hyp) # add the calculated output to the result vector, as if it was calculated by superflex
    }
    ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=var, outnames=outnames)
    y.mod   <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=layout, meas.point.val=sudriv$settings$meas.point.val, lump=lump)
    return(y.mod)
}
