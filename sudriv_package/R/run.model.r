run.model <- function(layout, sudriv){ ## par and layout are just dummy arguments to keep the function compatible with the general structure of the calling likelihood function. They are not used. Instead, the parameters and layout are taken from the sudriv object.
    res.sup <- run.engine(sudriv)
    ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=unique(layout$layout[,1]))
    y.mod   <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=layout, meas.point.val=sudriv$settings$meas.point.val)
    return(y.mod)
}
