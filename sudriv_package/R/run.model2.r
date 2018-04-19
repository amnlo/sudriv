run.model2 <- function(sudriv, par, layout){ ## par and layout are just dummy arguments to keep the function compatible with the general structure of the calling likelihood function. They are not used. Instead, the parameters and layout are taken from the sudriv object.
    sudriv$layout <- layout ## ATTENTION: replace sudriv's layout with the one supplied as an argument. As long as this function doesn't return a sudriv object, this should be fine...
    res.sup <- run.engine(sudriv)
    ind.var <- result_index_var(res.sup=res.sup, file.o="outnames.txt", variables=unique(sudriv$layout$layout[,1]))
    y.mod   <- result2layout(res.sup=res.sup, ind.var=ind.var, layout=sudriv$layout)
    return(y.mod)
}
