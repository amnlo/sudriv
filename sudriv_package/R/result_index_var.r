#' Get the location of the output variables of a Superflex result corresponding to the specified layout.
#'
#' @param res.sup Output from Superflex run (numeric vector)
#' @param file.o Character name of file where output names of Superflex are stored
#' @param layout The layout for which the output should be extracted.
#' @return A list of n vectors, where n is equal to the number of different varialbes in 'layout'. The first number of the vector is the first index of that variable in \code{res.sup} and the second is the last index in \code{res.sup}
result_index_var<- function(res.sup, file.o, variables, outnames=NULL){
    if(is.null(outnames)){
        outnames <- as.character(read.table(file.o)[,1])
        outnames <- gsub("%", "_", outnames)
        outnames <- gsub("\\[", "", outnames)
        outnames <- gsub("\\]", "", outnames)
    }
    n <- length(res.sup$time)
    target <- rep(list(nme=c(0,0)), length(variables))
    names(target) <- variables
    for(n.curr in variables){
        ind.outn <- which(outnames == n.curr)
        if(length(ind.outn)>1) stop(paste(n.curr, " occurs multiple times in result", sep = ""))
        if(length(ind.outn)==0){
            stop(paste(n.curr, " not found in model output", sep=""))
        }else{
            target[[n.curr]] <- c((ind.outn-1)*n+1, ind.outn*n)
        }
    }
    return(target)
}
