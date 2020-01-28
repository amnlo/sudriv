set.parameters <- function(sudriv, partable){
		x0 <- c(sudriv$model$parameters[as.logical(sudriv$model$par.fit)], sudriv$likelihood$parameters[as.logical(sudriv$likelihood$par.fit)])
		for(i in 1:sum(sudriv$model$par.fit)){
			x0[i] <- partable[partable[,1]==names(x0[i]),2]
		}
		for(j in 1:sum(sudriv$likelihood$par.fit)){
			x0[i+j] <- partable[partable[,1]==names(x0[i+j]),2]
		}
		np <- sum(sudriv$model$par.fit)
		x0[1:np] <- ifelse(as.logical(sudriv$model$args$parTran[as.logical(sudriv$model$par.fit)]), log(x0[1:np]), x0[1:np])
		x0[(np+1):length(x0)] <- ifelse(as.logical(sudriv$likelihood$tran[as.logical(sudriv$likelihood$par.fit)]), log(x0[(np+1):length(x0)]), x0[(np+1):length(x0)])
		sudriv$model$parameters[as.logical(sudriv$model$par.fit)] <- x0[1:np]
		sudriv$likelihood$parameters[as.logical(sudriv$likelihood$par.fit)] <- x0[(np+1):length(x0)]
                return(sudriv)
}
