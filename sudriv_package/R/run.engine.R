run.engine <-
function(sudriv){ ## runs the superflex model for the given parameters, dt, ntime and input
    args.model   = sudriv$model$args
    inputobs     = sudriv$input$inputobs
    stateDef     = args.model$stateDef
    parTran      = args.model$parTran
    parLo        = args.model$parLo
    parHi        = args.model$parHi
    npar.det     = args.model$npar.det
    nout         = args.model$nout
    modelID      = args.model$modelID
    outnames     = args.model$outnames
    par <- sudriv$model$parameters
    lded <- is.loaded("runmodel")
    if(!lded) cat("function name not loaded\n")
    tol <- 1e-10
    if(any(is.na(par))){
        write(par, file="debug.txt")
        stop("par contains NAs")
    }
    dt <- as.numeric(1.0)
    ntime <- as.integer(nrow(inputobs))
    time.in <- as.numeric(inputobs[,1])
    inputobs <- inputobs[,-1]
    ninp <- as.integer(ncol(inputobs))
    inputobs <- as.double(inputobs)
    ntotin <- length(inputobs)
    output <- vector("numeric", length=ntime*nout)
    ntotout <- length(output)
    stateDef <- as.numeric(stateDef)
    npar.det <- as.integer(npar.det)
    out.bounds <- par<parLo | par>parHi
    if(sum(out.bounds) > 0){
        if(any(par[out.bounds]<parLo[out.bounds]-tol | par[out.bounds]>parHi[out.bounds]+tol)){
            cat("par value:\n")
            print(par[which(out.bounds)])
            cat("lower bound:\n")
            print(parLo[which(out.bounds)])
            cat("upper bound:\n")
            print(parHi[which(out.bounds)])
            stop(paste("parameter(s) ", paste(which(out.bounds), collapse=" "), " is (are) out of bounds", sep = " "))
        }else{
            par[out.bounds] <- pmin(pmax(parLo[out.bounds]), parHi[out.bounds])
        }
    }
    par[which(parTran==1)] <- exp(par[which(parTran==1)])
    if(!is.null(sudriv$model$transK)){## transform Ks for better identifiability
        Q0s      <- sudriv$model$transK[["Q0s"]]
        ind.ks   <- sudriv$model$transK[["ind.ks"]]
        S0s      <- sudriv$model$transK[["S0s"]]
        ind.alph <- sudriv$model$transK[["ind.alph"]]
        cat("ks pre: ", par[ind.ks], "\n")
        par[ind.ks] <- Q0s * par[ind.ks] * S0s^(-1*par[ind.alph])
        cat("ks post: ", par[ind.ks], "\n")
        cat("alph: ", par[ind.alph], "\n")
    }
    result <- .Fortran("runmodel", par=as.double(par), stateDef=as.double(stateDef), npar=as.integer(npar.det), ntime=as.integer(ntime), ninp=as.integer(ninp), nout=as.integer(nout), dt=as.double(dt), inputobs=as.double(inputobs), modelID=as.integer(modelID), output=as.double(output))
    result <- list(y=result$output, time=time.in, nout=nout)
    return(result)
}
