op.sys <- Sys.info()["sysname"]
if(op.sys == "Linux") .libPaths("/mnt/project/ammannlo/personalR/library")
plt <- FALSE
eval.multistart <- FALSE
wrk.dir <- getwd()
source("source.functions.r")
source.functions(wrk.dir)

library(nloptr)
library(rjson)
library(utils)
library(ggplot2)
library(plyr)
options(stringsAsFactors=FALSE)

fn.seq <- run.seq.MLE
su <- sudriv.setup()

if(op.sys == "Linux"){
    pth.base <- "\"/mnt/project/ammannlo/PhD/runfiles/"
    lib.path <- "/mnt/project/ammannlo/personalR/library"
    f.path.config <- paste(pth.base, "flexConfigEschMain_", su$settings$subcatchment, "_", su$settings$structure, "_", su$settings$tracer, ".dat", "\"", sep = "")
    f.path.hru <- paste(pth.base, su$settings$subcatchment, "_", su$settings$input.tag, ".dat", "\"", sep="")
}else{
    pth.base <- "Q:\\Abteilungsprojekte\\siam\\ammannlo\\PhD\\Model\\SuperFlex\\Driver\\runfiles\\"
    lib.path <- "C:/Users/ammannlo/Programs/R/library"
    f.path.config <- paste(pth.base, "flexConfigEschMain_", su$settings$subcatchment, "_", su$settings$structure, "_", su$settings$tracer, ".dat", sep = "")
    f.path.hru <- paste(pth.base, su$settings$subcatchment, "_", su$settings$input.tag, ".dat", sep="")
}
file.config <- file("path_config.txt")
write(f.path.config, file.config)

su <- model.setup(su, writeO=TRUE, f.path.hru=f.path.hru)
su <- parameters.setup(su)
su <- optimizer.setup(su)
su <- likelihood.setup(su)

su <- construct.prior(su, file="../../sudriv_input/priordef.txt")

#npar.fit <- su$settings$OPT.npar.fit.max

## Split the time into global and local optimizer time (I have seen that it is important to refine the global result with
## a local optimizer, even when the global optimizer converged ...)
time.global <- su$settings$OPT.time.tot * su$settings$OPT.frac.glob
time.local  <- su$settings$OPT.time.tot - time.global

savefile <- paste("../../sudriv_output/su_", su$settings$subcatchment, "_", su$settings$structure, "_", su$settings$input.tag, "_", su$settings$fit.vars, "_", su$settings$tracer, "_", su$settings$OPT.time.tot, sep = "")

if(time.global > 0){
    MLE.per.proc <- floor(time.global / su$settings$OPT.opts$maxtime)
    tabl <- run.par.MLE(nparallel=su$settings$nparallel, par.fit.psbl=su$model$par.fit.psbl, npar.fit=npar.fit, fn.seq=fn.seq, MLE.per.proc=MLE.per.proc, su=su, lib.path=lib.path)
    ## Select the best fit of the parallel optimization and update the object accordingly
    write.table(tabl, file=paste(savefile, ".txt", sep=""), row.names=FALSE)
}

if(time.local > 0){

    ## Refine the best fit of the global optimization by applying a local optimization (try to fit all parameters)
    su$settings$OPT.opts$algorithm <- "NLOPT_LN_COBYLA" ## select a local optimizer
    su$settings$OPT.opts$maxtime <- time.local ## apply two different algorithms each 1/2 of the time
    ##su$model$par.fit <- su$model$par.fit.psbl ## try to fit all parameters


    ## Perform the actual sequential refinement through local optimization
    su <- MLE.sudriv(su)
    su <- select.best.MLE(su)
    save(su, file=paste(savefile, ".RData", sep=""))

}

if(eval.multistart){

    pth.output <- "../../sudriv_output/"
    fnames <- list.files(pth.output)
    fnames <- fnames[grep("AAll.*earlredslop70wtns880.*txt", fnames)]
    fnames <- paste(pth.output, fnames, sep="")
    fitted <- as.logical(c(su$model$par.fit, su$likelihood$par.fit))
    ms <- multistart.get(fnames)
    ms <- subset(ms, Objective < 1500)
    ms$Objective <- ms$Objective/1500
    ms$Objective <- ms$Objective - min(ms$Objective)
    ms.gg <- make.gg2.df(ms, fitted)
    su <- select.pars.multistart(su, ms, index=1)
    sp <- find.special.pars(ms, index=NA)
    sp.fitted <- sp[fitted]

    ##ggplot
    gg.obj <- ggplot(ms.gg, aes(p.value, obj)) + facet_wrap(~ p.name, scales="free_x") + geom_point() + theme_bw()
    plot(gg.obj)

}

if(plt){

    ## Run the engine
    ##result <- run.model(par=NA, layout=NA, sudriv=su)
    re <- run.engine(su)

    ##variables.mod=c("C5Tc1_Qstream", "U2F5Tc1Lv1_Su1", "U2F5Tc1Lv1_Sr1", "U3F5Tc1Lv1_Su1", "U3F5Tc1Lv1_Sf1")
    ##variables.mod=c("U5F1Wv_Su1", "U2F1Wv_Sr1", "U3F1Wv_Sf1", "U5F1Wv_Ss1")
    ##variables.mod=c("C4Wv_Qstream", "U1F4Wv_Qstrm", "U2F4Wv_Qstrm","U3F4Wv_Qstrm","U5F4Wv_Qstrm")
    variables.mod=c("C1Tc1_Qstream", "C2Tc1_Qstream",  "C3Tc1_Qstream",  "C4Tc1_Qstream",  "C5Tc1_Qstream")
    ##variables.mod=c("C4Tc1_Qstream", "C4Tc2_Qstream")
    ##variables.mod=c("C1Wv_Qstream", "C2Wv_Qstream", "C3Wv_Qstream")
    ##variables.mod=c("U3F1Wv_Su1", "U3F1Tc1Lv1_Su1", "U3F1Tc1Lv2_Su1", "U3F1Wv_Sf1", "U3F1Tc1Lv1_Sf1", "U3F1Tc1Lv2_Sf1")
    ##variables.mod=c("C4Wv_Qstream", "C4Tc1_Qstream", "C4Tc2_Qstream")
    ## possible variables:
    ##"U2F1Wv_Su1", "U1F1Tc1Lv1_Sf1", "U2F1Tc1Lv1_Sr1", "U3F1Tc1Lv1_Sf1", "U4F1Tc1Lv1_Sr1", "U5F1Tc1Lv1_Sf1", "U5F1Tc1Lv1_Ss1")
    variables.inp=c()
    layout.mod <- data.frame(var=rep(variables.mod, each=length(re$time)), time=rep(re$time, length(variables.mod)))
    ## Plot some things
    ##y.mod <- result$original
    ind.var <- result_index_var(res.sup=re, file.o="outnames.txt", variables=variables.mod)
    y.mod <- numeric()
    for(var.curr in ind.var){
        y.mod <- c(y.mod, re$y[var.curr[1]:var.curr[2]])
    }
    layout.obs <- subset(su$layout$layout, var %in% variables.mod)
    layout.obs <- rbind(layout.obs, data.frame(var=rep(variables.inp, each=nrow(su$input$inputobs)), time=rep(su$input$inputobs[,"julday"], length(variables.inp))))
    layout.obs[,"var"] <- as.character(layout.obs[,"var"])
    l.obs <- subset(su$layout$layout, var %in% variables.mod)
    y.obs <- su$observations[su$layout$layout[,1] %in% variables.mod]
    y.obs <- c(y.obs, as.numeric(su$input$inputobs[,variables.inp]))

    ##plot.results(layout.mod=layout.mod, y.mod=y.mod, layout.obs=layout.obs, y.obs=y.obs, scales="free_y", xlim=as.POSIXct(c("2009-05-26 10:00","2009-05-27 10:00")), per.area=TRUE, hru.areas=su$model$hru.areas, file="../../output/dummy.png")
    plot.results(layout.mod=layout.mod, y.mod=y.mod, layout.obs=layout.obs, y.obs=y.obs, scales="free_y", xlim=as.POSIXct(c("2009-06-25 10:00","2009-06-30 10:00")), per.area=TRUE, hru.areas=su$model$hru.areas, file="../../output/dummy.png")


I <- as.data.frame(su$input$inputobs)
    I <- subset(I, julday >= 440 & julday < 460)
    ind.q <- su$layout$layout[,1] == "C1Wv_Qstream" & su$layout$layout[,2] >= 440 & su$layout$layout[,2] < 460
    L <- su$layout$layout[ind.q,]
    ybs <- y.obs[ind.q]
    df <- data.frame(time = c(rep(I[,1],2), L[,2]), y =c(I[,"P"], I[,"T"], ybs), var = as.factor(c(rep("P", nrow(I)), rep("T", nrow(I)), rep("D", length(ybs)))))
    plt <- ggplot() + geom_bar(aes(time, y), data = subset(df, var=="P"), stat="identity", size=2) + geom_line(data=subset(df, var=="D"), aes(time,y)) + facet_grid(var~., scales="free")
}
