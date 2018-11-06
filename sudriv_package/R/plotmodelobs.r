## This script contains functions to plot the model results and compare them to the observations

plot.results <- function(layout.mod, y.mod, layout.obs=NULL, y.obs=NA, variables=NA, extend.to=NA, plot=TRUE, file=NA, scales=c("free","fixed","free_x","free_y"), xlim=NULL, ylim=NULL, per.area=TRUE, hru.areas=NA){
    library(scales)
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    scales <- scales[1]
    ##translate.var <- c("C1Tc1_Qstream", "C1Tc2_Qstream", "C2Tc1_Qstream", "C2Tc2_Qstream", "C3Tc1_Qstream", "C3Tc2_Qstream", "C4Tc1_Qstream", "C4Tc2_Qstream", "C5Tc1_Qstream", "C5Tc2_Qstream")
    translate.var <- c("C1Wv_Qstream", "C2Wv_Qstream", "C3Wv_Qstream", "C4Wv_Qstream", "C5Wv_Qstream", "C1Tc1_Qstream", "C1Tc2_Qstream", "C2Tc1_Qstream", "C2Tc2_Qstream", "C3Tc1_Qstream", "C3Tc2_Qstream", "C4Tc1_Qstream", "C4Tc2_Qstream", "C5Tc1_Qstream", "C5Tc2_Qstream")
    translate.to <- c("C1Wv", "C2Wv", "C3Wv", "C4Wv", "C5Wv", "C1Tc1", "C1Tc2", "C2Tc1", "C2Tc2", "C3Tc1", "C3Tc2", "C4Tc1", "C4Tc2", "C5Tc1", "C5Tc2")
    ##translate.to <- c(expression("St. 2, Atraz."~(mu*g/l)), expression("St. 2, Terb."~(mu*g/l)), expression("St. 4, Atraz."~(mu*g/l)), expression("St. 4, Terb."~(mu*g/l)))
    ##translate.to  <- c(expression(Atraz.~(mu*g/l)), expression(Terb.~(mu*g/l)), expression(Discharge~(l/s)), expression("P"~"(mm/15min)"))
    if(is.na(c(hru.areas)[1]) & !per.area) stop("hru.areas required if per.area is FALSE")
    if(all(is.na(variables))) variables <- unique(c(as.character(layout.mod[,1]), as.character(layout.obs[,1])))
    if(all(is.na(y.obs))){
        y.dat <- cbind(layout.mod, data.frame(y = as.numeric(y.mod), modobs = rep("mod", nrow(layout.mod))))
    }else{
        if(is.null(layout.obs)){
            layout.obs <- layout.mod
            if(length(y.obs) != length(y.mod)) stop("Model output and observations must be of same length if 'layout.obs' is not supplied")
        }else{
            if(length(y.obs) != nrow(layout.obs)) stop("'layout.obs' and 'y.obs' have different length")
        }
        y.dat <- cbind(rbind(layout.mod,layout.obs), data.frame(y = c(as.numeric(y.mod),as.numeric(y.obs)), modobs = c(rep("mod",nrow(layout.mod)), rep("obs", nrow(layout.obs)))))
    }
    catch.hru <- rep("Contrib. of HRUs (l/s)", nrow(y.dat))
    catch.hru[grep("C[0-9]+", y.dat$var)] <- "St. 4, Streamflow (l/s)"
    catch.hru <- factor(catch.hru, levels=c("St. 4, Streamflow (l/s)", "Contrib. of HRUs (l/s)"))
    var.ext <- as.character(y.dat$var)
    var.ext[y.dat$modobs == "obs"] <- paste(var.ext[y.dat$modobs=="obs"], ".obs", sep="")
    y.dat <- cbind(y.dat, catch.hru, var.ext)
    y.dat     <- y.dat[y.dat$var %in% variables,]
    ## Translate variable names for plotting
    y.dat$var <- as.factor(y.dat$var)
    y.dat$vartrans <- y.dat$var
    lv <- levels(y.dat$vartrans)
    if(length(translate.to)>0){
        lv[lv %in% translate.var] <- translate.to[match(lv[lv %in% translate.var], translate.var)]
    }
    levels(y.dat$vartrans) <- lv
    ## Convert streamflow from intesive to extensive quantity
    if(!per.area){
        cumulation <- list(c(1,2,3,4,5),
                           c(2,3),
                           c(3),
                           c(4,5),
                           c(5))
        strmflws <- variables[grepl("Wv", variables)]
        for(var.strm in strmflws){
            if(grepl("C[0-9]+", var.strm) | grepl("R[0-9]+", var.strm)){
                i <- as.numeric(substr(var.strm, start=2, stop=gregexpr("Wv", var.strm)[[1]]-1))
                i <- cumulation[[i]]
                j <- 1:ncol(hru.areas)
            }else{
                stp=gregexpr("F[0-9]+", var.strm)[[1]]-1
                j <- as.numeric(substr(var.strm, start=2, stop=stp))
                stp2 <- gregexpr("F[0-9]+", var.strm)
                i <- as.numeric(substr(var.strm, start=3+nchar(as.character(j)), stop=stp2[[1]]+attr(stp2[[1]],"match.length")-1))
            }
            y.dat[y.dat$var == var.strm,"y"] <- y.dat[y.dat$var == var.strm,"y"] * sum(hru.areas[i,j])/15/60
        }
    }
    ## Convert days in y.dat into time for plotting purposes
    strt <- as.POSIXct("2008-01-01")
    y.dat$time <- strt + as.numeric(y.dat$time * 60*60*24)
    print(head(y.dat$time))
    print(tail(y.dat$time))
    ## limit the data plotted to the specified region
    if(!is.null(xlim)) y.dat <- subset(y.dat, time >= xlim[1] & time <= xlim[2])
    ggplot.obj <- ggplot(y.dat, aes(x=time, y=y, group=modobs, shape=modobs, col=modobs)) + scale_x_datetime(limits=xlim) + scale_y_continuous(limits=ylim)
    ggplot.obj <- ggplot.obj + geom_point(data=subset(y.dat, modobs=="obs" & var!="P"), size=1.8) + geom_line(data=subset(y.dat, modobs=="mod" & var!="P"), size=1.2)+ theme_bw(base_size=20) + theme(legend.position="none", axis.text.x=element_text(size=20)) + geom_col(data=subset(y.dat, var=="P"))
    ggplot.obj <- ggplot.obj + labs(x="", y="") + facet_wrap(~vartrans, nrow=length(variables), scales=scales, labeller=label_parsed, strip.position="left")
    ## ggplot.obj <- ggplot() + scale_x_datetime(limits=xlim, labels=date_format("%d.%m %H:%M")) + scale_y_continuous(limits=ylim)
    ## ggplot.obj <- ggplot.obj + geom_line(aes(x=time, y=y, group=var.ext, col=var.ext, linetype=var.ext), data=y.dat, size=1.8)+ theme_bw(base_size=26) + theme(axis.text.x=element_text(size=16))## + theme(legend.position="none")
    ## cols2 <- gg_color_hue(2)
    ## cols6 <- gg_color_hue(6)
    ## ggplot.obj <- ggplot.obj +
    ##     scale_colour_manual(name="",
    ##                         labels=c(C4Wv_Qstream.obs="Outlet, obs.", C4Wv_Qstream="Outlet, sim.", U1F4Wv_Qstrm="Impervious", U2F4Wv_Qstrm="Shortcut", U3F4Wv_Qstrm="Drained", U5F4Wv_Qstrm="Remaining"),
    ##                         values=c(C4Wv_Qstream.obs=cols2[2], C4Wv_Qstream=cols2[1], U1F4Wv_Qstrm=cols6[2], U2F4Wv_Qstrm=cols6[3],U3F4Wv_Qstrm=cols6[4],U5F4Wv_Qstrm=cols6[5]))+
    ##      scale_linetype_manual(name="",
    ##                         labels=c(C4Wv_Qstream.obs="Outlet, obs.", C4Wv_Qstream="Outlet, sim.", U1F4Wv_Qstrm="Impervious", U2F4Wv_Qstrm="Shortcut", U3F4Wv_Qstrm="Drained", U5F4Wv_Qstrm="Remaining"),
    ##                          values=c(C4Wv_Qstream.obs="dotted", C4Wv_Qstream="solid", U1F4Wv_Qstrm="solid", U2F4Wv_Qstrm="solid",U3F4Wv_Qstrm="solid",U5F4Wv_Qstrm="solid"))
    ## ggplot.obj <- ggplot.obj + labs(x="", y="") + facet_wrap(~catch.hru, nrow=length(unique(catch.hru)), scales=scales, strip.position="left")
    if(plot){
        Sys.setlocale("LC_TIME", "English")
        if(is.na(file)){
            dev.new()
            plot(ggplot.obj)
        }else{
            width = 10
            height = 6
            nchar = length(unlist(strsplit(file, split = NULL)))
            pat = substr(file, nchar-2, nchar)
            if(pat == "pdf"){
                pdf(file, width = width, height = height)
            }else if(pat == "peg" | pat == "jpg"){
                jpeg(file, res = 400, units = "in", width = width, height = height)
            }else if(pat == "png"){
                png(file, res = 400, units = "in", width = width, height = height)
            }else stop("file type not recognized")
            plot(ggplot.obj)
            dev.off()
        }
        Sys.setlocale("LC_TIME", "")
    }else{
        return(ggplot.obj)
    }
}

plot.sd <- function(sudriv, variables=NA){
    if(is.na(variables[1])) variables <- unique(sudriv$layout$layout[,1])
    for(var.curr in variables){
        ind.par <- grep(var.curr, names(sudriv$likelihood$parameters))
        tran <- sudriv$likelihood$tran
        d <- ifelse(tran[ind.par[1]], exp(sudriv$likelihoo$parameters[ind.par[1]]), sudriv$likelihoo$parameters[ind.par[1]])
        e <- ifelse(tran[ind.par[2]], exp(sudriv$likelihoo$parameters[ind.par[2]]), sudriv$likelihoo$parameters[ind.par[2]])
        c <- ifelse(tran[ind.par[3]], exp(sudriv$likelihoo$parameters[ind.par[3]]), sudriv$likelihoo$parameters[ind.par[3]])
        Q <- seq(0, max(su$observations[sudriv$layout$layout[,1]==var.curr], na.rm=TRUE), length.out = 1000)
        sd <- d*(e+Q)^c
        dev.new()
        plot(Q,sd, type="l", xaxs="i", yaxs="i", ylim=c(0,max(sd)), main=var.curr, xlab="Variable")
    }

}
