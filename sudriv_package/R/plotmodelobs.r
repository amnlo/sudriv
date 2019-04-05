## This script contains functions to plot the model results and compare them to the observations

plot.results <- function(layout.mod, y.mod, layout.obs=NULL, y.obs=NA, vary=list(), variables=NA, extend.to=NA, plot=TRUE, file=NA, scales=c("free","fixed","free_x","free_y"), xlim=NULL, ylim=NULL, per.area=TRUE, hru.areas=NA, distributed=FALSE, parameters=NA){
    library(scales)
    ## gg_color_hue <- function(n) {
    ##     hues = seq(15, 375, length = n + 1)
    ##     hcl(h = hues, l = 65, c = 100)[1:n]
    ## }
    scales <- scales[1]
    strmflw.units <- ifelse(per.area,"(mm/15min)","l/s")
    ##translate.var <- c("C1Tc1_Qstream", "C1Tc2_Qstream", "C2Tc1_Qstream", "C2Tc2_Qstream", "C3Tc1_Qstream", "C3Tc2_Qstream", "C4Tc1_Qstream", "C4Tc2_Qstream", "C5Tc1_Qstream", "C5Tc2_Qstream")
    translate.var <- c("C1Wv_Qstream", "C2Wv_Qstream", "C3Wv_Qstream", "C4Wv_Qstream", "C5Wv_Qstream", "C1Tc1_Qstream", "C1Tc2_Qstream", "C2Tc1_Qstream", "C2Tc2_Qstream", "C3Tc1_Qstream", "C3Tc2_Qstream", "C4Tc1_Qstream", "C4Tc2_Qstream", "C5Tc1_Qstream", "C5Tc2_Qstream", "U1F1Wv_Qstrm", "U2F1Wv_Qstrm", "U3F1Wv_Qstrm", "U4F1Wv_Qstrm", "U5F1Wv_Qstrm", "U1F1Tm1_Qstrm", "U2F1Tm1_Qstrm", "U3F1Tm1_Qstrm", "U4F1Tm1_Qstrm", "U5F1Tm1_Qstrm", "U1F1Tm2_Qstrm", "U2F1Tm2_Qstrm", "U3F1Tm2_Qstrm", "U4F1Tm2_Qstrm", "U5F1Tm2_Qstrm", "U1F1Wv_Qq_FR", "U2F1Wv_Q_RR", "U3F1Wv_Qq_FR", "U1F1Tm1_Qq_FR", "U2F1Tm1_Q_RR", "U3F1Tm1_Qq_FR", "U1F1Tm2_Qq_FR", "U2F1Tm2_Q_RR", "U3F1Tm2_Qq_FR", "U3F1MaT1_Si1Lv0", "U3F1MaT1_Si1Lv1", "U3F1MaT1_Si1Lv2", "U3F1MaT2_Si1Lv0", "U3F1MaT2_Si1Lv1", "U3F1MaT2_Si1Lv2", "HYPERSTATE_sorption")
    translate.to <- c(bquote("Streamflow"~(.(strmflw.units))), "C2Wv", "C3Wv", "C4Wv", "C5Wv", expression("Atrazine"~(mu*g/l)), expression("Terbuthylazine"~(mu*g/l)), "C2Tc1", "C2Tc2", "C3Tc1", "C3Tc2", "C4Tc1", "C4Tc2", "C5Tc1", "C5Tc2", "Impervious", "Shortcut", "Drained", "SC and Drained", "Rest", "Impervious", "Shortcut", "Drained", "SC and Drained", "Rest", "Impervious", "Shortcut", "Drained", "SC and Drained", "Rest", "ImperviousFR", "ShortcutRR", "DrainedFR", "Impervious", "Shortcut", "Drainage",  "Impervious", "Shortcut", "Drainage", "Dissolved", "Fast sorbed", "Slow sorbed", "Dissolved", "Fast sorbed", "Slow sorbed", "Apparent~K[d]~(l/kg)")
    ##translate.to <- c(expression("St. 2, Atraz."~(mu*g/l)), expression("St. 2, Terb."~(mu*g/l)), expression("St. 4, Atraz."~(mu*g/l)), expression("St. 4, Terb."~(mu*g/l)))
    ##translate.to  <- c(expression(Atraz.~(mu*g/l)), expression(Terb.~(mu*g/l)), expression(Discharge~(l/s)), expression("P"~"(mm/15min)"))
    if(is.na(c(hru.areas)[1]) & !per.area) stop("hru.areas required if per.area is FALSE")
    if(all(is.na(variables))) variables <- unique(c(as.character(layout.mod[,1]), as.character(layout.obs[,1])))
    if(all(is.na(y.obs))){
        y.dat <- cbind(layout.mod, data.frame(y = as.numeric(y.mod), modobs = paste0("mod", layout.mod$vary)))
    }else{
        if(is.null(layout.obs)){
            layout.obs <- layout.mod
            if(length(y.obs) != length(y.mod)) stop("Model output and observations must be of same length if 'layout.obs' is not supplied")
        }else{
            if(length(y.obs) != nrow(layout.obs)) stop("'layout.obs' and 'y.obs' have different length")
        }
        y.dat <- cbind(rbind(layout.mod[,c("var","time")],layout.obs), data.frame(y = c(as.numeric(y.mod),as.numeric(y.obs)), modobs = c(paste0("mod",layout.mod$vary), rep("obs", nrow(layout.obs)))))
    }
    catch.hru <- rep("Contrib. of HRUs (l/s)", nrow(y.dat))
    catch.hru[grep("C[0-9]+", y.dat$var)] <- "St. 4, Streamflow (l/s)"
    catch.hru <- factor(catch.hru, levels=c("St. 4, Streamflow (l/s)", "Contrib. of HRUs (l/s)"))
    var.ext <- as.character(y.dat$var)
    var.ext[y.dat$modobs == "obs"] <- paste(var.ext[y.dat$modobs=="obs"], ".obs", sep="")
    y.dat <- cbind(y.dat, catch.hru, var.ext)
    y.dat     <- y.dat[y.dat$var %in% variables,]
    if(length(vary)>0){## ATTENTION: this function can only deal with vary of length zero or one (i.e. when one parameter is changed at a time).
        options("scipen"=-100, "digits"=5)
        for(i in 0:length(unique(y.dat$modobs))){
            y.dat$modobs <- gsub(paste0("mod",i), paste(letters[i+1],signif(vary[[1]][i+1],3)), y.dat$modobs)
        }
        options("scipen"=0, "digits"=7)
    }
    ## Convert streamflow from intesive to extensive quantity
    if(!per.area){
        cumulation <- list(c(1,2,3,4,5),
                           c(2,3),
                           c(3),
                           c(4,5),
                           c(5))
        strmflws <- variables[grepl("Wv", variables)]
        for(var.strm in strmflws){
            print(var.strm)
            if(grepl("C[0-9]+", var.strm) | grepl("R[0-9]+", var.strm)){
                i <- as.numeric(substr(var.strm, start=2, stop=gregexpr("Wv", var.strm)[[1]]-1))
                ##i <- cumulation[[i]]
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
    masses <- FALSE
    fluxes.multhru <- FALSE
    if(sum(grepl("Tc", variables))>0 & sum(grepl("U[0-9]?F[0-9]?Wv_S", variables)) > 0){# calculate the masses in the different levels of the reservoirs
        masses <- TRUE
        ## calculate the sorption coefficient Kd to compare to field measurements
        hL1 <- exp(parameters["GloTr%CmltSlOne_IR"]) ## ATTENTION: here we assume that only global dead volumes are relevant and that they are transformed
        hL2 <- exp(parameters["GloTr%CmltSlTwo_IR"]) ## ATTENTION: here we assume that only global dead volumes are relevant and that they are transformed
        Smax <- exp(parameters["Glo%CmltSmax_IR"]) ## ATTENTION: here we assume that only global Smax_IR is relevant and that it is transformed
        rho.soil.bulk <- 1.2 ## kg/dm^3, i.e. kg/L
        ne <- 0.35
        Kd <- (hL2 + hL1 - 0.1*Smax)/(rho.soil.bulk*Smax/ne)
		half.life <- log(2)/(exp(parameters["GloTr%CmltKd_WR"])*4*24)
        y.dat$var <- as.character(y.dat$var)
        ## in this section of the code some assumptions are made to keep things simple: there are two tracers, the parameters of the dead levels are transformed, we get exactly the input columns (of y.dat) that we need, nothing more(?) and nothing less
        lvls <- grep("U[0-9]?F[0-9]?Wv_S", variables)
        cases <- lapply(variables[lvls], function(x) strsplit(x, split="Wv_")[[1]])
        print(cases)
        concs1 <- lapply(cases, function(x) paste0(x[1], c("Tc1","Tc2"), "Lv1_", x[2]))
        concs2 <- lapply(cases, function(x) paste0(x[1], c("Tc1","Tc2"), "Lv2_", x[2]))
        y.dat.conc <- subset(y.dat, var %in% unlist(c(concs1,concs2)) & grepl("mod", modobs))
        print(head(y.dat.conc))
        y.dat.conc <- spread(y.dat.conc[,c("time","var","y")], key="var", value="y")
        y.dat.conc <- y.dat.conc[,c("time",rep(unlist(concs1),2),unlist(concs2))]
        print(head(y.dat.conc))
        y.dat.lvl <- subset(y.dat, var %in% variables[lvls] & grepl("mod", modobs))
        print(head(y.dat.lvl))
        y.dat.lvl <- spread(y.dat.lvl[,c("time","var","y")], key="var", value="y")
        y.dat.lvl <- y.dat.lvl[,c(1,rep(2:ncol(y.dat.lvl),each=2))]
        print(head(y.dat.lvl))
        reservoirs <- toupper(substr(unlist(cases)[grep("S.1",unlist(cases))], 2, 2))
        par.names.global <- rep(paste0("GloTr%CmltSl", rep(c("One_", "Two_"),each=length(cases)), reservoirs, "R"), each=2)
        par.names.local <- paste0(substr(unlist(cases)[grep("U[0-9]?",unlist(cases))], 1, 2), rep(c("T1","T2"),length(cases)), "%Sl", rep(c("One_", "Two_"),each=2*length(cases)), rep(reservoirs,each=2), "R")
        const.lvls <- matrix(exp(parameters[par.names.global])*exp(parameters[par.names.local]), nrow=nrow(y.dat.lvl), ncol=4*length(lvls), byrow=TRUE) # ATTENTION: here we assume that the parameters that describe the dead volumes of the reservoirs are transformed!
        colnames(const.lvls) <- paste0(rep(unlist(lapply(cases, paste0, collapse="Wv_")),times=2), rep(c("Lv1", "Lv2"),each=2))
        y.dat.lvl <- cbind(y.dat.lvl, const.lvls)
        y.dat.mass <- cbind(y.dat.conc$time, y.dat.conc[,-1] * y.dat.lvl[,-1]) # calculate the mass
        colnames(y.dat.mass) <- c("time",paste0(rep(unlist(lapply(cases, function(x) paste0(x[1], c("MaT1_","MaT2_"),x[2]))),times=3), rep(c("Lv0", "Lv1", "Lv2"),each=2)))
        ## make the data narrow again
        y.dat.mass <- gather(y.dat.mass, var, y, -time)
        y.dat <- y.dat.mass
    }
    ## Translate variable names for plotting
    y.dat$var <- as.factor(y.dat$var)
    y.dat$vartrans <- y.dat$var
    lv <- levels(y.dat$vartrans)
    if(length(translate.to)>0){
        lv[lv %in% translate.var] <- translate.to[match(lv[lv %in% translate.var], translate.var)]
    }
    levels(y.dat$vartrans) <- lv
    ## Convert days in y.dat into time for plotting purposes
    strt <- as.POSIXct("2008-01-01")
    y.dat$time <- strt + as.numeric(y.dat$time * 60*60*24)
    width = 10
    height = 3*length(variables)
    ## limit the data plotted to the specified region
    if(!is.null(xlim)) y.dat <- subset(y.dat, time >= xlim[1] & time <= xlim[2])
    if(sum(grepl("F[0-9].*Qstrm", variables)) > 0){
    # stack the fluxes of water or tracers that leave the Hrus on top of each other
        if(grepl("Tm[0-9]", variables)){yl <- expression("Load ("*mu*g*"/15min)")}else{yl <- "Streamflow (l/s)"}
        ggplot.obj <- ggplot(subset(y.dat, !grepl("C[0-9]+", var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y=yl, fill="")
        width = 15
        height = 10
    }else if(masses){
    # stack the masses of tracer in the different reservoirs on top of each other
        yl <- expression("Mass ("*mu*g~m^{-2}*")")
        ggplot.obj1 <- ggplot(subset(y.dat, grepl("T1", var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y=yl, fill="", caption=paste("Half-life:",round(half.life),"days","Sorpt. coeff.: ", round(Kd,2),"l/kg"))
        ggplot.obj2 <- ggplot(subset(y.dat, grepl("T2", var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y=yl, fill="")
        ggplot.obj <- gtable_rbind(ggplotGrob(ggplot.obj1), ggplotGrob(ggplot.obj2))
        dev.off()
        width = 15
        height = 10
    }else if(sum(grepl("U[0-9]?F[0-9]?[WT].*_Q[^s]", variables))>0){
        ## compare the output (streamflow or mass load) of the reservoirs of the same types of all HRUs
        ## if there are multiple SR outputs form different HRUs, add them together
        slow.res <- grepl("U[0-9]?F[0-9]?[WT].*_Qq_SR", variables)
        y.dat$var <- as.character(y.dat$var)
        y.dat2 <- y.dat
        y.dat2 <- subset(y.dat2, modobs!="obs")
        if(sum(slow.res)>1){
            ind <- grepl("U[0-9]?F[0-9]?[WT].*_Qq_SR",y.dat2$var)
            one <- y.dat2$var==(y.dat2$var[which(ind)[1]])
            all.SR <- as.numeric(tapply(y.dat2[ind,"y"], as.factor(as.numeric(y.dat2[ind,"time"])), sum))
            y.dat3 <- y.dat2[one,]
            y.dat3$y <- all.SR
            y.dat3$var <- "All groundwater"
            y.dat2[one,] <- y.dat3
            y.dat2 <- y.dat2[!(ind & !one),]
        }
        y.dat <- y.dat2
        ## Translate variable names for plotting
        y.dat$var <- as.factor(y.dat$var)
        y.dat$vartrans <- y.dat$var
        lv <- levels(y.dat$vartrans)
        if(length(translate.to)>0){
            lv[lv %in% translate.var] <- translate.to[match(lv[lv %in% translate.var], translate.var)]
        }
        levels(y.dat$vartrans) <- lv
        height <- 7
        fluxes.multhru <- TRUE
        ggplot.obj <- list()
        if(sum(grepl("Wv", variables))>0)  ggplot.obj <- c(ggplot.obj, list(ggplotGrob(ggplot(subset(y.dat, grepl("Wv", var) | grepl("All",var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y="Streamflow contribution (l/s)", fill=""))))
        if(sum(grepl("Tm1", variables))>0) ggplot.obj <- c(ggplot.obj, list(ggplotGrob(ggplot(subset(y.dat, grepl("Tm1", var)| grepl("All",var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y=expression("Load ("*mu*g*"/15min)"), fill=""))))
        if(sum(grepl("Tm2", variables))>0) ggplot.obj <- c(ggplot.obj, list(ggplotGrob(ggplot(subset(y.dat, grepl("Tm2", var) | grepl("All",var)), aes(x=time, y=y, fill=vartrans)) + geom_area() + labs(x="Time", y=expression("Load ("*mu*g*"/15min)"), fill=""))))
        ggplot.obj <- do.call(gtable_rbind, ggplot.obj)
    }else{
        ggplot.obj <- ggplot(y.dat, aes(x=time, y=y, shape=modobs, col=modobs)) + scale_x_datetime(limits=xlim) + scale_y_continuous(limits=ylim)
        ggplot.obj <- ggplot.obj + geom_point(data=subset(y.dat, modobs=="obs" & var!="P"), size=1.8) + geom_line(data=subset(y.dat, modobs!="obs" & var!="P"), size=1.2)+ theme_bw(base_size=20) + theme(title=element_text(size=14), axis.text.x=element_text(size=20))
        if("P" %in% y.dat$var) ggplot.obj <- ggplot.obj + geom_col(data=subset(y.dat, var=="P"))
        ggplot.obj <- ggplot.obj + labs(x="", y="", shape="", col="", title=names(vary)[1]) + facet_wrap(~vartrans, nrow=length(variables), scales=scales, labeller=label_parsed, strip.position="left")
    }
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
            if(masses | sum(grepl("U[0-9].*[WT]"))>0){
                grid.arrange(ggplot.obj)
            }else{
                dev.new()
                plot(ggplot.obj)
            }
        }else{
            nchar = length(unlist(strsplit(file, split = NULL)))
            pat = substr(file, nchar-2, nchar)
            if(pat == "pdf"){
                pdf(file, width = width, height = height)
            }else if(pat == "peg" | pat == "jpg"){
                jpeg(file, res = 400, units = "in", width = width, height = height)
            }else if(pat == "png"){
                png(file, res = 400, units = "in", width = width, height = height)
            }else stop("file type not recognized")
            if(masses){
                grid.arrange(ggplot.obj)
            }else{
                plot(ggplot.obj)
            }
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
