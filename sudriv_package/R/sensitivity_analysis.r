## This funtion performs a local sensitivity analysis on a sudriv object
sensitivity_analysis <- function(sudriv, vary.all=list()){
    allsub <- ifelse(sudriv$settings$subcatchment == "AAll", TRUE, FALSE)
    if(!is.null(sudriv$parameter.sample)) sudriv <- select.maxlikpars(sudriv)
    var.obs.q <- c("C1Wv_Qstream","C2Wv_Qstream","C3Wv_Qstream","C4Wv_Qstream","C5Wv_Qstream","P") ## observations and the model output of those are plotted
    var.obs.atra <- c("C1Tc1_Qstream", "C2Tc1_Qstream", "C3Tc1_Qstream", "C4Tc1_Qstream", "C5Tc1_Qstream")
    var.obs.terb <- c("C1Tc2_Qstream", "C2Tc2_Qstream", "C3Tc2_Qstream", "C4Tc2_Qstream", "C5Tc2_Qstream")
    if(length(vary.all)==0) vary.all <- list("Glo%Cmlt_E"=c(-0.3,0.5), "Glo%Cmlt_Dspl_SD"=c(0.6,0.9))
    for(par.curr in names(vary.all)){
        vary <- vary.all[par.curr]
        savepath <- paste("../output/", su$settings$subcatchment, su$settings$structure, "/", tag, "/sensitivity/",par.curr, sep="")
        savepath <- gsub("%", "_", savepath)
        savepath <- gsub("\\[.*\\]", "", savepath)
        dir.create(savepath)
        if(!allsub){
            var.obs.q <- var.obs.q[1]
            var.obs.atra <- var.obs.atra[1]
            var.obs.terb <- var.obs.terb[1]
            var.obs.mult <- c(var.obs.q[1], var.obs.atra[1], var.obs.terb[1], "P")
        }else{
            var.obs.mult <- c("C1Wv_Qstream","C1Tc1_Qstream","C1Tc2_Qstream","C3Wv_Qstream","C3Tc1_Qstream","C3Tc2_Qstream","P")
        }
        if(grepl("Str07", sett)){
            if(allsub){
                var.mod1 <- c("U1F5Wv_Sf1", "U5F5Wv_Si1", "U5F5Wv_Su1") ## only the model output of those are plotted
                var.mod2 <- c("U2F5Wv_Sr1", "U3F2Wv_Sf1", "U3F4Wv_Ss1", "U5F5Wv_Ss1")
                if(grepl("Str072", sett)){
                    var.mod2 <- c("U2F5Wv_Sr1", "U2F4Wv_Ss1", "U5F5Wv_Ss1")
                }
                var.modTcCon<- c("U2F1Tc1Lv1_Si1", "U2F1Tc1Lv2_Si1", "U2F1Tc2Lv1_Si1", "U2F1Tc2Lv2_Si1")
                var.modTcDrn<- c("U3F1Tc1Lv1_Si1", "U3F1Tc1Lv2_Si1", "U3F1Tc2Lv1_Si1", "U3F1Tc2Lv2_Si1")
                if(grepl("Str071", sett)){
                    var.modTcCon<- c("U2F1Tc1Lv1_Si1", "U2F1Tc2Lv1_Si1")
                    var.modTcDrn<- c("U3F1Tc1Lv1_Si1", "U3F1Tc2Lv1_Si1")
                }
                var.mod3 <- c("U1F5Tm1_Qstrm", "U2F5Tm1_Qstrm", "U3F5Tm1_Qstrm", "U4F5Tm1_Qstrm", "U5F5Tm1_Qstrm")
                var.mod4 <- c("U1F5Tm2_Qstrm", "U2F5Tm2_Qstrm", "U3F5Tm2_Qstrm", "U4F5Tm2_Qstrm", "U5F5Tm2_Qstrm")
                var.mod5 <- c("U1F5Wv_Qstrm", "U2F5Wv_Qstrm", "U3F5Wv_Qstrm", "U4F5Wv_Qstrm", "U5F5Wv_Qstrm")
                pp.mod1  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod1, vary=vary)
                pp.mod2  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod2, vary=vary)
                pp.modTcCon <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.modTcCon, vary=vary)
                pp.modTcDrn <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.modTcDrn, vary=vary)
                pp.mod3  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod3, vary=vary)
                pp.mod4  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod4, vary=vary)
                pp.mod5  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod5, vary=vary)
            }else{
                var.mod1 <- c("U1F1Wv_Sf1", "U5F1Wv_Si1", "U5F1Wv_Su1") ## only the model output of those are plotted
                var.mod2 <- c("U2F1Wv_Sr1", "U3F1Wv_Sf1", "U3F1Wv_Ss1", "U5F1Wv_Ss1")
                if(grepl("Str072", sett)){
                    var.mod2 <- c("U2F1Wv_Sr1", "U2F1Wv_Ss1", "U5F1Wv_Ss1")
                }
                var.modTcCon<- c("U2F1Tc1Lv1_Si1", "U2F1Tc1Lv2_Si1", "U2F1Tc2Lv1_Si1", "U2F1Tc2Lv2_Si1")
                var.modTcDrn<- c("U3F1Tc1Lv1_Si1", "U3F1Tc1Lv2_Si1", "U3F1Tc2Lv1_Si1", "U3F1Tc2Lv2_Si1")
                if(grepl("Str071", sett)){
                    var.modTcCon<- c("U2F1Tc1Lv1_Si1", "U2F1Tc2Lv1_Si1")
                    var.modTcDrn<- c("U3F1Tc1Lv1_Si1", "U3F1Tc2Lv1_Si1")
                }
                var.mod3 <- c("U1F1Tm1_Qstrm", "U2F1Tm1_Qstrm", "U3F1Tm1_Qstrm", "U4F1Tm1_Qstrm", "U5F1Tm1_Qstrm")
                var.mod4 <- c("U1F1Tm2_Qstrm", "U2F1Tm2_Qstrm", "U3F1Tm2_Qstrm", "U4F1Tm2_Qstrm", "U5F1Tm2_Qstrm")
                var.mod5 <- c("U1F1Wv_Qstrm", "U2F1Wv_Qstrm", "U3F1Wv_Qstrm", "U4F1Wv_Qstrm", "U5F1Wv_Qstrm")
                pp.mod1  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod1, vary=vary)
                pp.mod2  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod2, vary=vary)
                pp.modTcCon <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.modTcCon, vary=vary)
                pp.modTcDrn <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.modTcDrn, vary=vary)
                pp.mod3  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod3, vary=vary)
                pp.mod4  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod4, vary=vary)
                pp.mod5  <- prepare.plot.layout(sudriv=sudriv, var.obs=c("C1Wv_Qstream"), var.mod=var.mod5, vary=vary)
            }
        }
        E0 <- as.POSIXct(c("2009-05-21 12:00", "2009-05-23 00:00"))
	E1 <- as.POSIXct(c("2009-05-26 12:00", "2009-05-28 00:00"))
        E2 <- as.POSIXct(c("2009-06-06 00:00", "2009-06-07 18:00"))
        E3 <- as.POSIXct(c("2009-06-25 00:00", "2009-06-30 00:00"))
	E4 <- as.POSIXct(c("2009-07-10 00:00", "2009-07-25 18:00"))
        Ebeg <- as.POSIXct(c("2009-03-01 00:00", "2009-05-15 00:00"))
        Eapp <- as.POSIXct(c("2009-05-15 00:00", "2009-07-30 00:00"))
        Ewint <- as.POSIXct(c("2008-10-01 00:00", "2009-03-31 00:00"))
	## xlim <- as.POSIXct(c("2026-03-01", "2026-08-01"))
        pp.q <- prepare.plot.layout(sudriv=sudriv, var.obs=var.obs.q, vary=vary)
        pp.atra <- prepare.plot.layout(sudriv=sudriv, var.obs=var.obs.atra, vary=vary)
        pp.terb <- prepare.plot.layout(sudriv=sudriv, var.obs=var.obs.terb, vary=vary)
        pp.mult <- prepare.plot.layout(sudriv=sudriv, var.obs=var.obs.mult, vary=vary)
        vary.ext <- pp.mult$vary
        hru.areas = matrix(c(0, 5420, 11512, 212, 154126.8,
                             0, 7648, 29340, 24, 88277.53,
                             7660, 6060, 62300, 16, 170386.4,
                             2136, 9572, 40176, 512, 114949.5,
                             5710, 39592, 43692, 2116, 381456.4),byrow=TRUE,ncol=5)
	if(!allsub){
            hru.areas <- matrix(colSums(hru.areas), ncol=5)
	}
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_E0_", tag, ".png", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_E1_", tag, ".png", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_E2_", tag, ".png", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_E3_", tag, ".png", sep=""), xlim=E3, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_E4_", tag, ".pdf", sep=""), xlim=E4, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.q$layout.mod$layout, y.mod=pp.q$y.mod, , layout.obs=pp.q$layout.obs, y.obs=pp.q$y.obs, vary=vary.ext, variables=var.obs.q, file=paste(savepath,"/states_q_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_E0_", tag, ".png", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_E1_", tag, ".png", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_E2_", tag, ".png", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_E3_", tag, ".png", sep=""), xlim=E3, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_E4_", tag, ".png", sep=""), xlim=E4, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.atra$layout.mod$layout, y.mod=pp.atra$y.mod, , layout.obs=pp.atra$layout.obs, y.obs=pp.atra$y.obs, vary=vary.ext, variables=var.obs.atra, file=paste(savepath,"/states_atra_Eapp_", tag, ".png", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.terb$layout.mod$layout, y.mod=pp.terb$y.mod, , layout.obs=pp.terb$layout.obs, y.obs=pp.terb$y.obs, vary=vary.ext, variables=var.obs.terb, file=paste(savepath,"/states_terb_E0_", tag, ".png", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.terb$layout.mod$layout, y.mod=pp.terb$y.mod, , layout.obs=pp.terb$layout.obs, y.obs=pp.terb$y.obs, vary=vary.ext, variables=var.obs.terb, file=paste(savepath,"/states_terb_E1_", tag, ".png", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.terb$layout.mod$layout, y.mod=pp.terb$y.mod, , layout.obs=pp.terb$layout.obs, y.obs=pp.terb$y.obs, vary=vary.ext, variables=var.obs.terb, file=paste(savepath,"/states_terb_Eapp_", tag, ".png", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.mult$layout.mod$layout, y.mod=pp.mult$y.mod, , layout.obs=pp.mult$layout.obs, y.obs=pp.mult$y.obs, vary=vary.ext, variables=var.obs.mult, file=paste(savepath,"/states_mult_Eapp_", tag, ".png", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.mult$layout.mod$layout, y.mod=pp.mult$y.mod, , layout.obs=pp.mult$layout.obs, y.obs=pp.mult$y.obs, vary=vary.ext, variables=var.obs.mult, file=paste(savepath,"/states_mult_E0_", tag, ".png", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.mult$layout.mod$layout, y.mod=pp.mult$y.mod, , layout.obs=pp.mult$layout.obs, y.obs=pp.mult$y.obs, vary=vary.ext, variables=var.obs.mult, file=paste(savepath,"/states_mult_E1_", tag, ".png", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.mult$layout.mod$layout, y.mod=pp.mult$y.mod, , layout.obs=pp.mult$layout.obs, y.obs=pp.mult$y.obs, vary=vary.ext, variables=var.obs.mult, file=paste(savepath,"/states_mult_E2_", tag, ".png", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
        plot.results(layout.mod=pp.mult$layout.mod$layout, y.mod=pp.mult$y.mod, , layout.obs=pp.mult$layout.obs, y.obs=pp.mult$y.obs, vary=vary.ext, variables=var.obs.mult, file=paste(savepath,"/states_mult_E3_", tag, ".png", sep=""), xlim=E3, per.area=TRUE, hru.areas=hru.areas)
        if(grepl("Str07", sett)){
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_Ebeg_", tag, ".pdf", sep=""), xlim=Ebeg, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_E2_", tag, ".pdf", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod1$layout.mod$layout, y.mod=pp.mod1$y.mod, , layout.obs=pp.mod1$layout.obs, y.obs=pp.mod1$y.obs, vary=vary.ext, variables=var.mod1, file=paste(savepath,"/states_mod1_E3_", tag, ".pdf", sep=""), xlim=E3, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_Ebeg_", tag, ".pdf", sep=""), xlim=Ebeg, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_E2_", tag, ".pdf", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod2$layout.mod$layout, y.mod=pp.mod2$y.mod, , layout.obs=pp.mod2$layout.obs, y.obs=pp.mod2$y.obs, vary=vary.ext, variables=var.mod2, file=paste(savepath,"/states_mod2_E3_", tag, ".pdf", sep=""), xlim=E3, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcCon$layout.mod$layout, y.mod=pp.modTcCon$y.mod, , layout.obs=pp.modTcCon$layout.obs, y.obs=pp.modTcCon$y.obs, vary=vary.ext, variables=var.modTcCon, file=paste(savepath,"/states_modTcCon_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcCon$layout.mod$layout, y.mod=pp.modTcCon$y.mod, , layout.obs=pp.modTcCon$layout.obs, y.obs=pp.modTcCon$y.obs, vary=vary.ext, variables=var.modTcCon, file=paste(savepath,"/states_modTcCon_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcCon$layout.mod$layout, y.mod=pp.modTcCon$y.mod, , layout.obs=pp.modTcCon$layout.obs, y.obs=pp.modTcCon$y.obs, vary=vary.ext, variables=var.modTcCon, file=paste(savepath,"/states_modTcCon_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcCon$layout.mod$layout, y.mod=pp.modTcCon$y.mod, , layout.obs=pp.modTcCon$layout.obs, y.obs=pp.modTcCon$y.obs, vary=vary.ext, variables=var.modTcCon, file=paste(savepath,"/states_modTcCon_E2_", tag, ".pdf", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcDrn$layout.mod$layout, y.mod=pp.modTcDrn$y.mod, , layout.obs=pp.modTcDrn$layout.obs, y.obs=pp.modTcDrn$y.obs, vary=vary.ext, variables=var.modTcDrn, file=paste(savepath,"/states_modTcDrn_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcDrn$layout.mod$layout, y.mod=pp.modTcDrn$y.mod, , layout.obs=pp.modTcDrn$layout.obs, y.obs=pp.modTcDrn$y.obs, vary=vary.ext, variables=var.modTcDrn, file=paste(savepath,"/states_modTcDrn_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcDrn$layout.mod$layout, y.mod=pp.modTcDrn$y.mod, , layout.obs=pp.modTcDrn$layout.obs, y.obs=pp.modTcDrn$y.obs, vary=vary.ext, variables=var.modTcDrn, file=paste(savepath,"/states_modTcDrn_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.modTcDrn$layout.mod$layout, y.mod=pp.modTcDrn$y.mod, , layout.obs=pp.modTcDrn$layout.obs, y.obs=pp.modTcDrn$y.obs, vary=vary.ext, variables=var.modTcDrn, file=paste(savepath,"/states_modTcDrn_E2_", tag, ".pdf", sep=""), xlim=E2, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod3$layout.mod$layout, y.mod=pp.mod3$y.mod, , layout.obs=pp.mod3$layout.obs, y.obs=pp.mod3$y.obs, vary=vary.ext, variables=var.mod3, file=paste(savepath,"/states_fluxA_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod3$layout.mod$layout, y.mod=pp.mod3$y.mod, , layout.obs=pp.mod3$layout.obs, y.obs=pp.mod3$y.obs, vary=vary.ext, variables=var.mod3, file=paste(savepath,"/states_fluxA_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod3$layout.mod$layout, y.mod=pp.mod3$y.mod, , layout.obs=pp.mod3$layout.obs, y.obs=pp.mod3$y.obs, vary=vary.ext, variables=var.mod3, file=paste(savepath,"/states_fluxA_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod4$layout.mod$layout, y.mod=pp.mod4$y.mod, , layout.obs=pp.mod4$layout.obs, y.obs=pp.mod4$y.obs, vary=vary.ext, variables=var.mod4, file=paste(savepath,"/states_fluxT_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod4$layout.mod$layout, y.mod=pp.mod4$y.mod, , layout.obs=pp.mod4$layout.obs, y.obs=pp.mod4$y.obs, vary=vary.ext, variables=var.mod4, file=paste(savepath,"/states_fluxT_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod4$layout.mod$layout, y.mod=pp.mod4$y.mod, , layout.obs=pp.mod4$layout.obs, y.obs=pp.mod4$y.obs, vary=vary.ext, variables=var.mod4, file=paste(savepath,"/states_fluxT_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=TRUE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod5$layout.mod$layout, y.mod=pp.mod5$y.mod, , layout.obs=pp.mod5$layout.obs, y.obs=pp.mod5$y.obs, vary=vary.ext, variables=var.mod5, file=paste(savepath,"/states_fluxQ_E1_", tag, ".pdf", sep=""), xlim=E1, per.area=FALSE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod5$layout.mod$layout, y.mod=pp.mod5$y.mod, , layout.obs=pp.mod5$layout.obs, y.obs=pp.mod5$y.obs, vary=vary.ext, variables=var.mod5, file=paste(savepath,"/states_fluxQ_E0_", tag, ".pdf", sep=""), xlim=E0, per.area=FALSE, hru.areas=hru.areas)
            plot.results(layout.mod=pp.mod5$layout.mod$layout, y.mod=pp.mod5$y.mod, , layout.obs=pp.mod5$layout.obs, y.obs=pp.mod5$y.obs, vary=vary.ext, variables=var.mod5, file=paste(savepath,"/states_fluxQ_Eapp_", tag, ".pdf", sep=""), xlim=Eapp, per.area=FALSE, hru.areas=hru.areas)
        }
    }
}
