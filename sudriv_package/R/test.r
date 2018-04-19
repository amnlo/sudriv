rm(list = ls(all=TRUE))
## Try to run the 'package' version of the driver
library(rjson)
library(nloptr)

f.names <- list.files()
f.names <- f.names[-grep("test", f.names)]
f.names <- f.names[-grep("~", f.names)]
f.names <- f.names[-grep("json", f.names)]
f.names <- f.names[-grep("superflexdll", f.names)]
f.names <- f.names[-grep("outnames", f.names)]
sapply(f.names, source)

ob <- sudriv.setup(settings = "settings.json")
ob <- model.setup(ob, settings="settings.json")
ob <- optimizer.setup(ob, settings="settings.json")
ob <- likelihood.setup(ob, settings="settings.json")

ob2<- MLE.sudriv(ob)
