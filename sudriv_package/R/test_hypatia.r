.libPaths("/mnt/project/ammannlo/personalR/library")
## Try to run the 'package' version of the driver
library(rjson)
library(nloptr)

f.names <- list.files("/mnt/project/ammannlo/PhD/sudriv_package/R/")
f.names <- f.names[-grep("test", f.names)]
f.names <- f.names[-grep("~", f.names)]
f.names <- f.names[-grep("json", f.names)]
f.names <- f.names[-grep(".job", f.names)]
f.names <- f.names[-grep("errs_oss", f.names)]
f.names <- f.names[-grep("out_oss", f.names)]
f.names <- f.names[-grep("superflexdll", f.names)]
f.names <- f.names[-grep("outnames", f.names)]
sapply(f.names, source)

ob <- sudriv.setup(settings = "settings.json")

## Here we can define a binary vector (0,1) stating whether each parameter should be fitted or not. If saved as sudriv$model$fit.par it will not be overwritten by 'model.setup'

ob <- model.setup(ob, settings="settings.json")
ob <- optimizer.setup(ob, settings="settings.json")
ob <- likelihood.setup(ob, settings="settings.json")

ob2<- MLE.sudriv(ob)

## develop: run on one processor was successful. Implement a parallel version where one MLE is run on each processor. We could vary the (number of) parameters to be inferred. Also: start with global optimization and once this is sucessfully finished, refine the result with local optimization.
