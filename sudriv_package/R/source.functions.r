source.functions <- function(wrk.dir){

     setwd(wrk.dir)
    f.names <- list.files()
    f.names <- f.names[-grep("test", f.names)]
    f.names <- f.names[-grep("~", f.names)]
    f.names <- f.names[-grep("json", f.names)]
##    f.names <- f.names[-grep(".job", f.names)]
##    write.table(f.names, file="fnames.txt")
##    f.names <- f.names[-grep("errs_oss", f.names)]
##    f.names <- f.names[-grep("out_oss", f.names)]
    f.names <- f.names[-grep("superflexdll", f.names)]
    f.names <- f.names[-grep("Thumbs.db", f.names)]
    f.names <- f.names[-grep(".txt", f.names)]
    sapply(f.names, source)
}
