sudriv.setup <-
function(settings = "settings.json",
                            ...){
    options(list(stringsAsFactors = FALSE))
    if(class(settings) == "character"){
        settings <- fromJSON(file = settings, ...)
    }else if(class(settings != "list")){
        stop("Argument 'settings' needs to be a character specifying the settings file or a list containig the settings.")
    }

    ## ==================================================================
    ## Load input for superflex. Needs: 'dir.input'
    load(paste(settings$dir.input, "/input_sudriv_", settings$subcatchment, settings$input.tag, ".RData", sep = ""))
    ## Load layout. Needs: 'dir.input'
    load(paste(settings$dir.input, "/layout_sudriv_", settings$subcatchment, "_", settings$fit.vars, ".RData", sep = ""))
    ## Load observations. Needs: 'dir.input'
    load(paste(settings$dir.input, "/observations_sudriv_", settings$subcatchment, "_", settings$fit.vars, ".RData", sep = ""))

    ## ==========================
    ## construct sudriv object:
    sudriv <- list()
    sudriv$input          <- input_sudriv
    sudriv$layout         <- layout_sudriv
    sudriv$observations   <- observations_sudriv$observations
    sudriv$settings       <- settings   # settings for the driver (regarding optimization, sampling etc.)
    class(sudriv) <- "sudriv"

    return(sudriv)
}
