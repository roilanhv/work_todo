sib2.eval.SA <- function (j, Thetas, nparamsets, N, X.Boundaries, write2disk = FALSE, 
    REPORT, verbose, digits, model.FUN, model.FUN.args, parallel, 
    ncores, part.dirs) 
{
    if (parallel != "none") 
        model.FUN.args <- modifyList(model.FUN.args, list(model.drty = part.dirs[j]))
    nelements <- 2
    out <- vector("list", nelements)
    gof.is.numeric <- FALSE
    while (!gof.is.numeric) {
        params <- Thetas[j, ]
        suppressWarnings(param.values <- as.numeric(formatC(params, 
            format = "E", digits = digits)))
        model.FUN.args <- modifyList(model.FUN.args, as.list( params))
        hydromod.out <- do.call(model.FUN, as.list(model.FUN.args))
        out[[1]] <- as.numeric(hydromod.out[["GoF"]])
        out[[2]] <- hydromod.out[["sim"]]
        ifelse(is.finite(out[[1]]), gof.is.numeric <- TRUE, gof.is.numeric <- FALSE)
        if (!gof.is.numeric) {
            warning(" parameter set ", j, ": not numeric GoF ! => it was replaced")
            tmp <- rLHS(n = N, ranges = X.Boundaries)
            Thetas[j, ] <- tmp[j, ]
        }
    }
    names(out)[1:nelements] <- c("GoF", "model.out")
    if (j/REPORT == floor(j/REPORT)) {
        if (verbose) 
            message("[ Parameter set ", format(j, width = 4, 
                justify = "left"), "/", nparamsets, ". Finished.   GoF: ", 
                format(hydromod.out[["GoF"]], scientific = TRUE, 
                  digits = digits), "]")
    }
    return(out)
}