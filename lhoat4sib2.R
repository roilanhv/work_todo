
function (fn = 'sib2' , ..., lower = -Inf, upper = Inf, control = list(), 
    model.FUN = NULL, model.FUN.args = list()) 
{ # Trocando o nome de "hydromod" pelo sib2
    
    if (missing(fn)) {
        stop("Missing argument: 'fn' must be provided")
    } else if (is.character(fn) | is.function(fn)) {
        if (is.character(fn)) {
            fn.name <- fn
            fn <- match.fun(fn)
        } else if (is.function(fn)) {
            fn.name <- as.character(substitute(fn))
            fn <- fn
        }
    } else stop("Missing argument: 'class(fn)' must be in c('function', 'character')")
    
    
    if (length(lower) != length(upper)) 
        stop(paste("Invalid argument: 'length(lower) != length(upper) (", 
            length(lower), "!=", length(upper), ")'", sep = ""))
    # Lista com os argumentos por default
    con <- list(N = 3, f = 0.15, drty.in = "PSO.in", drty.out = "LH_OAT", 
        param.ranges = "ParamRanges.txt", digits = 7, normalise = FALSE, 
        normaliseRanking = FALSE, gof.name = "GoF", do.plots = FALSE, 
        write2disk = TRUE, verbose = TRUE, REPORT = 100, parallel = c("none", 
            "multicore", "parallel", "parallelWin"), par.nnodes = NA, 
        par.pkgs = c())
    
    parallel <- match.arg(control[["parallel"]], con[["parallel"]])
    nmsC <- names(con)
    # Substituindo os argumentos por default por os introduzidos nos 
    # argumentos da função
    con[(namc <- names(control))] <- control
    # Conferindo argumentos
    if (length(noNms <- namc[!namc %in% nmsC])) 
        warning("[Unknown names in control: ", paste(noNms, 
            collapse = ", "), " (not used) !]")
    # Separando os argumentos
    N <- con[["N"]]
    f <- con[["f"]]
    drty.in <- con[["drty.in"]]
    drty.out <- con[["drty.out"]]
    param.ranges <- con[["param.ranges"]]
    digits <- con[["digits"]]
    normalise <- as.logical(con[["normalise"]])
    gof.name <- con[["gof.name"]]
    do.plots <- as.logical(con[["do.plots"]])
    write2disk <- as.logical(con[["write2disk"]])
    verbose <- as.logical(con[["verbose"]])
    REPORT <- con[["REPORT"]]
    par.nnodes <- con[["par.nnodes"]]
    par.pkgs <- con[["par.pkgs"]]
    if (trunc(N) != N) 
        stop("Invalid argument: 'N' must be integer !")
    if ((f <= 0) | (f >= 1)) 
        stop("Invalid argument: 'f' must be in ]0, 1[")
    if (fn.name == "sib2") {
        if (!file.exists(param.ranges)) 
            stop(paste("Invalid argument: The file '", param.ranges, 
                "' does not exist !", sep = ""))
        if (is.null(model.FUN)) {
            stop("'model.FUN' has to be defined !")
        }
        else {
            model.FUN.name <- model.FUN
            model.FUN <- match.fun(model.FUN)
        }
        if (length(model.FUN.args) == 0) {
            warning("'model.FUN.args' is an empty list. Are you sure your model doesn't have any argument(s) ?")
        }
        else {
            model.FUN.argsDefaults <- formals(model.FUN)
            model.FUN.args <- modifyList(model.FUN.argsDefaults, 
                model.FUN.args)
        }
    }
    if (fn.name == "sib2") {
        if (verbose) 
            message("==============================================================")
        if (verbose) 
            message("[   1)   Reading 'param.ranges' ...                          ]")
        if (verbose) 
            message("==============================================================")
        X.Boundaries <- read.ParameterRanges(ParamRanges.fname = param.ranges)
        lower <- X.Boundaries[, 1]
        upper <- X.Boundaries[, 2]
    }
    else {
        if ((lower[1L] == -Inf) || (upper[1L] == Inf)) {
            stop("Invalid argument: 'lower' and 'upper' boundaries must be finite !!'")
        }
        else X.Boundaries <- cbind(lower, upper)
    }
    P <- nrow(X.Boundaries)
    if (is.null(rownames(X.Boundaries))) {
        param.IDs <- paste("Param", 1:P, sep = "")
    }
    else param.IDs <- rownames(X.Boundaries)
    nparamsets <- (P + 1) * N
    if (nparamsets < REPORT) {
        REPORT <- nparamsets
        warning("[ 'REPORT' is greater than 'nparamsets' => 'REPORT=nparamsets' ]")
    }
    if (verbose) 
        message("                                                              ")
    if (verbose) 
        message("[ Number of strata for each parameter (N) : ", 
            N, " ]")
    if (verbose) 
        message("                                                              ")
    if (verbose) 
        message("[ Number of Parameter Sets to be run      : ", 
            nparamsets, " ]")
    if (normalise) {
        lower.ini <- lower
        upper.ini <- upper
        X.Boundaries.ini <- X.Boundaries
        LOWER.ini <- matrix(rep(lower.ini, nparamsets), nrow = nparamsets, 
            byrow = TRUE)
        UPPER.ini <- matrix(rep(upper.ini, nparamsets), nrow = nparamsets, 
            byrow = TRUE)
        lower <- rep(0, P)
        upper <- rep(1, P)
        X.Boundaries <- cbind(lower, upper)
        rownames(X.Boundaries) <- param.IDs
    }
    if (drty.out == basename(drty.out)) 
        drty.out <- paste(getwd(), "/", drty.out, sep = "")
    if (!file.exists(file.path(drty.out))) {
        if (write2disk) {
            suppressWarnings(dir.create(file.path(drty.out)))
            if (verbose) 
                message("                                            ")
            if (verbose) 
                message("[ Output directory '", basename(drty.out), 
                    "' was created on: '", dirname(drty.out), 
                    "' ]")
            if (verbose) 
                message("                                            ")
        }
    }
    if (parallel != "none") {
        if (((parallel == "multicore") | (parallel == "parallel")) & 
            ((R.version$os == "mingw32") | (R.version$os == 
                "mingw64"))) 
            stop("[ Fork clusters are not supported on Windows =>  'parallel' can not be set to '", 
                parallel, "' ]")
        ifelse(parallel == "parallelWin", parallel.pkg <- "parallel", 
            parallel.pkg <- parallel)
        if (!require(parallel)) {
            warning("[ Package '", parallel.pkg, "' is not installed =>  parallel='none' ]")
            parallel <- "none"
        }
        else {
            if (verbose) 
                message("                               ")
            if (verbose) 
                message("[ Parallel initialization ... ]")
            fn1 <- function(i, x) fn(x[i, ])
            nnodes.pc <- detectCores()
            if (verbose) 
                message("[ Number of cores/nodes detected: ", 
                    nnodes.pc, " ]")
            if ((parallel == "parallel") | (parallel == "parallelWin")) {
                logfile.fname <- paste(file.path(drty.out), 
                    "/", "parallel_logfile.txt", sep = "")
                if (file.exists(logfile.fname)) 
                    file.remove(logfile.fname)
            }
            if (is.na(par.nnodes)) {
                par.nnodes <- nnodes.pc
            }
            else if (par.nnodes > nnodes.pc) {
                warning("[ 'nnodes' > number of detected cores (", 
                    par.nnodes, ">", nnodes.pc, ") =>  par.nnodes=", 
                    nnodes.pc, " ] !" )
                par.nnodes <- nnodes.pc
            }
            if (par.nnodes > nparamsets) {
                warning("[ 'par.nnodes' > N*(P+1) (", par.nnodes, 
                    ">", nparamsets, ") =>  par.nnodes=", nparamsets, 
                    " ] !")
                par.nnodes <- nparamsets
            }
            if (verbose) 
                message("[ Number of cores/nodes used    : ", 
                    par.nnodes, " ]")
            if (parallel == "parallel") {
                ifelse(write2disk, cl <- makeForkCluster(nnodes = par.nnodes, 
                    outfile = logfile.fname), cl <- makeForkCluster(nnodes = par.nnodes))
            }
            else if (parallel == "parallelWin") {
                ifelse(write2disk, cl <- makeCluster(par.nnodes, 
                    outfile = logfile.fname), cl <- makeCluster(par.nnodes))
                pckgFn <- function(packages) {
                    for (i in packages) library(i, character.only = TRUE)
                }
                clusterCall(cl, pckgFn, par.pkgs)
                clusterExport(cl, ls.str(mode = "function", 
                    envir = .GlobalEnv))
                if (fn.name == "hydromod") {
                    clusterExport(cl, model.FUN.args$out.FUN)
                    clusterExport(cl, model.FUN.args$gof.FUN)
                }
            }
            if (fn.name == "sib2") {
                if (!("model.drty" %in% names(formals(hydromod)))) {
                    stop("[ Invalid argument: 'model.drty' has to be an argument of the 'hydromod' function! ]")
                }
                else {
                    model.drty <- path.expand(model.FUN.args$dir_out)
                    files <- list.files(model.drty, full.names = TRUE, 
                        include.dirs = TRUE)
                    tmp <- which(basename(files) == "parallel")
                    if (length(tmp) > 0) 
                        files <- files[-tmp]
                    parallel.drty <- paste(file.path(model.drty), 
                        "/parallel", sep = "")
                    if (file.exists(parallel.drty)) {
                        if (verbose) 
                            message("[ Removing the 'parallel' directory ... ]")
                        try(unlink(parallel.drty, recursive = TRUE, 
                            force = TRUE))
                    }
                    dir.create(parallel.drty)
                    mc.dirs <- character(par.nnodes)
                    if (verbose) 
                        message("                                                     ")
                    for (i in 1:par.nnodes) {
                        mc.dirs[i] <- paste(parallel.drty, "/", 
                            i, "/", sep = "")
                        dir.create(mc.dirs[i])
                        if (verbose) 
                            message("[ Copying model input files to directory '", 
                                mc.dirs[i], "' ... ]")
                        file.copy(from = files, to = mc.dirs[i], 
                            overwrite = TRUE, recursive = TRUE)
                    }
                    n <- ceiling(nparamsets/par.nnodes)
                    part.dirs <- rep(mc.dirs, n)[1:nparamsets]
                }
            }
        }
    }
    if (write2disk) {
        InfoTXT.fname <- paste(file.path(drty.out), "/", "LH_OAT-logfile.txt", 
            sep = "")
        InfoTXT.TextFile <- file(InfoTXT.fname, "w+")
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        writeLines(c("hydroPSO version     :", sessionInfo()$otherPkgs$hydroPSO$Version), 
            InfoTXT.TextFile, sep = "  ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("hydroPSO Built       :", sessionInfo()$otherPkgs$hydroPSO$Built), 
            InfoTXT.TextFile, sep = "  ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("R version            :", sessionInfo()[[1]]$version.string), 
            InfoTXT.TextFile, sep = "  ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("Platform             :", sessionInfo()[[1]]$platform), 
            InfoTXT.TextFile, sep = "  ")
        writeLines("", InfoTXT.TextFile)
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        Time.Ini <- Sys.time()
        writeLines(c("Starting Time        :", date()), InfoTXT.TextFile, 
            sep = "  ")
        writeLines("", InfoTXT.TextFile)
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        writeLines(c("N (number of strata) :", N), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("f (changing factor)  :", f), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("drty.in              :", drty.in), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("drty.out             :", drty.out), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("param.ranges         :", param.ranges), 
            InfoTXT.TextFile, sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("digits               :", digits), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("gof.name             :", gof.name), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("do.plots             :", do.plots), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("write2disk           :", write2disk), 
            InfoTXT.TextFile, sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("verbose              :", verbose), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("normalise            :", normalise), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines(c("parallel             :", parallel), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        if (parallel != "none") {
            writeLines(c("par.nnodes           :", par.nnodes), 
                InfoTXT.TextFile, sep = " ")
            writeLines("", InfoTXT.TextFile)
            writeLines(c("par.pkgs             :", par.pkgs), 
                InfoTXT.TextFile, sep = " ")
            writeLines("", InfoTXT.TextFile)
        }
        if (fn.name == "sib2") {
            try(writeLines(c("hydromod function    :", model.FUN.name), 
                InfoTXT.TextFile, sep = " "), TRUE)
            writeLines("", InfoTXT.TextFile)
            writeLines(c("hydromod args        :"), InfoTXT.TextFile, 
                sep = " ")
            writeLines("", InfoTXT.TextFile)
            for (i in 1:length(model.FUN.args)) {
                arg.name <- names(model.FUN.args)[i]
                arg.name <- format(paste("  ", arg.name, sep = ""), 
                    width = 20, justify = "left")
                arg.value <- ""
                arg.value <- try(as.character(as.character(model.FUN.args[i])), 
                    TRUE)
                writeLines(c(arg.name, ":", arg.value), InfoTXT.TextFile, 
                    sep = " ")
                writeLines("", InfoTXT.TextFile)
            }
        }
        close(InfoTXT.TextFile)
    }
    if (write2disk) {
        model.out.text.fname <- paste(file.path(drty.out), "/", 
            "LH_OAT-out.txt", sep = "")
        model.out.text.file <- file(model.out.text.fname, "w+")
        close(model.out.text.file)
        gof.text.fname <- paste(file.path(drty.out), "/", "LH_OAT-gof.txt", 
            sep = "")
        gof.text.file <- file(gof.text.fname, "w+")
        writeLines(c(gof.name, param.IDs), gof.text.file, sep = " ")
        writeLines("", gof.text.file)
        close(gof.text.file)
    }
    if (verbose) 
        message("                                                              ")
    if (verbose) 
        message("==============================================================")
    if (verbose) 
        message("[  2)   Initial  LHS ...                                     ]")
    if (verbose) 
        message("==============================================================")
    Theta.Ini <- rLHS(n = N, ranges = X.Boundaries)
    if (verbose) 
        message("                                                              ")
    if (verbose) 
        message("==============================================================")
    if (verbose) 
        message("[  3)   Running LH-OAT ...                                   ]")
    if (verbose) 
        message("==============================================================")
    S <- matrix(NA, ncol = P, nrow = N)
    colnames(S) <- param.IDs
    Thetas <- matrix(NA, nrow = nparamsets, ncol = P)
    colnames(Thetas) <- param.IDs
    Theta.index <- seq(1, nparamsets, by = (P + 1))
    for (j in 1:N) {
        Thetas[Theta.index[j], ] <- Theta.Ini[j, ]
        for (i in 1:P) {
            canonical <- rep(1, P)
            canonical[i] <- 1 + f * sign(rnorm(1))
            Theta.New <- Theta.Ini[j, ] * canonical
            Thetas[Theta.index[j] + i, ] <- Theta.New
        }
    }
    gof <- rep(NA, nparamsets)
    ModelOut <- vector("list", nparamsets)
    if (write2disk) {
        model.out.text.file <- file(model.out.text.fname, "a")
        gof.text.file <- file(gof.text.fname, "a")
    }
    else {
        model.out.text.file <- ""
        gof.text.file <- ""
    }
    if (normalise) {
        Xn <- Thetas * (UPPER.ini - LOWER.ini) + LOWER.ini
    }else Xn <- Thetas
    if (fn.name != "sib2") {
        if (parallel == "none") {
            GoF <- apply(Xn, fn, MARGIN = 1, ...)
        } else if (parallel == "multicore") {
            GoF <- unlist(mclapply(1:nparamsets, FUN = fn1, 
                x = Xn, ..., mc.cores = par.nnodes, mc.silent = TRUE))
        } else if ((parallel == "parallel") | (parallel == "parallelWin")) {
            GoF <- parRapply(cl = cl, x = Xn, FUN = fn, ...)
        }
        gof[1:nparamsets] <- GoF
        ModelOut[1:nparamsets] <- GoF
    } else {
        if (parallel == "none") {
            out <- lapply(1:nparamsets, 
                          function(j) sib2.eval.SA(j, 
                                                   Thetas = Xn, 
                                                   nparamsets = nparamsets,
                                                   N = N, 
                                                   X.Boundaries = X.Boundaries, 
                                                   write2disk = write2disk, 
                                                   REPORT = REPORT, 
                                                   verbose = verbose, 
                                                   digits = digits,
                                                   model.FUN = model.FUN, 
                                                   model.FUN.args = model.FUN.args, 
                                                   parallel = parallel)
                          )
        }
        else if ((parallel == "parallel") | (parallel == "parallelWin")) {
            out <- clusterApply(cl = cl, x = 1:nparamsets, fun = hydromod.eval.SA, 
                Thetas = Xn, nparamsets = nparamsets, N = N, 
                X.Boundaries = X.Boundaries, write2disk = write2disk, 
                REPORT = REPORT, verbose = verbose, digits = digits, 
                model.FUN = model.FUN, model.FUN.args = model.FUN.args, 
                parallel = parallel, ncores = par.nnodes, part.dirs = part.dirs)
        }
        else if (parallel == "multicore") {
            out <- mclapply(1:nparamsets, hydromod.eval.SA, 
                Thetas = Xn, nparamsets = nparamsets, N = N, 
                X.Boundaries = X.Boundaries, write2disk = write2disk, 
                REPORT = REPORT, verbose = verbose, digits = digits, 
                model.FUN = model.FUN, model.FUN.args = model.FUN.args, 
                parallel = parallel, ncores = par.nnodes, part.dirs = part.dirs, 
                mc.cores = par.nnodes, mc.silent = TRUE, mc.cleanup = TRUE)
        }
        for (j in 1:nparamsets) {
            gof[j] <- out[[j]][["GoF"]]
            ModelOut[[j]] <- out[[j]][["model.out"]]
            if (write2disk) {
                suppressWarnings(param.values <- as.numeric(formatC(Xn[j, 
                    ], format = "E", digits = digits)))
                writeLines(as.character(out[[j]][["model.out"]]), 
                    model.out.text.file, sep = " ")
                writeLines("", model.out.text.file)
                flush(model.out.text.file)
                suppressWarnings(writeLines(as.character(c(formatC(out[[j]][["GoF"]], 
                    format = "E", digits = digits, flag = " "), 
                    formatC(param.values, format = "E", digits = digits, 
                        flag = " "))), gof.text.file, sep = "  "))
                writeLines("", gof.text.file)
                flush(gof.text.file)
            }
        }
    }
    if (write2disk) {
        if ((fn.name != "hydromod")) {
            if (verbose) 
                message("==============================================================")
            if (verbose) 
                message("[  5)   Writing output files ...                             ]")
            if (verbose) 
                message("==============================================================")
            for (j in 1:nparamsets) {
                writeLines(as.character(gof[j]), model.out.text.file, 
                    sep = " ")
                writeLines("", model.out.text.file)
                flush(model.out.text.file)
                if (normalise) {
                    temp <- Thetas[j, ] * (upper.ini - lower.ini) + 
                        lower.ini
                }
                else temp <- Thetas[j, ]
                temp.gof <- gof[j]
                if (is.finite(temp.gof)) {
                    suppressWarnings(writeLines(as.character(c(formatC(temp.gof, 
                        format = "E", digits = digits, flag = " "), 
                        formatC(temp, format = "E", digits = digits, 
                            flag = " "))), gof.text.file, sep = "  "))
                }
                else suppressWarnings(writeLines(as.character(c("NA", 
                    formatC(temp, format = "E", digits = digits, 
                        flag = " "))), gof.text.file, sep = "  "))
                writeLines("", gof.text.file)
                flush(gof.text.file)
            }
        }
        close(model.out.text.file)
        close(gof.text.file)
    }
    for (j in 1:N) {
        M.Zero <- gof[Theta.index[j]]
        for (i in 1:P) {
            M.New <- gof[Theta.index[j] + i]
            S[j, i] <- abs((M.New - M.Zero)/((M.New + M.Zero)/2)) * 
                (100/f)
        }
    }
    Ranking <- colMeans(S, na.rm = TRUE)
    if (verbose) 
        message("                             |                                ")
    if (verbose) 
        message("                             |                                ")
    if (verbose) 
        message("==============================================================")
    if (verbose) 
        message("[==================    LH-OAT finished !    =================]")
    if (verbose) 
        message("==============================================================")
    if (parallel != "none") {
        if ((parallel == "parallel") | (parallel == "parallelWin")) 
            stopCluster(cl)
        if (fn.name == "hydromod") {
            if (verbose) 
                message("                                         ")
            if (verbose) 
                message("[ Removing the 'parallel' directory ... ]")
            unlink(dirname(mc.dirs[1]), recursive = TRUE)
        }
    }
    if (write2disk) {
        InfoTXT.TextFile <- file(InfoTXT.fname, "a")
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        writeLines(c("Ending Time          :", date()), InfoTXT.TextFile, 
            sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        Time.Fin <- Sys.time()
        writeLines(c("Elapsed Time         :", format(round(Time.Fin - 
            Time.Ini, 2))), InfoTXT.TextFile, sep = " ")
        writeLines("", InfoTXT.TextFile)
        writeLines("================================================================================", 
            InfoTXT.TextFile)
        close(InfoTXT.TextFile)
    }
    Ranking <- sort(Ranking, decreasing = TRUE, na.last = TRUE)
    Ranking <- data.frame(RankingNmbr = format(as.character(1:P), 
        width = 11, justify = "left"), ParameterName = format(names(Ranking), 
        width = 13, justify = "left"), RelativeImportance = as.numeric(Ranking))
    Ranking[, "RankingNmbr"] <- as.character(Ranking[, "RankingNmbr"])
    Ranking[, "RelativeImportance.Norm"] <- Ranking[, "RelativeImportance"]/sum(Ranking[, 
        "RelativeImportance"], na.rm = TRUE)
    row.index <- which(Ranking[, "RelativeImportance"] == 0)
    ninsens <- length(row.index)
    if (ninsens > 0) 
        Ranking[row.index, "RankingNmbr"] <- format(as.character(rep(P, 
            ninsens)), width = 11, justify = "left")
    if (write2disk) {
        Ranking.fname <- paste(file.path(drty.out), "/", "LH_OAT-Ranking.txt", 
            sep = "")
        write.table(Ranking, file = Ranking.fname, row.names = FALSE, 
            quote = FALSE)
    }
    out <- vector("list", 2)
    if (normalise) {
        Xn <- Thetas * (UPPER.ini - LOWER.ini) + LOWER.ini
    }
    else Xn <- Thetas
    out[[1]] <- Xn
    out[[2]] <- Ranking
    names(out) <- c("ParameterSets", "Ranking")
    return(out)
}
