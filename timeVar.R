timeVariationMod <- 
function (mydata, pollutant = "nox", graph = c(hour,hour,month,weekday),
          local.tz = NULL, normalise = FALSE, 
          xlab = c("hour", "hour", "month", "weekday"), name.pol = pollutant, 
          type = "default", group = NULL, difference = FALSE, statistic = "mean", 
          conf.int = 0.95, B = 100, ci = TRUE, cols = "hue", ref.y = NULL, 
          key = NULL, key.columns = 1, start.day = 1, auto.text = TRUE, 
          alpha = 0.4, ...) 
{
  
  require(openair)
  require(reshape2)
  require(plyr)
  require(dplyr)
  variable = NULL
  if (length(cols) == 1 && cols == "greyscale") {
    trellis.par.set(list(strip.background = list(col = "white")))
  }
  current.strip <- trellis.par.get("strip.background")
  current.font <- trellis.par.get("fontsize")
  on.exit(trellis.par.set(strip.background = current.strip, 
                          fontsize = current.font))
  extra.args <- list(...)
  extra.args$ylab <- if ("ylab" %in% names(extra.args)) 
    quickText(extra.args$ylab, auto.text)
  else quickText(paste(pollutant, collapse = ", "), auto.text)
  extra.args$main <- if ("main" %in% names(extra.args)) 
    quickText(extra.args$main, auto.text)
  else quickText("", auto.text)
  if ("fontsize" %in% names(extra.args)) 
    trellis.par.set(fontsize = list(text = extra.args$fontsize))
  if (statistic == "median" && missing(conf.int)) 
    conf.int <- c(0.75, 0.95)
  if (statistic == "mean") {
    sub.text <- paste("mean and ", 100 * conf.int[1], "% confidence interval in mean", 
                      sep = "")
  }
  else {
    if (length(conf.int) == 1L) {
      sub.text <- paste("median and ", 100 * (1 - conf.int[1]), 
                        "/", 100 * conf.int[1], "th quantiles", sep = "")
    }
    else {
      sub.text <- paste("median, ", 100 * (1 - conf.int[1]), 
                        "/", 100 * conf.int[1], " and ", 100 * (1 - conf.int[2]), 
                        "/", 100 * conf.int[2], "th quantiles", sep = "")
    }
  }
  extra.args$sub <- if ("sub" %in% names(extra.args)) 
    quickText(extra.args$sub, auto.text)
  else quickText(sub.text, auto.text)
  extra.args$lwd <- if ("lwd" %in% names(extra.args)) 
    extra.args$lwd
  else 2
  ylim.handler <- if ("ylim" %in% names(extra.args)) 
    FALSE
  else TRUE
  ylimList <- FALSE
  if ("ylim" %in% names(extra.args)) {
    if (is.list(extra.args$ylim)) {
      if (length(extra.args$ylim) != 4) 
        stop("ylim should be a list of 4")
      ylim.list <- extra.args$ylim
      ylimList <- TRUE
    }
  }
  vars <- c("date", pollutant)
  if (!missing(group) & length(pollutant) > 1) {
    stop("Can only have one pollutant with a grouping variable, or several pollutants and no grouping variable.")
  }
  if (length(type) > 1) 
    stop("Can only have one type for timeVariation.")
  if (type %in% pollutant) 
    stop("Cannot have type the same as a pollutant name.")
  if (!missing(group)) {
    if (group %in% pollutant) 
      stop("Cannot have group the same as a pollutant name.")
  }
  if (difference) {
    if (missing(group)) {
      if (length(pollutant) != 2) 
        stop("Need to specify two pollutants to calculate their difference.")
    }
    if (!missing(group)) {
      if (length(unique(mydata[, group])) != 2) 
        stop("Need to specify two pollutants to calculate their difference.")
    }
  }
  if (!statistic %in% c("mean", "median")) {
    statistic <- "mean"
    warning("statistic should be 'mean' or 'median',  setting it to 'mean'.")
  }
  if (type == "variable") {
    mydata <- rename(mydata, c(variable = "tempVar"))
    type <- "tempVar"
  }
  if (!missing(group)) {
    if (group %in% dateTypes) {
      vars <- unique(c(vars, "date"))
    }
    else {
      vars <- unique(c(vars, group))
    }
  }
  mydata <- openair:::checkPrep(mydata, vars, type, remove.calm = FALSE)
  if (!missing(group)) 
    mydata <- cutData(mydata, group, local.tz = local.tz, 
                      ...)
  mydata <- cutData(mydata, type, local.tz = local.tz, ...)
  if (!is.null(local.tz)) 
    attr(mydata$date, "tzone") <- local.tz
  overall.main <- extra.args$main
  extra.args$main <- ""
  overall.sub <- extra.args$sub
  extra.args$sub <- ""
  poll.orig <- pollutant
  if (difference && missing(group)) 
    pollutant <- c(pollutant, paste(pollutant[2], "-", pollutant[1]))
  if (missing(name.pol)) 
    mylab <- sapply(seq_along(pollutant), function(x) quickText(pollutant[x], 
                                                                auto.text))
  if (!missing(name.pol)) 
    mylab <- sapply(seq_along(name.pol), function(x) quickText(name.pol[x], 
                                                               auto.text))
  if (missing(group)) {
    mydata <- melt(mydata, measure.vars = poll.orig)
    mydata$variable <- factor(mydata$variable)
  }
  else {
    id <- which(names(mydata) == poll.orig)
    names(mydata)[id] <- "value"
    id <- which(names(mydata) == group)
    names(mydata)[id] <- "variable"
    mydata$variable <- factor(mydata$variable)
    the.names <- levels(mydata[["variable"]])
    if (difference) 
      the.names <- c(the.names, paste(levels(mydata$variable)[2], 
                                      "-", levels(mydata$variable)[1]))
    mylab <- sapply(the.names, function(x) quickText(x, auto.text))
  }
  divide.by.mean <- function(x) {
    Mean <- mean(x$Mean, na.rm = TRUE)
    x$Mean <- x$Mean/Mean
    x$Lower <- x$Lower/Mean
    x$Upper <- x$Upper/Mean
    x
  }
  if (normalise) 
    extra.args$ylab <- "normalised level"
  days <- format(ISOdate(2000, 1, 2:8), "%A")
  days.abb <- format(ISOdate(2000, 1, 2:8), "%a")
  if (start.day < 0 || start.day > 6) 
    stop("start.day must be between 0 and 6.")
  if (start.day > 0) {
    day.ord <- c(days[(1 + start.day):7], days[1:(1 + start.day - 
                                                    1)])
    day.ord.abb <- c(days.abb[(1 + start.day):7], days.abb[1:(1 + 
                                                                start.day - 1)])
  }
  else {
    day.ord <- days
    day.ord.abb <- days.abb
  }
  mydata <- within(mydata, {
    wkday <- format(date, "%A")
    wkday <- ordered(wkday, levels = day.ord)
    hour <- as.numeric(format(date, "%H"))
    mnth <- as.numeric(format(date, "%m"))
  })
  rng <- function(x) {
    if (all(is.na(x[, c("Lower", "Upper")]))) {
      lims <- NULL
      return(lims)
    }
    if (ci) {
      lims <- range(c(x$Lower, x$Upper), na.rm = TRUE)
      inc <- 0.04 * abs(lims[2] - lims[1])
      lims <- c(lims[1] - inc, lims[2] + inc)
    }
    else {
      lims <- range(c(x$Mean, x$Mean), na.rm = TRUE)
      inc <- 0.04 * abs(lims[2] - lims[1])
      lims <- c(lims[1] - inc, lims[2] + inc)
      if (diff(lims) == 0) 
        lims <- NULL
    }
    lims
  }
  npol <- length(levels(mydata$variable))
  if (difference) {
    npol <- 3
    if (missing(group)) 
      poll1 <- pollutant[1]
    poll2 <- pollutant[2]
    if (!missing(group)) 
      poll1 <- levels(mydata$variable)[1]
    poll2 <- levels(mydata$variable)[2]
  }
  if (missing(key.columns)) 
    key.columns <- npol
  myColors <- openColours(cols, npol)
  if (!is.null(key)) {
    key <- list(rectangles = list(col = myColors[1:npol], 
                                  border = NA), title = "", text = list(lab = mylab), 
                space = "bottom", columns = key.columns, lines.title = 1)
    extra.args$main <- overall.main
  }
  if (difference) {
    data.hour <- errorDiff(mydata, vars = "hour", type = type, 
                           poll1 = poll1, poll2 = poll2, B = B, conf.int = conf.int)
  }
  else {
    data.hour <- plyr::ldply(conf.int, proc, mydata, vars = "hour", 
                             pollutant, type, B = B, statistic = statistic)
  }
  if (normalise) 
    data.hour <- group_by(data.hour, variable) %>% do(divide.by.mean(.))
  if (is.null(xlab[2]) | is.na(xlab[2])) 
    xlab[2] <- "hour"
  if (type != "default") {
    stripName <- sapply(levels(mydata[, type]), function(x) quickText(x, 
                                                                      auto.text))
    strip <- strip.custom(factor.levels = stripName)
  }
  else {
    strip <- FALSE
  }
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ hour | ", temp, sep = ""))
  if (ylim.handler) 
    extra.args$ylim <- rng(data.hour)
  if (ylimList) 
    extra.args$ylim <- ylim.list[[1]]
  xy.args <- list(x = myform, data = data.hour, groups = data.hour$variable, 
                  as.table = TRUE, xlab = xlab[2], xlim = c(0, 23), strip = strip, 
                  par.strip.text = list(cex = 0.8), key = key, scales = list(x = list(at = c(0, 
                                                                                             6, 12, 18, 23))), par.settings = simpleTheme(col = myColors), 
                  panel = function(x, y, ...) {
                    panel.grid(-1, 0)
                    panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                    panel.superpose(x, y, ..., panel.groups = function(x, 
                                                                       y, col.line, type, group.number, subscripts, 
                                                                       ...) {
                      if (difference) panel.abline(h = 0, lty = 5)
                      id <- which(data.hour$ci[subscripts] == data.hour$ci[1])
                      panel.xyplot(x[id], y[id], type = "l", col.line = myColors[group.number], 
                                   ...)
                      if (ci) {
                        mkpoly(data.hour[subscripts, ], x = "hour", 
                               y = "Mean", group.number, myColors, alpha)
                      }
                      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                    })
                  })
  xy.args <- listUpdate(xy.args, extra.args)
  hour <- do.call(xyplot, xy.args)
  if (difference) {
    data.weekday <- errorDiff(mydata, vars = "wkday", type = type, 
                              poll1 = poll1, poll2 = poll2, B = B, conf.int = conf.int)
  }
  else {
    data.weekday <- plyr::ldply(conf.int, proc, mydata, vars = "wkday", 
                                pollutant, type, B = B, statistic = statistic)
  }
  if (normalise) 
    data.weekday <- group_by(data.weekday, variable) %>% 
    do(divide.by.mean(.))
  data.weekday$wkday <- ordered(data.weekday$wkday, levels = day.ord)
  data.weekday$wkday <- as.numeric(as.factor(data.weekday$wkday))
  if (is.null(xlab[4]) | is.na(xlab[4])) 
    xlab[4] <- "weekday"
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ wkday | ", temp, sep = ""))
  if (ylim.handler) 
    extra.args$ylim <- rng(data.weekday)
  if (ylimList) 
    extra.args$ylim <- ylim.list[[2]]
  xy.args <- list(x = myform, data = data.weekday, groups = data.weekday$variable, 
                  as.table = TRUE, par.settings = simpleTheme(col = myColors, 
                                                              pch = 16), scales = list(x = list(at = 1:7, labels = day.ord.abb)), 
                  xlab = xlab[4], strip = strip, par.strip.text = list(cex = 0.8), 
                  key = key, panel = function(x, y, ...) {
                    panel.grid(-1, 0)
                    panel.abline(v = 1:7, col = "grey85")
                    panel.superpose(x, y, ..., panel.groups = function(x, 
                                                                       y, col.line, type, group.number, subscripts, 
                                                                       ...) {
                      if (difference) panel.abline(h = 0, lty = 5)
                      id <- which(data.weekday$ci[subscripts] == data.weekday$ci[1])
                      panel.xyplot(x[id], y[id], type = "l", col.line = myColors[group.number], 
                                   ...)
                      if (ci) {
                        mkrect(data.weekday[subscripts, ], x = "wkday", 
                               y = "Mean", group.number, myColors, alpha)
                      }
                      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                    })
                  })
  xy.args <- listUpdate(xy.args, extra.args)
  day <- do.call(xyplot, xy.args)
  if (difference) {
    data.month <- errorDiff(mydata, vars = "mnth", type = type, 
                            poll1 = poll1, poll2 = poll2, B = B, conf.int = conf.int)
  }
  else {
    data.month <- plyr::ldply(conf.int, proc, mydata, vars = "mnth", 
                              pollutant, type, B = B, statistic = statistic)
  }
  if (normalise) 
    data.month <- group_by(data.month, variable) %>% do(divide.by.mean(.))
  if (is.null(xlab[3]) | is.na(xlab[3])) 
    xlab[3] <- "month"
  temp <- paste(type, collapse = "+")
  myform <- formula(paste("Mean ~ mnth | ", temp, sep = ""))
  if (ylim.handler) 
    extra.args$ylim <- rng(data.month)
  if (ylimList) 
    extra.args$ylim <- ylim.list[[3]]
  xy.args <- list(x = myform, data = data.month, groups = data.month$variable, 
                  as.table = TRUE, xlab = xlab[3], xlim = c(0.5, 12.5), 
                  key = key, strip = strip, par.strip.text = list(cex = 0.8), 
                  par.settings = simpleTheme(col = myColors, pch = 16), 
                  scales = list(x = list(at = 1:12, labels = substr(format(seq(as.Date("2000-01-01"), 
                                                                               as.Date("2000-12-31"), "month"), "%B"), 1, 1))), 
                  panel = function(x, y, ...) {
                    panel.grid(-1, 0)
                    panel.abline(v = 1:12, col = "grey85")
                    panel.superpose(x, y, ..., panel.groups = function(x, 
                                                                       y, col.line, type, group.number, subscripts, 
                                                                       ...) {
                      if (difference) panel.abline(h = 0, lty = 5)
                      pltType <- "l"
                      if (length(subscripts) == 1) pltType <- "p"
                      id <- which(data.month$ci[subscripts] == data.month$ci[1])
                      panel.xyplot(x[id], y[id], type = pltType, col.line = myColors[group.number], 
                                   ...)
                      if (ci) {
                        mkrect(data.month[subscripts, ], x = "mnth", 
                               y = "Mean", group.number, myColors, alpha)
                      }
                      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                    })
                  })
  xy.args <- listUpdate(xy.args, extra.args)
  month <- do.call(xyplot, xy.args)
  if (difference) {
    data.day.hour <- errorDiff(mydata, vars = "day.hour", 
                               type = type, poll1 = poll1, poll2 = poll2, B = B, 
                               conf.int = conf.int)
  }
  else {
    data.day.hour <- plyr::ldply(conf.int, proc, mydata, 
                                 vars = "day.hour", pollutant, type, B = B, statistic = statistic)
  }
  if (normalise) 
    data.day.hour <- group_by(data.day.hour, variable) %>% 
    do(divide.by.mean(.))
  ids <- which(is.na(data.day.hour$Lower))
  data.day.hour$Lower[ids] <- data.day.hour$Mean[ids]
  ids <- which(is.na(data.day.hour$Upper))
  data.day.hour$Upper[ids] <- data.day.hour$Mean[ids]
  if (is.null(xlab[1]) | is.na(xlab[1])) 
    xlab[1] <- "hour"
  strip <- strip.custom(par.strip.text = list(cex = 0.8))
  if (type == "default") {
    strip.left <- FALSE
    layout <- c(length(unique(mydata$wkday)), 1)
  }
  else {
    stripName <- sapply(levels(mydata[, type]), function(x) quickText(x, 
                                                                      auto.text))
    strip.left <- strip.custom(factor.levels = stripName)
    layout <- NULL
  }
  temp <- paste(type, collapse = "+")
  if (type == "default") {
    myform <- formula("Mean ~ hour | wkday")
  }
  else {
    myform <- formula(paste("Mean ~ hour | wkday *", temp, 
                            sep = ""))
  }
  if (ylim.handler) 
    extra.args$ylim <- rng(data.day.hour)
  if (ylimList) 
    extra.args$ylim <- ylim.list[[4]]
  xy.args <- list(x = myform, data = data.day.hour, groups = data.day.hour$variable, 
                  as.table = TRUE, xlim = c(0, 23), xlab = xlab[1], layout = layout, 
                  par.settings = simpleTheme(col = myColors), scales = list(x = list(at = c(0, 
                                                                                            6, 12, 18, 23))), key = key, strip = strip, strip.left = strip.left, 
                  par.strip.text = list(cex = 0.8), panel = function(x, 
                                                                     y, ...) {
                    panel.grid(-1, 0)
                    panel.abline(v = c(0, 6, 12, 18, 23), col = "grey85")
                    panel.superpose(x, y, ..., panel.groups = function(x, 
                                                                       y, col.line, type, group.number, subscripts, 
                                                                       ...) {
                      if (difference) panel.abline(h = 0, lty = 5)
                      id <- which(data.day.hour$ci[subscripts] == data.day.hour$ci[1])
                      panel.xyplot(x[id], y[id], type = "l", col.line = myColors[group.number], 
                                   ...)
                      if (ci) {
                        mkpoly(data.day.hour[subscripts, ], x = "hour", 
                               y = "Mean", group.number, myColors, alpha)
                      }
                      if (!is.null(ref.y)) do.call(panel.abline, ref.y)
                    })
                  })
  xy.args <- listUpdate(xy.args, extra.args)
  day.hour <- do.call(xyplot, xy.args)
  subsets = c("day.hour", "hour", "day", "month")
  if (length(grep("atop", overall.main) == 1)) {
    y.upp <- 0.95
    y.dwn <- 0.05
  }
  else {
    y.upp <- 0.975
    y.dwn <- 0.025
  }
  main.plot <- function(...) {
    if (type == "default") {
      print(update(day.hour, key = list(rectangles = list(col = myColors[1:npol], 
                                                          border = NA), text = list(lab = mylab), space = "bottom", 
                                        columns = key.columns, title = "", lines.title = 1)), 
            position = c(0, 0.5, 1, y.upp), more = TRUE)
    }
    else {
      print(update(useOuterStrips(day.hour, strip = strip, 
                                  strip.left = strip.left), key = list(rectangles = list(col = myColors[1:npol], 
                                                                                         border = NA), text = list(lab = mylab), space = "bottom", 
                                                                       columns = key.columns, title = "", lines.title = 1)), 
            position = c(0, 0.5, 1, y.upp), more = TRUE)
    }
    print(hour, position = c(0, y.dwn, 0.33, 0.53), more = TRUE)
    print(month, position = c(0.33, y.dwn, 0.66, 0.53), more = TRUE)
    print(day, position = c(0.66, y.dwn, 1, 0.53))
    grid.text(overall.main, 0.5, y.upp, gp = gpar(fontsize = 14))
    grid.text(overall.sub, 0.5, y.dwn, gp = gpar(fontsize = 12))
  }
  ind.plot = function(x, ...) {
    plot(update(x, key = list(rectangles = list(col = myColors[1:npol], 
                                                border = NA), text = list(lab = mylab), space = "top", 
                              columns = key.columns)), ...)
  }
  main.plot()
  output <- list(plot = list(hour,  subsets = "hour"), 
                 #plot = list(day.hour, hour, day, month, subsets = subsets), 
                 data = list( data.hour, 
                             subsets = "hour"),
                 call = match.call(), 
                 main.plot = main.plot, 
                 ind.plot = ind.plot)
  names(output$data) <- "hour" #subsets
  names(output$plot)[1:4] <- "hour" #subsets
  class(output) <- "openair"
  invisible(output)
}
