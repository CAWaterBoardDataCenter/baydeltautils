# plotting.R
#
# Plotting functions.

#' Draw a paired boxplot
#'
#' @param formula formula of form y~x to be applied to both data frames
#' @param df1 data frame with values to be plotted at left
#' @param df2 data frame with values to be plotted at right
#' @param border vector of colors for borders of boxes
#' @param width relative width of plots
#' @param sepFactor relative separation between boxes in a pair [describe better]
#' @param ... additional paramaters to be passed to boxplot
#' @export
boxplotPaired <- function(formula, df1, df2, border=c("black", "blue"),
                          width=0.25, sepFactor=1.5, ...) {
  # make an invisible plot of the full set of data; the "[,all.vars(formula)]"
  # index ensures that the data frames can be concatenated, by (1) selecting only
  # the columns of interest, and (2) ordering them in the same way
  bxp <- graphics::boxplot(formula, droplevels(rbind(df1[,all.vars(formula)],
                                           df2[,all.vars(formula)])),
                 border="white", ...)
  xIdx <- seq_along(bxp$names)
  graphics::boxplot(formula, droplevels(df1), border=border[1], boxwex=width,
          at=xIdx-width/sepFactor, add=T, axes=F)
  graphics::boxplot(formula, droplevels(df2), border=border[2], boxwex=width,
          at=xIdx+width/sepFactor, add=T, axes=F)
}


#' Add a second boxplot to an existing plot, formatted to be distinguishable.
#'
#' @param ... parameters to be passed to boxplot
#' @param border color for border of new boxes
#' @param boxwex relative width of new boxes
#' @export
boxplotAdd <- function(..., border="blue", boxwex=0.5) {
  graphics::boxplot(..., outline=F, border=border, boxwex=boxwex, add=T, axes=F)
}

#' Plot an exceedance curve.
#'
#' Plot an exceedance curve with plotting positions determined by the empirical
#' cumulative distribution function.
#'
#' Note: should consider modifying to use Weibull plotting positions or
#' providing more choices.
#'
#' @param x the data series
#' @param add if TRUE, add plot to existing axes
#' @param pch plotting character
#' @param col color
#' @param xlab xlabel text
#' @param ylab ylabel text
#' @param ylim y-axis limits
#' @param xlim x-axis limits
#' @param bg background color
#' @param type one of \code{p} (points), \code{l} (line), \code{b} (both), or
#' \code{n} (none)
#' @param jitter if TRUE, jitter points randomly to prevent overplotting
#' @param jitterTol relative magnitude for jittering points
#' @param plot.pos one of \code{left} (maximum plotted at 0), \code{right}
#' (minimum plotted at 1), or \code{center} (positions centered between extremes
#' described above); the distance between plotting positions is always
#' \code{1/n}, provided that the values in \code{x} are distinct (or points are
#' jittered)
#' @param ... additional parameters passed to \code{plot}
#' @export
plotExceedance <- function(x, add=F, pch=19, col="black",
                           xlab="Exceedance Frequency",
                           ylab="x", ylim=NULL, xlim=c(0,1),
                           bg=NA, type="p", jitter=FALSE,
                           jitterTol=1e-9, plot.pos="center", ...) {
  # The user may want to jitter points if, for example, the same value
  # appears repeatedly in the data. If so, we introduce a small magnitude
  # random variation to the data. This prevents points from being plotted
  # on top of each other in the exceedance curve, but ensures that the
  # data are not altered in a meaningful way.
  if (jitter) {
    jitterMag <- min(jitterTol, jitterTol*min(x))
    x <- x + jitterMag*stats::runif(length(x), -1, 1)
  }
  F.x <- stats::ecdf(x)
  # Tweak plotting positions to reflect user preference
  dx <- 0 # plot.pos == "left"
  if (plot.pos=="center") {
    dx <- min(F.x(x))/2
  }
  else if (plot.pos=="right") {
    dx <- min(F.x(x))
  }
  if (!add) {
    if (type=="n") {
      graphics::plot(1-F.x(x)+dx, x, type="n", xlab=xlab, ylab=ylab, ylim=ylim, xlim=xlim, ...)
    }
    if (type=="p" | type=="b") {
      graphics::plot(1-F.x(x)+dx, x, pch=pch, col=col, xlab=xlab, ylab=ylab, xlim=xlim,
           ylim=ylim, bg=bg, ...)
    }
    if (type=="l") {
      graphics::plot(1-F.x(x[order(x)])+dx, x[order(x)], type="l", col=col, xlab=xlab,
           ylab=ylab, xlim=xlim, ylim=ylim, bg=bg, ...)
    }
    if (type=="b") {
      graphics::lines(1-F.x(x[order(x)])+dx, x[order(x)], col=col, ...)
    }
  }
  else {
    if (type=="p" | type=="b") {
      graphics::points(1-F.x(x)+dx, x, pch=pch, col=col, bg=bg, ...)
    }
    if (type=="l" | type=="b") {
      graphics::lines(1-F.x(x[order(x)])+dx, x[order(x)], col=col, ...)
    }
  }
}

#' Plot exceedance curves for each series of values in a list.
#'
#' @param l.x list of data series
#' @param v.col vector of colors for plotting series; in NULL, a sensible
#' default of black and blue, or black and a rainbow gradient will be used
#' @param ylim y-axis limits
#' @param main main title for figure
#' @param ylab y-axis label text
#' @param xlab x-axis label text
#' @param legend vector of legend entries
#' @param legend.cex text scaling factor for legend
#' @param type one of \code{p} (points), \code{l} (line), \code{b} (both), or
#' \code{n} (none)
#' @param ... additional parameters passed to \code{plotExceedance}
#' @export
plotExceedanceComparison <- function(l.x, v.col=NULL, ylim=NULL,
                                     main=NULL, ylab=NULL,
                                     xlab="Exceedance Frequency",
                                     legend=NULL, legend.cex=1, type="p", ...) {
  # create a sensible default v.col if none is provided
  if (is.null(v.col)) {
    if (length(l.x) == 2) { v.col <- c("black", "blue") }
    else { v.col <- c("black", grDevices::rainbow(length(l.x)-1)) }
  }

  # create an empty plot that encompasses the full range of the data
  plotExceedance(Reduce(c, l.x), type="n", main=main, xlab=xlab, ylab=ylab, ylim=ylim)
  for (i in seq_along(l.x)) {
    plotExceedance(l.x[[i]], add=T, col=v.col[i], jitter=T, type=type, ...)
  }
  if (!is.null(legend)) {
    if (type=="l") {
      legend("topright", col=v.col, legend=legend, cex=legend.cex, ...)
    }
    legend("topright", col=v.col, legend=legend, cex=legend.cex, ...)
  }
}

#' Plot multiple time series of the same pair of variables from a list of data
#' frames with conforming entries.
#'
#' Note: there is no obvious reason for this function to be used only for time
#' series since it is really just an xy scatter plot. Consider renaming to be
#' more descriptive.
#'
#' @param formula plotting formula of form y~x
#' @param l.df list of data frames containing data to be plotted
#' @param v.col vector of colors for plotting series; in NULL, a sensible
#' default of black and blue, or black and a rainbow gradient will be used
#' @param main main title of plot
#' @param ylab y-axis label
#' @param xlab x-axis label
#' @param ... additional parameters passed to plot
#' @export
plotTSComparison <- function(formula, l.df, v.col=NULL,
                             main=NULL, ylab=NULL,
                             xlab="Date", ...) {
  # create a sensible default v.col if none is provided
  if (is.null(v.col)) {
    if (length(l.df) == 2) { v.col <- c("black", "blue") }
    else { v.col <- c("black", grDevices::rainbow(length(l.df)-1)) }
  }

  # create an empty plot that encompasses the full range of the data
  dummy.x <- Reduce(range, lapply(l.df, "[[", as.character(formula.tools::rhs(formula))))
  dummy.y <- Reduce(range, lapply(l.df, "[[", as.character(formula.tools::lhs(formula))))
  graphics::plot(dummy.x, dummy.y, type="n", main=main, xlab=xlab, ylab=ylab)
  for (i in seq_along(l.df)) {
    graphics::lines(formula, l.df[[i]], col=v.col[i], ...)
  }
}

#' Plot multiple time series from a single data frame on a single set of axes.
#'
#' Similar to plotTsComparison, but rather than using a formula, plot implied
#' (or specified) y-values as a function of an x-value in a data frame. The
#' default behavior is to plot columns 2:ncol as a function of column 1. If
#' "xcol" is specified as an integer in 1:ncol, then the y-values will be taken
#' to be the remaining values, or a vector of integers specified as "ycol".
#'
#' Note: there is no obvious reason for this function to be used only for time
#' series since it is really just an xy scatter plot. Consider renaming to be
#' more descriptive.
#'
#' @param df data frame
#' @param xcol column containing x-values
#' @param ycol column containing y-values
#' @param v.col vector of colors for plotting series; in NULL, a sensible
#' default of black and blue, or black and a rainbow gradient will be used
#' @param main main title of plot
#' @param ylab y-axis label
#' @param xlab x-axis label
#' @param ... additional parameters passed to plot
#' @export
plotTSCols <- function(df, xcol=1, ycol=NULL, v.col=NULL,
                       main=NULL, ylab=NULL, xlab="Date", ...) {
  # create a sensible default v.col if none is provided
  if (is.null(v.col)) {
    if (length(df)-1 == 3) { v.col <- c("black", "blue") }
    else { v.col <- c("black", grDevices::rainbow(length(df)-2)) }
  }

  if (is.null(ycol)) {
    ycol <- setdiff(1:ncol(df), xcol)
  }

  # create an empty plot that encompasses the full range of the data
  dummy.x <- range(df[,xcol])
  dummy.y <- range(df[,ycol])
  graphics::plot(dummy.x, dummy.y, type="n", main=main, xlab=xlab, ylab=ylab)
  for (i in seq_along(ycol)) {
    graphics::lines(df[,xcol], df[,ycol[i]], col=v.col[i], ...)
  }
}

