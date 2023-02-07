# ggplot.R
#
# Extensions to ggplot2.

#' StatExcf
#'
#' @format NULL
#' @usage NULL
#' @export
StatExcf <- ggplot2::ggproto(
  "StatExcf", ggplot2::Stat, compute_group =
    function(data, scales, n = NULL, pad = TRUE, tol = 1e-9) {
      # If n is NULL, use raw values; otherwise interpolate
      if (is.null(n)) {
        x <- unique(data$x)
        } else {
          x <- seq(min(data$x), max(data$x), length.out = n)
          }
      if (pad) {
        x <- c(min(x) * (1 - tol), x, max(x) * (1 + tol))
        }
      y <- excf(data$x)(x)
      data.frame(x = x, y = y)
      },

  default_aes = ggplot2::aes(y = ..y..),
  required_aes = c("x")
)


#' Compute the empirical exceedance distribution
#'
#' The empirical exceedance distribution function is the complement of the
#' empirical cumulative distribution function. It is commonly used to
#' characterize the distribution of hydrological data.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If \code{TRUE}, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{exceedance frequency corresponding x}
#' }
#' @export
stat_excf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatExcf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}

#' StatExcfPct
#'
#' @format NULL
#' @usage NULL
#' @export
StatExcfPct <- ggplot2::ggproto(
  "StatExcfPct", ggplot2::Stat, compute_group =
    function(data, scales, n = NULL, pad = TRUE, tol = 1e-9) {
      # If n is NULL, use raw values; otherwise interpolate
      if (is.null(n)) {
        x <- unique(data$x)
      } else {
        x <- seq(min(data$x), max(data$x), length.out = n)
      }
      if (pad) {
        x <- c(min(x) * (1 - tol), x, max(x) * (1 + tol))
      }
      y <- 100 * excf(data$x)(x)
      data.frame(x = x, y = y)
    },

  default_aes = ggplot2::aes(y = ..y..),
  required_aes = c("x")
)


#' Compute the empirical exceedance distribution on a percentage basis
#'
#' The empirical exceedance distribution function is the complement of the
#' empirical cumulative distribution function. It is commonly used to
#' characterize the distribution of hydrological data.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If \code{TRUE}, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{exceedance frequency corresponding x}
#' }
#' @export
stat_excf_pct <- function(mapping = NULL, data = NULL,
                          geom = "step", position = "identity",
                          ...,
                          n = NULL,
                          pad = TRUE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatExcfPct,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}
