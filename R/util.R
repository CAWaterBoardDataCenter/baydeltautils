# util.R
#
# Miscellaneous utilities to support analysis of WEAP data.

#' Remove parenthetical arguments from the names of an object containing named
#' elements (e.g., a data frame).
#'
#' This arises most commonly when a data frame is created with a variable that
#' is the result of a function call.
#'
#' @param x object with names that need to be cleaned up
#' @export
names_rm_args <- function(x) {
  names(x) <- gsub("\\(.*\\)", "", names(x))
  return(x)
}

#' Collapse a sequence of arguments into a file path.
#'
#' This is purely a convenience utility, and is equivalent to
#' \code{paste(..., sep="/")}.
#'
#' @param ... two or more strings to be collapsed into a file path
#' @export
path_collapse <- function(...) {
  return(paste(..., sep="/"))
}
