#' Format numbers with commas for pretty printing
#'
#' \code{format_comma} outputs the given numeric vector with a comma thousands
#' separator for pretty printing.
#'
#' @param x The numeric vector.
#' @param trim logical; if FALSE, logical, numeric and complex values are
#' right-justified to a common width: if TRUE the leading blanks for
#' justification are suppressed (default: TRUE).
#' @return A character version of x where thousands are separated by comma.
#' @seealso \code{\link{format}}
#' @examples
#' \dontrun{
#' format_comma(vars$START)
#' format_comma(vars$END)
#' }
#' @export
format_comma <- function(x, trim = TRUE, ...) {
  format(x, big.mark = ",", trim = TRUE...)
}

#' Format floating point numbers with two decimals for pretty printing
#'
#' \code{format_dec} outputs the given numeric vector with two decimals
#' for pretty printing.
#'
#' @param x The numeric vector.
#' @return A character version of x where only two decimals are used.
#' @seealso \code{\link{format}}
#' @examples
#' \dontrun{
#' format_dec(vars$aaf.1KG)
#' format_dec(vars$esp6500_all)
#' }
#' @export
format_dec <- function(x, nsmall = 2, ...) {
  format(round(x, 2), nsmall = nsmall, ...)
}
