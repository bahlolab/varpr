#' Which variants are synonymous SNVs?
#'
#' \code{are_synsnv_vars} reads in the change column from the vars file and
#' outputs a logical vector indicating which rows/variants have a change equal to
#' \code{"synonymous SNV"}.
#'
#' @param vars The data frame of vars.
#' @return a logical vector with \code{length} equal to \code{nrow(vars)}.
#' @seealso \code{\link[base]{Logic}}.
#' @examples
#' \dontrun{
#' big_logical_vec <- are_synsnv_vars(vars)
#' }
#' @export
are_synsnv_vars <- function(vars = NULL) {
  stopifnot(!is.null(vars))
  vars[["change"]] %in% "synonymous SNV"
}
