#' Which variants are within exonic, splicing or exonic/splicing regions?
#'
#' \code{are_exosplice_vars} reads in the region column from the vars file and
#' outputs a logical vector indicating which rows/variants have a region equal to
#' \code{"exonic"}, \code{"splicing"} or \code{"exonic;splicing"}.
#'
#' @param vars The data frame of vars.
#' @return a logical vector with \code{length} equal to \code{nrow(vars)}.
#' @seealso \code{\link[base]{Logic}}.
#' @examples
#' \dontrun{
#' big_logical_vec <- are_exosplice_vars(vars)
#' }
#' @export
are_exosplice_vars <- function(vars = NULL) {
  stopifnot(!is.null(vars))
  vars[["region"]] %in% c("splicing", "exonic;splicing", "exonic")
}

