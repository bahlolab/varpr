#' Is the specific variant/row located within the provided linkage region?
#'
#' \code{are_in_lr} indicates which variants from the variant file lie within the
#' provided linkage region.
#'
#' @param chr The linkage region chromosome. Takes values from 1 to 22
#'   (autosomal) and X, Y, M (sex and mitochondrial).
#' @param startbp,endbp The start and end base pair coordinates.
#' @param vars The data frame of vars.
#' @return A logical vector of length equal to nrow(vars), indicating if the row is
#'   within any of the linkage regions.
#' @seealso \code{\link{Logic}}.
#' @seealso \code{\link{base::Logic}}.
#' @examples
#' \dontrun{
#' # assumes you have a vars data frame
#' big_logical_vec <- are_in_lr(chr = 4, startbp = 10000, endbp = 50000, vars)
#' }
#' @export
are_in_lr <- function(chr = NULL, startbp = NULL, endbp = NULL, vars = NULL) {
  fun_args <- c(as.list(environment()))
  stopifnot(any(!sapply(fun_args, is.null)))
  stopifnot(is.data.frame(vars))
  stopifnot(is.numeric(startbp),
            is.numeric(endbp))
  stopifnot(length(chr) == 1,
            length(startbp) == 1,
            length(endbp) == 1)
  if (startbp < 1 || endbp < 1) stop("Coordinates smaller than 1!")
  stopifnot(endbp > 1)
  stopifnot(chr %in% c(1:22, "X", "Y", "M"))
  (vars$CHROM %in% paste0("chr", chr)) &
    (vars$START >= startbp) &
    (vars$END <= endbp)
}
