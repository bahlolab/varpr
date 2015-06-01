#' Which columns are genotype columns?
#'
#' \code{is_gt_col} uses \code{\link{grep}} to check which columns of the vars
#' data frame contain the string "_GT".
#'
#' @param vars The data frame of vars.
#' @param pat character: the pattern which matches a typical genotype column
#' (default: "_GT$").
#' @return an atomic vector of the indices of the columns that yielded a match.
#' @seealso \code{\link{grep}}.
#' @examples
#' \dontrun{
#' is_gt_col(vars) # assumes you have a vars data frame
#' }
#' @export
is_gt_col <- function(vars, pat = "_GT$") {
  stopifnot(is.data.frame(vars))
  out <- grep(pat, names(vars))
  if (length(out) == 0) warning("No pattern matches found")
  return(out)
}

#' Which columns are genotype columns for the specified family?
#'
#' \code{is_gt_col_for_fam} uses \code{\link{grep}} to check which columns of
#' the vars data frame contain the string "<fam>_GT".
#'
#' @param vars The data frame of vars.
#' @param fam character: the family name
#' @return an atomic vector of the indices of the vars columns that yielded a match.
#' @seealso \code{\link{grep}}.
#' @examples
#' \dontrun{
#' is_gt_col_for_fam(vars, "fam123") # assumes you have a vars data frame
#' }
#' @export
is_gt_col_for_fam <- function(vars, fam){
  stopifnot(is.data.frame(vars))
  out <- grep(paste0(fam, ".*_GT$"), names(vars))
  if (length(out) == 0) warning("No pattern matches found")
  return(out)
}
