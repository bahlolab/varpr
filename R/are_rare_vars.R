#' Which variants are rare in the genomic databases?
#'
#' \code{are_rare_vars} reads in the specified genomic database columns from the
#' vars file and outputs a logical vector indicating which rows/variants have a
#' MAF less than or equal to the specified maf, or are novel.
#'
#' @param vars The data frame of vars.
#' @param gendb The genomic database to choose (default: both 1KG and ESP6500).
#' @return a logical vector with \code{length} equal to \code{nrow(vars)}.
#' @seealso \code{\link[base]{Logic}}.
#' @examples
#' \dontrun{
#' big_logical_vec <- are_rare_vars(vars, gendb = "both_kg_esp", maf = 0.05)
#' }
#' @export
are_rare_vars <- function(vars = NULL, gendb = c("both_kg_esp", "kg", "esp"), maf = 0.05) {
  stopifnot(!is.null(vars))
  gendb <- match.arg(gendb, several.ok = FALSE)
  print(gendb)
  pat <- switch(gendb,
                both_kg_esp = "aaf\\.1KG|esp6500_all",
                kg          = "aaf\\.1KG",
                esp         = "esp6500_all")
  db_col_ind <- grep(pat, names(vars))
  db_col <- vars[db_col_ind]
  db_col_list <- lapply(db_col, is_rare_in, maf)
  Reduce("&", db_col_list)

}

#' Check if this atomic vector is NA or less than a specified threshold.
#'
#' \code{is_rare_in} outputs a logical vector indicating which elements have a
#' value less than or equal to the specified threshold, or are unavailable.
#'
#' @param vec A numeric atomic vector.
#' @param thr The threshold.
#' @return a logical vector with \code{length} equal to \code{length(vec)}.
#' @seealso \code{\link{base::Logic}}.
#' \dontrun{
#' big_logical_vec <- is_rare_in(vec = vars$aaf.1KG, thr = 0.05)
#' }
is_rare_in <- function(vec, thr) {
  stopifnot(is.numeric(vec), is.atomic(vec), is.numeric(thr), length(thr) == 1L)
  (is.na(vec)) | (!is.na(vec) & vec <= thr)
}
