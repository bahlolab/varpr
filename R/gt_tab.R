#' Genotype table per sample.
#'
#' \code{gt_tab_per_sample} reads in the genotype variables from the
#' variant file and returns the number of different genotypes in each of these
#' variables.
#'
#' @param vars The data frame of vars.
#' @param rowSums logical: should the sum of genotypes per
#' sample (row) be output (default: TRUE)?
#' @return A data frame with the samples in rows and the different
#' genotypes in columns, and the counts of each genotype per sample
#' in the cells. A value of NA means that the sample did not have
#' the specific genotype.
#' @seealso \code{\link{table}}.
#' @examples
#' \dontrun{
#' gt_tab_per_sample(vars, rowSums = TRUE) # assumes you have a vars data frame
#' }
#' @export
gt_tab_per_sample <- function(vars, rowSums = TRUE) {
  stopifnot(is.data.frame(vars))
  # which columns are GT columns?
  ind <- is_gt_col(vars)
  if (sum(ind) == 1L) {
    warning("Only one genotype column (sample?) in dataset!")
  }
  gt_col <- vars[ind]
  gt_list <- lapply(gt_col, table, useNA = "ifany")
  # Transform each table to a data.frame with n_gt columns and 1 row
  gt_list_to_df <- lapply(gt_list, function(tab) as.data.frame(t(as.matrix(tab))))
  gt_tab <- dplyr::bind_rows(gt_list_to_df)
  rownames(gt_tab) <- names(gt_list_to_df)

  if (rowSums) {
    return(cbind(gt_tab, Tot = rowSums(gt_tab, na.rm = TRUE)))
  }
  gt_tab
}

gt_tab_per_fam <- function(vars) {

}

#' Which columns are GT columns?
#'
#' \code{is_gt_col} uses \code{\link{grepl}} to check whether each column
#' name contains the string "_GT" or not.
#'
#' @param vars The data frame of vars.
#' @param pat character: the pattern which matches a typical genotype column
#' (default: "_GT$").
#' @param pat logical: should the sum of genotypes per sample (row) be output
#' (default: TRUE)?
#' @return a logical vector where the ith TRUE element means that the ith column
#' is a genotype column.
#' @seealso \code{\link{grepl}}.
#' @examples
#' \dontrun{
#' is_gt_col(vars) # assumes you have a vars data frame
#' }
#' @export
is_gt_col <- function(vars, pat = "_GT$") {
  return(grepl(pat, names(vars)))
}

#' Which columns are genotype columns for specified family?
#'
#' \code{is_gt_col_for_fam} uses \code{\link{grepl}} to check whether each column
#' name contains the string "<fam>_GT" or not.
#'
#' @param vars The data frame of vars.
#' @param fam character: the family name
#' @return a logical vector where the ith TRUE element means that the ith column
#' is a genotype column belonging to family <fam>.
#' @seealso \code{\link{grepl}}.
#' @examples
#' \dontrun{
#' is_gt_col_for_fam(vars, "fam123") # assumes you have a vars data frame
#' }
#' @export
is_gt_col_for_fam <- function(vars, fam){
  stopifnot(is.data.frame(vars))
  grepl(paste0(fam, ".*_GT$"), names(vars))
}
