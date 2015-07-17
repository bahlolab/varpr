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
  # Turn the "NA" into "NotAv"
  gt_list_to_df[] <- lapply(gt_list_to_df,
                            function(df) {
                              names(df)[is.na(names(df))] <- "NotAv"
                              df })
  gt_tab <- dplyr::bind_rows(gt_list_to_df)
  rownames(gt_tab) <- names(gt_list_to_df)

  if (rowSums) {
    return(cbind(gt_tab, Tot = rowSums(gt_tab, na.rm = TRUE)))
  }
  gt_tab
}




#' Genotype table per family.
#'
#' \code{gt_tab_per_fam} reads in the genotype variables from the
#' variant file, groups them by individual family, and returns the number of
#' different genotypes in each of these groups.
#'
#' @param vars The data frame of vars.
#' @return A data frame with the families in rows and the different
#' genotypes in columns, and the counts of each genotype per family
#' in the cells.
#' @seealso \code{\link{table}}.
#' @examples
#' \dontrun{
#' gt_tab_per_fam(vars) # assumes you have a vars data frame
#' }
#' @export
gt_tab_per_fam <- function(vars) {
  ## which columns are GT columns
  gt_col <- vars[varpr::is_gt_col(vars)]
  # find unique genotypes
  gt <- unique(unlist(gt_col))
  # find families
  fam <- gsub("\\.[0-9]_GT$", "", names(gt_col))

  # Counts number of gt occurrences in col atomic vector
  count_gt <- function(gt, col) {
    vapply(X = gt, FUN = function(genotype) sum(col %in% genotype), FUN.VALUE = integer(1))
  }

  dat <- setNames(lapply(unique(fam), varpr::is_gt_col_for_fam, vars = vars),
                  unique(fam))
  dat[] <- lapply(dat, function(ind) vars[ind])

  apply2 <- function(df, gt) {
    out <- as.data.frame(t(df))
    lapply(out, count_gt, gt = gt)
  }

  dat[] <- lapply(dat, apply2, gt)

  list_of_tabs_to_df <- function(tab_list) {
    dat <- lapply(tab_list, function(tab) as.data.frame(t(as.matrix(tab))))
    return(dplyr::bind_rows(dat))
  }

  dat[] <- lapply(dat, list_of_tabs_to_df)

  change_na_nm <- function(df) {
    names(df)[is.na(names(df))] <- "NotAv"
    df
  }

  dat[] <- lapply(dat, change_na_nm)
  dat <- dplyr::tbl_df(plyr::ldply(dat, .id = "Family"))
  dat
}

