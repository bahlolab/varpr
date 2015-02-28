#' Are the main var columns of the expected class?
#'
#' \code{var_cl_main_ok} checks that the main columns of a variant file
#' are as expected.
#'
#' @param vars The data frame of vars.
#' @return A logical vector of length one.
#' @seealso \code{\link[plyr]{mapvalues}}
#' @examples
#' \dontrun{
#' var_cl_main_ok(vars) # assumes you have a vars data frame
#' }
var_cl_main_ok <- function(vars) {
  stopifnot(is.data.frame(vars))
  stopifnot(var_names_ok(vars))
  exp_df <- data.frame(
    col_nm = c("CHROM", "START", "END", "REF", "ALT", "DP", "QUAL",
               "MQ", "region", "gene", "change", "annotation", "dbSNP135",
               "aaf.1KG", "esp6500_all",
               paste0("control.", c("AC", "AN", "AF", "hom", "het", "fams")),
               "pph2_pred", "SIFT.Pred"),
    col_cl = c("chr", "int", "int", "chr", "chr", "int", "num", "num",
               rep("chr", 5), "num", "num", "int", "int", "num",
               rep("int", 3), rep("chr", 2)))
  var_col_nm <- names(vars)
  # where are the main variables?
  ind <- var_col_nm %in% exp_df$col_nm
  var_cl <- sapply(vars[var_col_nm[ind]], class)
  var_cl <- plyr::mapvalues(var_cl,
                            from = c("character", "integer", "numeric"),
                            to = c("chr", "int", "num"))
  all(exp_df$col_cl == var_cl)
}


#' Are the sample var columns of the expected class?
#'
#' \code{var_cl_sample_ok} checks that the sample columns of a variant file
#' are as expected.
#'
#' @param vars The data frame of vars.
#' @return A logical vector of length one.
#' @examples
#' \dontrun{
#' var_cl_sample_ok(vars) # assumes you have a vars data frame
#' }
var_cl_sample_ok <- function(vars) {
  stopifnot(is.data.frame(vars))
  stopifnot(var_names_ok(vars))
  all_col_cl <- sapply(vars, class) # named vector
  samp_col_nm <- paste0("_", c("GT", "GQ", "DP", "DPR", "DPA"), "$")
  exp_samp_col_cl <- c("character", rep("integer", 3), "character")
  # find which column names contain the sample-specific strings
  samp_col_cl <- sapply(samp_col_nm, function(pat) {
    ind <- grepl(pat, names(all_col_cl))
    all_col_cl[ind]})
  joint_col_cl <- apply(samp_col_cl, 2, function(col) names(table(col)))
  if (all(joint_col_cl == exp_samp_col_cl)) return(TRUE)
  FALSE
}

#' Are the names of the main var columns as expected?
#'
#' \code{var_names_ok} checks that the names of the main columns of a variant
#' file are as expected.
#'
#' @param vars The data frame of vars.
#' @return A logical vector of length one.
#' @examples
#' \dontrun{
#' var_names_ok(vars) # assumes you have a vars data frame
#' }
var_names_ok <- function(vars){
  vars_nm <- names(vars)
  standard_nm <- c("CHROM", "START", "END",
                   "region", "change", "pph2_pred", "SIFT.Pred")
  nm_ok <- standard_nm %in% df_nm
  if (!all(nm_ok)) {
    message("The following names need changing: ",
            paste(standard_nm[!nm_ok], collapse = ", "))
    return(FALSE)
  }
  TRUE
}
