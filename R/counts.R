#' Counts the number of SNVs and INDELs.
#'
#' \code{vartype_count} reads in the REF and ALT variables from the variant file
#' and counts how many variant "rows" there are with a single base. This should
#' imply a SNV, although exceptions to this are quite possible (can't think of an
#' example right now).
#'
#' @param vars The vars file that contains the REF and ALT variables.
#' @return A data frame with the counts of SNVs, INDELs and Total.
#' @seealso \code{\link{vartype_count_per_chr}}.
#' @examples
#' \dontrun{
#' vartype_count(vars) # assumes you have a vars data frame
#' }
vartype_count <- function(vars) {
  stopifnot(is.data.frame(vars))
  varn <- names(vars)
  stopifnot(all(c("REF", "ALT") %in% varn))
  ref <- vars$REF
  alt <- vars$ALT
  stopifnot(is.character(ref), is.character(alt))
  lr <- length(ref)
  la <- length(alt)
  stopifnot(lr == la)
  snvs <- sum(grepl("^[ACGT]$", ref, ignore.case = TRUE) &
                grepl("^[ACGT]$", alt, ignore.case = TRUE))
  indels <- lr - snvs
  data.frame(
    VarType = c("SNVs", "INDELs", "Total"),
    Count = c(snvs, indels, (snvs + indels)))
}

#' Counts the number of SNVs and INDELs within each chromosome
#'
#' \code{vartype_count_per_chr} reads in the REF and ALT variables from the variant file
#' and counts how many variant "rows" there are with a single base. This should
#' imply a SNV, although exceptions to this are quite possible (can't think of an
#' example right now).
#'
#' @param vars The vars file that contains the REF and ALT variables.
#' @return A data frame with the counts of SNVs and INDELs.
#' @seealso \code{\link{vartype_count}}.
#' @examples
#' \dontrun{
#' vartype_count_per_chr(vars) # assumes you have a vars data frame
#' }
vartype_count_per_chr <- function(vars) {
  stopifnot(is.data.frame(vars))
  varn <- names(vars)
  stopifnot("CHROM" %in% varn)
  vartype_tab <- plyr::ddply(.data = vars, .variables = ~CHROM, .fun = function(x) {
    vartype_count(x)
  })
  out <- reshape2::dcast(data = vartype_tab, formula = CHROM ~ VarType, value.var = "Count")
  out <- out[gtools::mixedorder(out$CHROM),]
  rownames(out) <- NULL
  out
}


#' Returns the number of different genotypes per sample.
#'
#' \code{genotype_table_per_sample} reads in the genotype variables from the
#' variant file and returns the number of different genotypes in each of these
#' variables.
#'
#' @param vars The data frame of vars.
#' @param rowSums logical: should the sum of genotypes per sample (row) be output (default: TRUE)?
#' @return A data frame with the counts of each genotype. NA means the sample did not have the specific genotype.
#' @seealso \code{\link{table}}.
#' @examples
#' \dontrun{
#' genotype_table_per_sample(vars, rowSums = TRUE) # assumes you have a vars data frame
#' }
genotype_table_per_sample <- function(vars, rowSums = TRUE) {
  stopifnot(is.data.frame(vars))
  pat <- "_GT$"
  # which columns are GT columns?
  ind <- grepl(pat, names(vars))
  gt_col <- vars[ind]
  gt_list <- lapply(gt_col, table, useNA = "ifany")
  # Transform each table to a data.frame with n_gt columns and 1 row
  gt_list_to_df <- lapply(gt_list, function(tab) as.data.frame(t(as.matrix(tab))))
  gt_tab <- dplyr::bind_rows(gt_list_to_df)
  rownames(gt_tab) <- names(gt_list_to_df)

  if (rowSums) {
    return(cbind(gt_tab, tot = rowSums(gt_tab, na.rm = TRUE)))
  }
  gt_tab
}


#' Returns a summary of a MAF variable (i.e. 1KG or ESP6500)
#'
#' \code{maf_tab} reads in a MAF vector (i.e. 1KG or ESP6500) and outputs the
#' number of variants satisfying certain conditions.
#'
#' @param maf_vec The vector containing the MAF values.
#' @return An integer vector with the counts of each condition
#' @seealso \code{\link{sum}} and \code{\link{setNames}}.
#' @examples
#' \dontrun{
#' maf_tab(vars$aaf.1KG) # assumes you have a vars data frame
#' maf_tab(vars$esp6500_all) # assumes you have a vars data frame
#' }
maf_tab <- function(maf_vec) {
  stopifnot(is.atomic(maf_vec), is.numeric(maf_vec))
  novel <- sum(is.na(maf_vec))
  avail <- sum(!is.na(maf_vec))
  total <- sum(novel, avail)
  novel_pc5 <- sum(is.na(maf_vec) | (!is.na(maf_vec) & maf_vec <= 0.05))
  # variants have an alt. allele frequency less than 1, 5 and 10%??
  perc <- c(1, 5, 10)
  perc_vec <- setNames(vector("integer", length(perc)), paste0("pc", perc))
  perc_vec[] <- sapply(perc, function(pc) {
    sum(!is.na(maf_vec) & maf_vec <= (pc / 100))
  })
  # now join all together
  c(total = total, avail = avail, novel = novel,
    novelpc5 = novel_pc5, perc_vec)

}
