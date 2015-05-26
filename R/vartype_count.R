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
#' @export
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
#' @importFrom dplyr "%>%"
#' @export
vartype_count_per_chr <- function(vars) {
  stopifnot(is.data.frame(vars))
  stopifnot("CHROM" %in% names(vars))
  vartype_tab <- vars %>%
    dplyr::group_by(CHROM) %>%
    dplyr::do(data.frame(vartype_count(.)))
  out <- reshape2::dcast(data = vartype_tab, formula = CHROM ~ VarType, value.var = "Count")
  out <- out[gtools::mixedorder(out$CHROM),]
  rownames(out) <- NULL
  out
}
