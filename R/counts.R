#' Counts the number of SNVs and INDELs.
#'
#' \code{vartype_count} reads in the REF and ALT variables from the variant file
#' and counts how many variants "rows" there are with a single base. This should
#' imply a SNV, although exceptions to this are quite possible (can't think of an
#' example right now).
#'
#' @param ref,alt The REF and ALT variables from the variant file.
#' @return A data frame with the counts of SNVs and INDELs.
#' @seealso \code{\link{grep}}
#' @examples
#' \dontrun{
#' vartype_count(vars$REF, vars$ALT) # assumes you have a vars data frame
#' }

vartype_count <- function(ref, alt) {
  stopifnot(is.character(ref), is.character(alt))
  lr <- length(ref)
  la <- length(alt)
  stopifnot(lr == la)
  snvs <- sum(grepl("^[ACGT]$", ref, ignore.case = TRUE) &
                grepl("^[ACGT]$", alt, ignore.case = TRUE))
  indels <- lr - snvs
  data.frame(
    VarType = c("SNVs", "INDELs"),
    Count = c(snvs, indels))
}
