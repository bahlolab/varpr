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
#' @export
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
