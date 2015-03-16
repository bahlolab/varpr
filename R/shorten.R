#' Shorten region column from variant file
#'
#' \code{shorten_region} shortens the region column from the variant file in
#' order to print results in a more efficient manner.
#'
#' @param region_vec The character vector containing region values.
#' @return A character vector with shortened region values.
#' @seealso \code{\link[plyr]{mapvalues}}
#' @examples
#' \dontrun{
#' tmp <- shorten_region(vars$region)
#' table(tmp, useNA = "ifany")
#' }
#' @export
shorten_region <- function(region_vec) {
  old_reg <- c("downstream", "exonic", "exonic;splicing", "intergenic",
               "intronic", "ncRNA_exonic", "ncRNA_intronic", "ncRNA_splicing",
               "ncRNA_UTR3", "ncRNA_UTR5", "ncRNA_UTR5;ncRNA_UTR3",
               "splicing", "upstream", "upstream;downstream",
               "UTR3", "UTR5", "UTR5;UTR3")
  new_reg <- c("down", "exonic", "exo;sp",  "interg",
               "intron",  "nRNAe", "nRNAi", "nRNAs",
               "nRNA3", "nRNA5", "nRNA53",
               "splice", "up", "up;down",
               "UTR3", "UTR5", "UTR53")
  plyr::mapvalues(region_vec, old_reg, new_reg)
}

#' Shorten change column from variant file
#'
#' \code{shorten_change} shortens the change column from the variant file in
#' order to print results in a more efficient manner.
#'
#' @param change_vec The character vector containing change values.
#' @return A character vector with shortened change values.
#' @seealso \code{\link[plyr]{mapvalues}}
#' @examples
#' \dontrun{
#' tmp <- shorten_region(vars$change)
#' table(tmp, useNA = "ifany")
#' }
#' @export
shorten_change <- function(change_vec) {
  old_ch <- c("frameshift deletion", "frameshift insertion",
              "nonframeshift deletion", "nonframeshift insertion",
              "nonframeshift substitution", "nonsynonymous SNV",
              "stopgain", "stoploss", "synonymous SNV", "unknown")
  new_ch <- c("f_del", "f_ins",
              "nf_del", "nf_ins",
              "nf_sub", "ns_SNV",
              "sg_SNV", "sl_SNV", "sy_SNV", "unkn")
  plyr::mapvalues(change_vec, old_ch, new_ch)
}

#' Shorten PPH2 column from variant file
#'
#' \code{shorten_pph2} shortens the PPH2 column from the variant file in
#' order to print results in a more efficient manner.
#'
#' @param pph2_vec The character vector containing pph2 values.
#' @return A character vector with shortened pph2 values.
#' @seealso \code{\link[plyr]{mapvalues}}
#' @examples
#' \dontrun{
#' tmp <- shorten_pph2(vars$pph2_pred)
#' table(tmp, useNA = "ifany")
#' }
#' @export
shorten_pph2 <- function(pph2_vec) {
  old_pph2 <- c("benign", "possibly damaging", "probably damaging", "unknown")
  new_pph2 <- c("ben", "dm_po", "dm_pr", "unkn")
  plyr::mapvalues(pph2_vec, old_pph2, new_pph2)
}

#' Shorten SIFT column from variant file
#'
#' \code{shorten_sift} shortens the SIFT column from the variant file in
#' order to print results in a more efficient manner.
#'
#' @param sift_vec The character vector containing sift values.
#' @return A character vector with shortened sift values.
#' @seealso \code{\link[plyr]{mapvalues}}
#' @examples
#' \dontrun{
#' tmp <- shorten_sift(vars$SIFT.pred)
#' table(tmp, useNA = "ifany")
#' }
#' @export
shorten_sift <- function(sift_vec) {
  old_sift <- c("DAMAGING", "DAMAGING *Warning! Low confidence.",
                "Damaging due to stop", "Not scored", "TOLERATED")
  new_sift <- c("dam", "dam_lc", "dam_st", "not_sc", "tol")
  plyr::mapvalues(sift_vec, old_sift, new_sift)
}
