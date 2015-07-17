#' Tidy up the vars data frame
#'
#' \code{tidy_vars} optimises the vars data frame for printing i.e.
#' it shortens the various labels and separates thousands with comma etc.
#'
#' @param vars The data frame of vars.
#' @param samp_nm The sample name to match (basically a string to match the
#' column names).
#' @return The tidied vars data frame
#' @seealso \code{\link[dplyr]{mutate}}
#' @examples
#' \dontrun{
#' tidy_vars(vars, "CANVAS886.*_GT$") # assumes you have a vars data frame
#' }
#' @export
tidy_vars <- function(vars, samp_nm) {
  main_col_nm <- c("chrom", "start", "end", "ref", "alt",
                   "region", "gene", "change", "dbsnp",
                   "maf_kg", "maf_esp", "control.AC",
                   "pph2", "sift")
  samp_col <- grep(samp_nm, names(vars))
  stopifnot(var_names_ok(vars))

  vars <- vars %>%
    rename(
      chrom = CHROM, start = START, end = END, ref = REF, alt = ALT,
      maf_kg = aaf.1KG, maf_esp = esp6500_all, dbsnp = dbSNP135,
      pph2 = pph2_pred, sift = SIFT.Pred) %>%
    mutate(
      chrom = sub("chr", "", chrom),
      gene = clean_genes(gene),
      start = format_comma(start),
      end = format_comma(end),
      ref = shorten_refalt(ref),
      alt = shorten_refalt(alt),
      region = shorten_region(region),
      change = shorten_change(change),
      dbsnp = sub("rs.+", "YES", dbsnp),
      maf_kg = format_dec(maf_kg),
      maf_esp = format_dec(maf_esp),
      pph2 = shorten_pph2(pph2),
      sift = shorten_sift(sift))
  vars[c(samp_col, match(main_col_nm, names(vars)))]
}
