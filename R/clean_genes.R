#' Cleans a vector of genes
#'
#' \code{clean_genes} reads in the gene variable from the variant file
#' and removes any additional info between parantheses.
#'
#' @param gene_vec The atomic vector containing the annotated genes.
#' @return character A clean version of the genes vector.
#' @seealso \code{\link{gsub}}.
#' @examples
#' \dontrun{
#' clean_genes(vars$gene) # assumes you have a vars data frame
#' }
#' @export
clean_genes <- function(gene_vec) {
  # clean everything between parens
  gsub("\\([^)]*\\)", "", gene_vec)
}
