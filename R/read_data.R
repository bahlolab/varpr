#' Read in a variant file.
#'
#' \code{read_var_file} reads in a variant file output by the MPS pipeline i.e.
#' merged_collab.txt. Note that it can be a subset of the original (raw) file;
#' the main thing is that its column headers should not be processed beforehand.
#' It is basically a wrapper around \code{read.delim}, but the \code{na.strings}
#' is an 'empirical' parameter that specifies strings used throughout the file to
#' denote NAs.
#'
#' @param fname The full file path to the variant file.
#' @param ... Additional arguments to be passed to \code{\link{read.delim}}.
#' @return The variant file as a data frame with \code{dplyr}'s tbl_df class.
#' @section Warning: This function takes more than 10mins to read in a data frame
#'   with > 7 million rows and 150 variables. \code{data.table}'s \code{fread}
#'   function is considerably faster, but I've found that it automatically
#'   converts column classes that were supposed to be numeric to character. I
#'   haven't figured out why it's doing this yet.
#' @seealso \code{\link{read.table}}, \code{\link[dplyr]{tbl_df}} for more
#'   details.
#' @examples
#' \dontrun{
#' full_path <- "~/Desktop/Fam1_merged_collab.txt"
#' DF <- read_var_file(file.path(full_path))
#' }
#' @export
read_var_file <- function(fname, ...){
  dplyr::tbl_df(read.delim(file = fname,
                    stringsAsFactors = FALSE,
                    na.strings=c("NA", "N/A", "", "."), ...))
}

#' Read in a variant file stored as an rds object.
#'
#' \code{read_var_obj} reads in a variant file if it has already
#' been stored as a single R object. It is basically a wrapper around
#' \code{readRDS}, but it also wraps the object around \code{dplyr}'s tbl_df
#' class.
#'
#' @param fname The full file path to the variant object.
#' @return The variant file as a data frame with \code{dplyr}'s tbl_df
#'   class.
#' @seealso \code{\link{readRDS}}, \code{\link[dplyr]{tbl_df}} for more
#'   details.
#' @examples
#' \dontrun{
#' full_path <- "~/Desktop/Fam1_merged_collab.rds"
#' DF <- read_var_obj(file.path(full_path))
#' }
#' @export
read_var_obj <- function(fname){
  dplyr::tbl_df(readRDS(fname))
  # just a comment
}
