#' Reads in the raw merged collab file.
#'
#' \code{read_collab_file} reads in the merged_collab.txt file output by the MPS
#' pipeline. It is basically a wrapper around \code{read.delim}, but the
#' \code{na.strings} parameter is used to specify strings used throughout the
#' file to denote NAs.
#'
#' @param fname The full file path to the merged_collab.txt file.
#' @return The merged_collab.txt file as a data frame with \code{dplyr}'s tbl_df
#'   class.
#' @seealso \code{\link{read.table}}, \code{\link[dplyr]{tbl_df}} for more
#'   details.
#' @examples
#' \dontrun{
#' full_path <- "~/Desktop/Fam1_merged_collab.txt"
#' DF <- read_collab_file(file.path(full_path))
#' }
read_collab_file <- function(fname, ...){
  tbl_df(read.delim(file = fname,
                    stringsAsFactors = FALSE,
                    na.strings=c("NA", "N/A", "", "."), ...))
}

#' Reads in the raw merged collab file if stored as an R object (i.e. rds).
#'
#' \code{read_collab_obj} reads in the merged_collab.txt file if it has already
#' been stored as a single R object. It is basically a wrapper around
#' \code{readRDS}, but it also wraps the object around \code{dplyr}'s tbl_df
#' class.
#'
#' @param fname The full file path to the merged_collab.rds object.
#' @return The merged_collab.txt file as a data frame with \code{dplyr}'s tbl_df
#'   class.
#' @seealso \code{\link{readRDS}}, \code{\link[dplyr]{tbl_df}} for more
#'   details.
#' @examples
#' \dontrun{
#' full_path <- "~/Desktop/Fam1_merged_collab.rds"
#' DF <- read_collab_obj(file.path(full_path))
#' }
read_collab_obj <- function(fname, ...){
  tbl_df(readRDS(fname))
}
