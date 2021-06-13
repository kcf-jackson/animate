#' A utility function for generating IDs
#'
#' @param x The data that require IDs.
#' @param prefix A character string; the prefix to be added to each ID.
#' @param sep A character string; the separator to be added between the prefix
#' and an ID.
#'
#' @export
new_id <- function(x, prefix = "ID", sep = "-") {
    paste(prefix, seq_along(x), sep = sep)
}
