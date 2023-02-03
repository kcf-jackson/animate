#' A utility function for generating IDs
#'
#' @param x The data that require IDs.
#' @param prefix A character string; the prefix to be added to each ID.
#' @param sep A character string; the separator to be added between the prefix
#' and an ID.
#'
#' @examples
#' new_id(x = runif(10), prefix = "points")
#'
#' @export
new_id <- function(x, prefix = "ID", sep = "-") {
    paste(prefix, seq_along(x), sep = sep)
}


#' Launch the 'FFmpeg'-based video editor ('Shiny' app)
#'
#' @note This requires `ffmpeg` to work. The 'ffmpeg' binary can be downloaded from
#' \url{https://ffmpeg.org/download.html}.
ffmpeg <- function() {
  shiny::runApp(ffmpeg_shiny(), launch.browser = TRUE)
}
