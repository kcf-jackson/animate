#' 'FFmpeg' command builder
#'
#' @param input A character string; the path to the input video file.
#' @param output A character string; the path to the output video file.
#' @param ... Parameters to be passed to the `ffmpeg` command-line interface.
#
# @examples
# ffmpeg_command("abc.mp4", "edited.mp4", skip = 5, time = 10,
#                crop = "1000:500:0:0", speed = "2.0", codec = "h264")
# ffmpeg_command("abc.mp4", "abc.gif", frame_rate = 20, loop = 0, palette = TRUE) |> print()
#
#' @return A character string; the 'FFmpeg' command.
ffmpeg_command <- function(input, output, ...) {
  param <- list(...)
  FFMPEG(
    Skip(param$skip),
    Time(param$time),
    Input(input),
    Codec(param$codec),
    VideoFilter(VF(Speed(param$speed),
                   Crop(param$crop),
                   FrameRate(param$frame_rate),
                   Palette(param$palette))),
    Loop(param$loop),
    output
  )
}



# Helper functions for handling FFMPEG flags ----
FFMPEG <- function(...) {
  paste(c("ffmpeg", ...), collapse = " ")
}

Flag <- function(flag) {
  function(x) {
    if (length(x) == 0) return(NULL)
    paste0(flag, " ", x)
  }
}

Input <- Flag("-i")
Time <- Flag("-t")
Skip <- Flag("-ss")
Codec <- Flag("-vcodec")
VideoFilter <- Flag("-vf")
Output <- Flag("")
Loop <- Flag("-loop")



# Helper functions for Video Filters ----
VF <- function(...) {
  if (missing(...) || length(c(...)) == 0) {
    return(NULL)
  }
  dQuote(paste(c(...), collapse = ","), "\"")
}

Palette <- function(bool) {
  if (!bool) return(NULL)
  "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse"
}

Attr <- function(attr, f = identity) {
  function(x) {
    if (length(x) == 0) return(NULL)
    paste0(attr, "=", f(x))
  }
}

FrameRate <- Attr("fps")
Crop <- Attr("crop")
Speed <- Attr("setpts", function(x) "PTS/" %+% x)



# Other helpers ----
change_file_ext <- function(x, format) {
  cs <- strsplit(basename(x), split = "[.]")[[1]]
  gsub(x, pattern = paste0(tail(cs, 1), "$"), replacement = format)
}
