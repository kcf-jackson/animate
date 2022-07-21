# FFMPEG command builder
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


# Working with FFMPEG flags
`%+%` <- paste0
FFMPEG <- \(...) paste(c("ffmpeg", ...), collapse = " ")
Flag <- \(flag) \(x) if (length(x) == 0) NULL else (flag %+% " " %+% x)

Input <- Flag("-i")
Time <- Flag("-t")
Skip <- Flag("-ss")
Codec <- Flag("-vcodec")
VideoFilter <- Flag("-vf")
Output <- Flag("")
Loop <- Flag("-loop")


# Working with Video Filters
VF <- \(...) {
  if (missing(...) || length(c(...)) == 0) {
    return(NULL)
  }
  dQuote(paste(c(...), collapse = ","), "\"")
}
Attr <- \(attr, f = identity) \(x) if (length(x) == 0) NULL else (attr %+% "=" %+% f(x))

FrameRate <- Attr("fps")
Crop <- Attr("crop")
Speed <- Attr("setpts", \(x) "PTS/" %+% x)
Palette <- \(bool) if (bool) "split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" else NULL


# Other helpers
change_file_ext <- function(x, format) {
  cs <- strsplit(basename(x), split = "[.]")[[1]]
  gsub(x, pattern = paste0(tail(cs, 1), "$"), replacement = format)
}


# # Usage
# ffmpeg_command("abc.mp4", "edited.mp4", skip = 5, time = 10,
#                crop = "1000:500:0:0", speed = "2.0", codec = "h264") |> print()
# ffmpeg_command("abc.mp4", "abc.gif", frame_rate = 20, loop = 0, palette = TRUE) |> print()
