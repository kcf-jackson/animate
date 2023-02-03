check_comment_out <- function(x) {
  lines <- trimws(readLines(x))
  lines <- lines[lines != ""]
  ss <- strsplit(lines, split = "")
  first_char <- function(x) x[1]
  all(sapply(ss, first_char) == "#")
}


all_dev_commented_out <- all(sapply(
  c("inst/dev/build_helpers.R", "inst/dev/build.R"),
  check_comment_out
))


if (all_dev_commented_out) {
  message("[OK] All dev files are commented out.")
} else {
  warning("All dev files should be commented out before publishing.")
}
