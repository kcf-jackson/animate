#! config(debug = F, rules = basic_rules(), deparsers = dp("basic", "auto"))

# https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/toBlob
svg_to_png <- function(selector, scale = 1, width, height, format = "png") {
  if (!width) {
    width <- parseInt(select_dom(selector)$getAttribute("width")) || 600
    width <- width * scale
  }
  if (!height) {
    height <- parseInt(select_dom(selector)$getAttribute("height")) || 600
    height <- height * scale
  }
  output_type <- "image/" %+% format

  svg <- selector %>%
    select_dom() %>%
    XMLSerializer$new()$serializeToString()

  wURL <- window$URL
  url <- Array(svg) %>%
    Blob$new(list(type = "image/svg+xml;charset=utf-8")) %>%
    wURL$createObjectURL()

  img <- Image$new()
  img$onload <- function() {
    temp_canvas <- dom("canvas", list(style = "display: none;", id = "temp_canvas",
                                      width = width, height = height))
    render(temp_canvas)

    ctx <- temp_canvas$getContext("2d")
    ctx$drawImage(img, 0, 0, width, height)
    output <- temp_canvas$toDataURL(output_type)

    select_dom("#temp_canvas")$remove()
    wURL$revokeObjectURL(output)

    # render(dom("img", list(src = output)))
    list(dataURL = output, type = output_type)
  }
  img$src <- url
  TRUE
}
