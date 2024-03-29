% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmarkdown.R
\name{insert_animate}
\alias{insert_animate}
\title{Insert an animated plot into an R Markdown document}
\usage{
insert_animate(file, options = click_to_play(), style, use_cdn = TRUE)
}
\arguments{
\item{file}{The exported plot.}

\item{options}{A character string; the JavaScript to customise the playback options.
Two basic options \code{click_to_play()} and \code{click_to_loop()} have been implemented
for general usage.}

\item{style}{Optional style for the iframe that hosts the visualisation.}

\item{use_cdn}{TRUE / FALSE; if TRUE, serve the assets from a CDN, otherwise
embed the assets into the HTML.}
}
\description{
Insert an animated plot into an R Markdown document
}
\examples{
\donttest{
input <- tempfile(fileext = ".Rmd")
output <- tempfile(fileext = ".html")
writeLines('
```{r, echo = FALSE, message = FALSE}
# Run / include the following in a code chunk of an R Markdown document
library(animate)
insert_animate(system.file("tests/Lorenz_system.json.gz", package = "animate"),
               options = click_to_loop())
```
', input)
knitr::knit(input, output)
# browseURL(output)
}

}
