## R package 'animate': A web-based graphics device for animated visualisation

_(Developer Preview: The API is maturing and feedback is welcome!)_

The R package 'animate' implements a web-based graphics device that models on the base R syntax and is powered by [d3.js](https://d3js.org/). The device is developed using the [sketch](https://github.com/kcf-jackson/sketch) package and targets real-time 
animated visualisations. The key use cases in mind are agent-based modelling and 
dynamical system, and it may also find applications in sports analytics, board 
game analysis and basic animated charting.

The package is relatively lightweight containing just above 1000 lines of code 
and depends on the packages `R6`, `httpuv`, `base64enc`, `jsonlite` (and 
`rmarkdown`, `shiny` and `knitr` if you need the corresponding integration). 
Note that while the animated visualisation requires R to generate, it is standalone once it is generated, so sharing can be done easily.

The difference of this package with [sketch](https://github.com/kcf-jackson/sketch)
is that it has a narrower focus (i.e. it does fewer things), and with that trade-off, 
it is possible to eliminate completely the need of knowing JavaScript to create
animated visualisations.

See below for three demos extracted from a [presentation](../man/slides/SVI_presentation.html) I gave in June at my institute, and see the [vignettes]() for a thorough introduction to the package.


#### Lorenz system

<iframe width="800" height="450" src="https://www.youtube.com/embed/KuDXRLiFKso" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen data-external="1"></iframe>


#### Predator-Prey process

<iframe width="800" height="450" src="https://www.youtube.com/embed/YPS1lkn-XT4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen data-external="1"></iframe>


#### Schelling's model of segregation

<iframe width="800" height="450" src="https://www.youtube-nocookie.com/embed/pmmOAh3-6iA" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen data-external="1"></iframe>


## Acknowledgement

This work is supported by my position at the [Bioinformatics and Cellular Genomics lab (BioCellGen)](https://www.svi.edu.au/research_themes/bioinformatics_and_cellular_genomics/) at the St. Vincent's Institute of Medical Research (SVI) under the 
supervision of [Davis McCarthy](https://www.svi.edu.au/research_themes/research_staff/dr_davis_mccarthy).
