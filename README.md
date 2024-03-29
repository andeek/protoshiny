
<!-- README.md is generated from README.Rmd. Please edit that file -->

# protoshiny

<!-- badges: start -->

[![Travis build
status](https://app.travis-ci.com/andeek/protoshiny.svg?branch=master)](https://app.travis-ci.com/andeek/protoshiny.svg?branch=master)
<!-- badges: end -->

This package launches a Shiny app to interactively visualize
hierarchical clustering with prototypes as described in [“Interactive
Exploration of Large Dendrograms with
Prototypes”](https://arxiv.org/abs/2206.01703) by Andee Kaplan & Jacob
Bien (2022).

For details on hierarchical clustering with prototypes, see
[“Hierarchical Clustering With Prototypes via Minimax
Linkage”](http://faculty.marshall.usc.edu/jacob-bien/papers/jasa2011minimax.pdf)
by Jacob Bien & Robert Tibshirani (2011).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("andeek/protoshiny")
```

## Launching the app

``` r
# launch the app
library(protoshiny)
visualize_hc()
```

## Using the app

<!-- ffmpeg -i default_data.mov -s 800x400 -pix_fmt rgb24 -r 10 -f gif - | gifsicle --optimize=3 --delay=3 > ~/Documents/research/data_viz/protoshiny/man/figures/README-default_data.gif -->

After launching the application, either upload your own cluster object
or use the provided demo data sets. To use the provided demo data sets,
simply select one from the dropdown. To quickly get to interacting with
the dendrogram, you can leave all other options as the default and click
the “Visualization” tab.

![](man/figures/README-default_data.gif)
<!-- TODO: remake to choose movies and click visualization tab. -->

If you would like to upload your own data set hierarchical clustering
object, select its location on your computer.

![](man/figures/README-upload_data.gif)

Once you have chosen the data set clustering to visualize, you can
specify more options in the application. To specify image labels for
each prototype in the dendrogram, select their location on your
computer.

![](man/figures/README-image_labels.gif)

If you would like your initial view of the dendrogram to be a dynamic
cut (provided by the `dynamicTreeCut` package), you can choose the
minimum size of the final clusters resulting from the cut.

![](man/figures/README-dynamic_cut.gif)

Once all of the options are chosen, click the “Visualization” tab to
interact with the dendrogram.

![](man/figures/README-visualize_tab.gif)

Your abilities to interact with the dendrogram include
expanding/contracting clusters,

![](man/figures/README-expand_contract.gif)

zooming and panning,

![](man/figures/README-zoom_pan.gif)

and searching for the first instance of a label.

![](man/figures/README-search.gif)
