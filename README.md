# protoshiny

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/andeek/protoshiny.svg?branch=master)](https://travis-ci.com/andeek/protoshiny)
<!-- badges: end -->

This package launches a Shiny app to interactively visualize hierarchical clustering with prototypes.

For details on hierarchical clustering with prototypes, see http://faculty.bscb.cornell.edu/~bien/papers/jasa2011minimax.pdf

# Installation

```
# Install the development version from GitHub
devtools::install_github("andeek/protoshiny")
```

# Launching the app

```
# launch the app from rstudio
library(protoshiny)
visualize_hc()
```

# Using the app

After launching the application, either upload your own cluster object or use the provided demo datasets. Click the `Visualization` tab to interact with the dendrogram.


