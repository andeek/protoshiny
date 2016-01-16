## Idea: could I use shinychords here?

###
### Libraries
###
library(shinythemes)


###
### Additional functions
###
dynGraph <- function(inputoutputId) 
{
  div(id = inputoutputId, class="d3graph")
}

###
### UI Definition
###
shinyUI(
  navbarPage("protoshiny",
             id="top-nav",
             tabPanel(title="", icon=icon("home", "fa-2x"),
                      tabsetPanel(
                        tabPanel("Data",
                                 br(), 
                                 column(4, 
                                        wellPanel(
                                          selectizeInput("upload", "Data source", choices = list("Preloaded" = FALSE, "Upload user data" = TRUE)),
                                          uiOutput("choose_dataset")
                                        )
                                      ),
                                 column(4, 
                                        wellPanel(
                                          verbatimTextOutput("objects"),
                                          uiOutput("choose_object")
                                        )),
                                 column(4,
                                        verbatimTextOutput("view_data"))
                          ),
                        tabPanel("Visualization", 
                                 dynGraph(inputoutputId = 'd3io'))
                        )
             ),
             #tabPanel(title="", value="http://andeekaplan.com/protoclust", icon=icon('question-circle')),
             #tabPanel(title="", value="http://andeekaplan.com", icon=icon('envelope')),
             tabPanel(title="", value="http://github.com/andeek/protoshiny", icon=icon("github", "fa-2x")),
             footer = tagList(
               includeScript("http://d3js.org/d3.v3.min.js"),
               includeScript("scripts/top-nav-links.js"),
               includeCSS("css/dendrogram.css"),
               includeScript("scripts/dendrogram.js")
             ),
             theme = shinytheme("spacelab")
  )
)