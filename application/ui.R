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

selectJS <- function(inputoutputId) 
{
  div(id = inputoutputId, class="select_custom")
}

###
### UI Definition
###
shinyUI(
  navbarPage("protoshiny",
             id="top-nav",
             tabPanel(title="", icon=icon("home", "fa-2x"),
                      div(style = "position:absolute;right:1em;", 
                          actionLink('reset', 'Reset')),
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
                                        verbatimTextOutput("view_data")),
                                 verbatimTextOutput("tmp")
                          ),
                        tabPanel("Visualization", 
                                 column(12, dynGraph(inputoutputId = 'd3io'), selectJS(inputoutputId = "select_label"))
                                 
                        )
                      )
             ),
             tabPanel(title="", value="http://github.com/andeek/protoshiny", icon=icon("github", "fa-2x")),
             footer = tagList(
               includeScript("scripts/d3.v3.min.js", charset="utf-8"),
               includeScript("scripts/top-nav-links.js"),
               includeScript("scripts/selectize.js"),
               includeCSS("css/dendrogram.css"),
               includeCSS("css/slider.css"),
               includeCSS("css/selectize.bootstrap3.css"),
               includeScript("scripts/dendrogram.js"),
               includeScript("scripts/select_label.js")
             ),
             theme = shinytheme("spacelab")
  )
)