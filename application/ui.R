###
### Libraries
###
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(devtools)) install.packages("devtools")
if(!require(protoclust)) devtools::install_github("jacobbien/protoclust")
if(!require(dynamicTreeCut)) install.packages("dynamicTreeCut")
if(!require(DT)) install.packages("DT")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
library(shinythemes)
library(shinycssloaders)


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
                                          tags$b("Protoshiny"), 
                                          "is a Shiny app for interactive",
                                          "visualization of",
                                          a("hierarchical clustering with prototypes.", 
                                            href = "http://faculty.marshall.usc.edu/Jacob-Bien/papers/jasa2011minimax.pdf"),
                                          br(),
                                          br(),
                                          tags$b("Step 1:"),
                                          "Choose a preloaded example or upload your own",
                                          a("protoclust object", 
                                            href = "https://github.com/jacobbien/protoclust"),
                                          "(then make sure the protoclust object is the chosen 'loaded object' from the .Rdata file).",
                                          br(),
                                          tags$b("Step 2:"),
                                          "(optional) If image labels are desired, change 'label type' to 'Image'.",
                                          "Also, choose whether to automate initial view of tree.",
                                          br(),
                                          tags$b("Step 3:"),
                                          "Click on the 'Visualization' tab."),
                                        wellPanel(
                                          selectizeInput("upload", 
                                                         HTML("Data source", as.character(actionLink("help_source", icon("info-circle")))), 
                                                         choices = list("Preloaded" = FALSE, "Upload user data" = TRUE)),
                                          uiOutput("choose_dataset"),
                                          #helpText("[TODO] Add help text about object selection."),
                                          #verbatimTextOutput("objects"),
                                          uiOutput("choose_object")
                                          
                                        )
                                      ),
                                 column(4, 
                                        wellPanel(
                                          uiOutput("choose_display_options")
                                        )),
                                 column(4,
                                        # verbatimTextOutput("view_data"),
                                        helpText("[TODO] Add help text about third column."),
                                        uiOutput("table_output")
                                        )
                          ),
                        tabPanel("Visualization", 
                                 column(12, withSpinner(dynGraph(inputoutputId = 'd3io'),  type=7), 
                                                        selectJS(inputoutputId = "select_label"))
                                 
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