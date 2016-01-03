## Idea: instead of this, make two tabs: upload and vis. This gives ability to choose protoclust object
##       and maximizes screen space for  visualization
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
                      column(3,
                             wellPanel(
                               h4("protoshiny:"),
                               #h5(""),
                               #p(""),
                               #p("For more information and instructions on use, click the question mark above."),
                               hr(),
                               conditionalPanel(
                                 condition = "input.upload == false",
                                 uiOutput("choose_dataset")
                               ),
                               checkboxInput("upload", "Upload new dataset", FALSE),
                               conditionalPanel(
                                 condition = "input.upload == true",
                                 fileInput('dataset_up', 'Choose protoclust data object (.RData)',
                                           accept="application/x-RData")
                               )   

                      ),
                      
                      column(9,    
                             dynGraph(inputoutputId = 'd3io')
                             )
                      )
             ),
             #tabPanel(title="", value="http://andeekaplan.com/protoclust", icon=icon('question-circle')),
             #tabPanel(title="", value="http://andeekaplan.com", icon=icon('envelope')),
             tabPanel(title="", value="http://github.com/andeek/protoshiny", icon=icon("github", "fa-2x")),
             footer = tagList(
               includeScript("http://d3js.org/d3.v3.min.js"),
               includeScript("scripts/top-nav-links.js")
             ),
             theme = shinytheme("spacelab")
  )
)