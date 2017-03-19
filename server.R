###
### Helper functions
###
source("r/helpers.R")

##list of default data files
data_sets <- list.files("data/", pattern="*.RData")

##function for loading an object into a new environment
load_obj <- function(file) {
  env <- new.env()
  nm <- load(file, env)
  return(list(env = env, objects = nm))
}

###
### Server Definition
###

shinyServer(function(input, output) {
  
  ##display preloaded data sets
  output$choose_dataset <- renderUI({
    if(input$upload) {
      return(fileInput('dataset', 'Choose protoclust data object (.RData)', accept="application/x-RData"))
    } else {
      return(selectInput("dataset", "Data set", as.list(data_sets)))
    }
           
  })
  
  ##reactive data object
  objects <- reactive({
    supported_formats<-c("rdata") ##only accept .RData
    
    if(!is.null(input$dataset)) {
      if(class(input$dataset) == "character") {
        name <- file <- paste0("data/", input$dataset)
      } else {
        file <- input$dataset$datapath ##uploaded data
        name <- input$dataset$name
      }
      
    } else { ##data isn't loaded yet
      return()
    }
    
    if(tolower(strsplit(name, "\\.")[[1]][2]) %in% supported_formats) { ##check for correct format
      obj <- load_obj(file) #load object into new environment and store
#       if("protoclust" %in% class(dataset)) {
#         return(protoclust_to_json(dataset)) #if data in correct format
#       } else {
#         ##TODO: add error handling here
#         return()   
#       }
      return(obj)
    } else {
      ##TODO: add error handling here
      return()
    }
  })
  
  data <- reactive({
    obj <- objects()
    if(!is.null(input$object)) {
      return(obj$env[[input$object]])
    } else {
      return()
    }
  })
  
  ##allow user to choose and view loaded object
  output$objects <- reactive({ 
    obj <- objects()
    obj$objects
  })
  
  output$choose_object <- renderUI({
    obj <- objects()
    selectInput("object", "Choose loaded object", as.list(obj$objects))
  })
  
  output$view_data <- renderPrint({
    dat <- data()
    dat$paths <- protoclust::find_elements(dat)
    str(dat[-length(dat)])
  })
  
  ##send data to client side handler
  output$d3io <- reactive({ 
    dat <- protoclust_to_json(data())
    dat
  })

  

  
})