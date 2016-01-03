###
### Additional functions
###

##list of default data files
data_sets <- paste("data/", list.files("data/", pattern="*.RData"), sep="")

##function for loading an object into a new environment
load_obj <- function(file) {
  env <- new.env()
  nm <- load(file, env)[1]
  env[[nm]]
}

###
### Server Definition
###

shinyServer(function(input, output) {
  
  ##display preloaded data sets
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  ##reactive data object
  data <- reactive({
    supported_formats<-c("rdata") ##only accept .RData
    
    if(!input$upload & !is.null(input$dataset)) { ##default datasets
      file <- input$dataset
    } else if (input$upload & !is.null(input$dataset_up)) { ##user dataset
      file <- input$dataset_up
    } else { ##data isn't loaded yet
      return()
    }
    
    if(tolower(strsplit(dataset, "\\.")[[1]][2]) %in% supported_formats) { ##check for correct format
      dataset <- load_obj(file) #load object into new environment and store
      if("protoclust" %in% class(dataset)) {
        return(protoclust_to_json(dataset)) #if data in correct format
      } else {
        ##TODO: add error handling here
        return()   
      }
    } else {
      ##TODO: add error handling here
      return()
    }
  })
  
  ##send data to client side handler
  output$d3io <- reactive({ data() })
  
})