###
### Helper functions
###
source("r/helpers.R")

##list of default data files
data_sets <- list.files("data/", pattern="*.RData|*.Rdata")

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
    supported_formats <- c("rdata") ##only accept .RData
    
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
  
  labels <- reactive({
    dat <- data()
    return(protoclust::find_elements(dat))
  })
  
  ## make path reactive so that it can reset with the new data
  path <- reactiveVal(NULL)
  observeEvent(input$select_label, {
    path(input$select_label)             
  })
  
  
  ##allow user to choose and view loaded object
  output$objects <- reactive({ 
    obj <- objects()
    obj$objects
  })
  
  output$choose_object <- renderUI({
    obj <- objects()
    
    tagList(
      selectInput("object", "Choose loaded object", as.list(obj$objects)),
      radioButtons("label_type", "Choose label type", choices = c("Text" = "text", "Image" = "image")),
      conditionalPanel(
        condition = "input.label_type == 'image'",
        fileInput('images', 'Upload all label images (.png)', accept="image/png", multiple = TRUE)
      )
    )
    
  })
  
  
  ## get img path
  img_path <- reactiveVal(FALSE)
  observeEvent(input$images, {
    if(input$label_type == "image") {
      fixed_images <- fixUploadedFilesNames(input$images)
      pa <- dirname(fixed_images$datapath)[1]
      addResourcePath('image_labels', pa)
      img_path(TRUE)
    }
  })
  observeEvent(input$label_type, {
    if(input$label_type == "text") img_path(FALSE)
  })
  
  output$view_data <- renderPrint({
    dat <- data()
    str(dat[-length(dat)])
  })
  
  ##send data to client side handler
  observe({
    output$d3io <- reactive({
      dat <- data()
      json <- protoclust_to_json(dat)
      pa <- path()
      img_pa <- img_path()
      list(data = json, path = pa, img_path = img_pa)
    })
  })
  
  output$select_label <- reactive({ 
    dat <- data()
    pa <- path()
    ## reset path when getting new data/labels
    ## path(NULL)
    lab <- labels()
    res <- lab$paths
    names(res) <- dat$labels
    res
  })
  
  ## reset button
  observeEvent(input$reset, {
    path("reset the image")
    path(NULL)
  })


  

  
})