#' Server function
#'
#' @importFrom shiny shinyServer
#' @importFrom shiny renderUI
#' @importFrom shiny helpText
#' @importFrom shiny fileInput
#' @importFrom shiny selectInput
#' @importFrom shiny reactiveVal
#' @importFrom shiny renderUI
#' @importFrom shiny tagList
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny radioButtons
#' @importFrom shiny HTML
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tagList
#' @importFrom shiny observeEvent
#' @importFrom shiny showModal
#' @importFrom shiny br
#' @importFrom shiny modalDialog
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom dynamicTreeCut cutreeDynamicTree
#' @importFrom shiny addResourcePath
#' @importFrom shiny removeResourcePath
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom shiny updateNumericInput
#' @importFrom DT formatStyle
#' @importFrom DT styleEqual
#' @importFrom shiny observe
#' @importFrom shiny numericInput
#' @importFrom stats quantile
#' @importFrom DT DTOutput
#' @importFrom tools file_ext
#' @importFrom protoclust find_elements
#' @importFrom shiny downloadHandler
#' @importFrom methods is
get_server <- function() {
  ##list of default data files
  data_sets <- list.files(system.file("ext_data", package = "protoshiny", mustWork = TRUE), pattern="*.RData|*.Rdata")

  ###
  ### Server Definition
  ###
  shinyServer(function(input, output, session) {

    ##display preloaded data sets ----
    output$choose_dataset <- renderUI({
      if(input$upload) {
        return(list(helpText("Use the",
                             a("protoclust", href = "https://github.com/jacobbien/protoclust"),
                             "R package to create a protoclust object and save it to a .Rdata file."),
                    fileInput('dataset',
                              'Choose .Rdata file with protoclust object',
                              accept="application/x-RData")))
      } else {
        return(selectInput("dataset",
                           HTML("Data set", as.character(actionLink("help_dataset_preload", icon("info-circle")))),
                           as.list(data_sets)))
      }

    })
    object_loaded <- reactiveVal(NULL)

    ## dynamic UI ----
    output$choose_object <- renderUI({
      obj <- objects()
      if (!is.null(obj)) {
        ii <- as.numeric(which(sapply(obj$env, function(o) "protoclust" %in% class(o))))
        if (length(ii) == 0) {
          stop(".Rdata file must have a protoclust object.")
        } else if (length(ii) == 1) {
          # can we skip this control and just set input$object to ii?
          object_loaded(obj$objects[ii])
          return()
        } else {
          tagList(
            helpText("Choose the protoclust object to visualize."),
            verbatimTextOutput("objects"),
            selectInput("object",
                        HTML("Choose loaded object", as.character(actionLink("help_object", icon("info-circle")))),
                        as.list(obj$objects))
          )
        }
      }
    })
    output$choose_display_options <- renderUI({
      dat <- data()
      
      if("protoshiny" %in% class(dat)) {
        init_obj <- radioButtons("init_type",
                                 HTML("Choose initial display type", as.character(actionLink("help_init", icon("info-circle")))),
                                 choices = c("Saved" = "saved", "Top 15" = "default", "Dynamic Cut" = "dynamic"))
      } else {
        init_obj <- radioButtons("init_type",
                                 HTML("Choose initial display type", as.character(actionLink("help_init", icon("info-circle")))),
                                 choices = c("Top 15" = "default", "Dynamic Cut" = "dynamic"))
      }
      
      tagList(
        # selectInput("object",
        #             HTML("Choose loaded object", as.character(actionLink("help_object", icon("info-circle")))),
        #             as.list(obj$objects)),
        radioButtons("label_type",
                     HTML("Choose label type", as.character(actionLink("help_label", icon("info-circle")))),
                     choices = c("Text" = "text", "Image" = "image")),
        conditionalPanel(
          condition = "input.label_type == 'image'",
          fileInput('images',
                    HTML('Upload all label images (.png)', as.character(actionLink("help_label_image", icon("info-circle")))),
                    accept="image/png", multiple = TRUE)
        ),
        init_obj, ## choices change depending on if saved object reloaded
        conditionalPanel(
          condition = "input.init_type == 'dynamic'",
          numericInput('min_module_size',
                       'Specify minimum module size parameter (minModuleSize)',
                       min = 1,
                       value = 2),
          helpText("minModuleSize parameter controls the number of starting nodes in the dendrogram. See table to the right for suggested value in red.")
        )
      )
    })
    output$table_output <- renderUI({
      conditionalPanel(
        condition = "input.init_type == 'dynamic'",
        withSpinner(DTOutput("number_clusters"), type=7),
        helpText("We recommend you start looking at the dendrogram with as close to 50 nodes on the screen as possible. Choose the minModuleSize parameter (left) with the value that results in your desired number of approximate nodes.")
      )
    })

    ## help buttons ----
    observeEvent(input$help_source, {
      showModal(modalDialog(
        "Choose 'Upload user data' to upload your own .Rdata file.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_dataset_preload, {
      showModal(modalDialog(
        "Choose one of the example data sets.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_object, {
      showModal(modalDialog(
        "Choose which protoclust object to visualize.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_label, {
      showModal(modalDialog(
        "Labels appear at each node of the tree.",
        "For interior nodes, the label of the prototype is shown;",
        "for leaf nodes, the label of the leaf is shown. The labels can be",
        "either text or images.",
        br(),
        br(),
        tags$b("Text labels:"),
        "These are taken from the protoclust object's 'labels' character vector.",
        br(),
        br(),
        tags$b("Image labels:"),
        "The file name of each image is given by the protoclust object's 'img' character vector.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_label_image, {
      showModal(modalDialog(
        "Select all of the image files at once.",
        "The file names of these images should match what is given",
        "in the protoclust object's 'img' character vector.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_init, {
      showModal(modalDialog(
        "By default, the highest 15 nodes in the tree are shown.",
        "'Dynamic cut' is a data-adaptive algorithm that chooses how much of the tree to show initially.",
        "If a previous session is loaded into protoshiny, you will be able to choose the saved state as the initial view.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_cluster_download, {
      showModal(modalDialog(
        "Download a 'protoshiny' object containing the cluster label vector resulting from current dendrogram.", 
        "To easily access these clusteers, see the protoshiny::get_clusters function.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })
    observeEvent(input$help_reset, {
      showModal(modalDialog(
        "Reset dendrogram back to settings in Data tab.",
        easyClose = TRUE,
        footer = NULL,
        size = "s"
      ))
    })

    ##reactive data object ----
    objects <- reactive({
      supported_formats <- c("rdata") ##only accept .RData

      if(!is.null(input$dataset)) {
        if(is(input$dataset, "character")) {
          name <- file <- system.file("ext_data", input$dataset, package = "protoshiny", mustWork = TRUE)
        } else {
          file <- input$dataset$datapath ##uploaded data
          name <- input$dataset$name
        }

      } else { ##data isn't loaded yet
        return()
      }

      if(tolower(file_ext(name)) %in% supported_formats) { ##check for correct format
        obj <- load_obj(file) #load object into new environment and store
        return(obj)
      } else {
        ##TODO: add error handling here
        return()
      }
    })
    data <- reactive({
      obj <- objects()
      ii <- object_loaded()
      if(!is.null(input$object)) {
        return(obj$env[[input$object]])
      } else if(!is.null(ii)){
        return(obj$env[[ii]])
      } else {
        return()
      }
    })
    labels <- reactive({
      dat <- data()
      if(!is.null(dat)) {
        return(find_elements(dat))
      } else {
        return(NULL)
      }
    })

    ## make path reactive so that it can reset with the new data ----
    path <- reactiveVal(NULL)
    reset_path <- reactiveVal(NULL)

    ## keep track of selected tab ----
    tab <- reactiveVal(NULL)
    observeEvent(input$tabs, {
      tab(input$tabs)
    })

    # keep track of path for reset when tabs change ----
    observeEvent(input$tabs, {
      pa <- path()
      if(input$tabs == "Visualization") reset_path(pa)
    })
    
    ## update path based on initial view ----
    observeEvent({input$init_type; input$min_module_size}, {
      req(input$init_type, input$min_module_size)
      dat <- data()
      lab <- labels()
      
      if(!is.null(dat)) {
        if(input$init_type == 'dynamic') {
          d <- input$min_module_size
          
          height <- quantile(dat$height, .1)
          
          # get dynamic cuts
          dc <- cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = d)
          out <- get_nodes_to_expand_info(dat, dc)
          
          # nodes to expand
          path(lapply(lab$int_paths[out == -1], function(x) paste(x, collapse = ",")))
        } else if(input$init_type == 'saved') {
          paths <- get_paths_from_cut(dat, dat$protoshiny$clusters)
          path(lapply(paths, function(x) paste(x, collapse = ",")))
        } else {
          path(NULL)
        }
      } else {
        path(NULL)
      }
      
    })

    ## updates path based on the search functionality ----
    observeEvent(input$select_label, {
      path(input$select_label)
    })

    ## allow user to choose and view loaded object ----
    output$objects <- reactive({
      obj <- objects()
      obj$objects
    })

    ## get img path ----
    img_path <- reactiveVal(FALSE)
    img_path_loc <- reactiveVal(FALSE)
    observeEvent(input$images, {
      pa <- img_path_loc()
      if(input$label_type == "image") {
        fixed_images <- fixUploadedFilesNames(input$images)
        datapa <- dirname(fixed_images$datapath)[1]
        img_path_loc(datapa)
        
        addResourcePath(gsub("/", "", datapa), datapa)
        img_path(TRUE)
      }
    })
    observeEvent(input$label_type, {
      if(input$label_type == "text") img_path(FALSE)
      else if(input$label_type == "image") img_path(TRUE)
    })

    ## preview number of initial nodes ----
    output$number_clusters <- renderDT({
      req(input$init_type)
      null_table <- datatable(data.frame(minModuleSize = NULL,
                                         `number clusters` = NULL,
                                         `number inner nodes` = NULL),
                              options = list(dom = 't'),
                              rownames = FALSE)

      if(input$init_type == 'dynamic') {
        dat <- data()
        if(!is.null(dat)) {
          height <- quantile(dat$height, .1)
          range <- c(2, 4)

          # get dynamic cuts
          cuts <- lapply(range, function(i) cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = i))
          num_init <- unlist(lapply(cuts, function(x) length(unique(x))))

          # course jumps
          counter1 <- 0 # make sure this doesn't go forever
          while(num_init[2] > 1 & counter1 < 10) {
            range[2] <- range[2] + 20
            cuts[[2]] <- cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = range[2])
            num_init[2] <- length(unique(cuts[[2]]))
            counter1 <- counter1 + 1
          }

          # half steps
          counter2 <- 0 # make sure this doesn't go forever
          while(num_init[2] == 1 & counter2 < 10 & counter1 > 0) {
            range[2] <- (range[2] - range[1])/2
            cuts[[2]] <- cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = range[2])
            num_init[2] <- length(unique(cuts[[2]]))
            counter2 <- counter2 + 1
          }

          # small jumps
          counter3 <- 0 # make sure this doesn't go forever
          while(num_init[2] > 1 & counter3 < 10) {
            range[2] <- range[2] + 2
            cuts[[2]] <- cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = range[2])
            num_init[2] <- length(unique(cuts[[2]]))
            counter3 <- counter3 + 1
          }

          d <- unique(round(seq(from = range[1], to = range[2], length.out = 6)))
          cuts_inner <- lapply(d[-c(1, length(d))], function(i) cutreeDynamicTree(dat, maxTreeHeight = height, minModuleSize = i))
          num_init_inner <- unlist(lapply(cuts_inner, function(x) length(unique(x))))

          outs <- lapply(c(list(cuts[[1]]), cuts_inner, list(cuts[[2]])), function(cuts) get_nodes_to_expand_info(dat, cuts))

          res <- data.frame(minModuleSize = d, `number clusters` = c(num_init[1], num_init_inner, num_init[2]) - 1,
                            `approx nodes` = unlist(lapply(outs, function(x) sum(x < -1)))*2 + unlist(lapply(outs, function(x) sum(x == -1))))

          best_idx <- abs(res[, 3] - 50) == min(abs(res[res[, 3] > 0, 3] - 50)) & res[, 3] > 0
          best_val <- res[best_idx, 3]
          other_vals <- setdiff(res[, 3], best_val)

          updateNumericInput(session, "min_module_size", value = min(d[best_idx]))

          res_table <- datatable(res, options = list(dom = 't'), rownames = FALSE)

          formatStyle(res_table,
                      3, target = 'row',
                      fontWeight = styleEqual(c(other_vals, best_val), c(rep("normal", length(other_vals)), rep("bold", length(best_val)))),
                      color = styleEqual(c(other_vals, best_val), c(rep("#666666", length(other_vals)), rep("red", length(best_val)))))

        } else {
          null_table
        }
      } else {
        null_table
      }
    })

    ##send data to client side handler ----
    observe({
      output$d3io <- reactive({
        dat <- data()
        if(!is.null(dat)) {
          json <- protoclust_to_json(dat)
          pa <- path()
          img_pa <- img_path()
          img_pa_loc <- img_path_loc()
          list(data = json, path = pa, img_path = img_pa, img_path_loc = gsub("/", "", img_pa_loc))
        } else {
          list(data = NULL, path = NULL, img_path = NULL, img_path_loc = NULL)
        }
      })
    })

    ## search box labels ----
    output$select_label <- reactive({
      select_tab <- tab()
      dat <- data()
      pa <- path()
      ## reset path when getting new data/labels
      ## path(NULL)
      lab <- labels()
      res <- list(paths = lab$paths, tab = select_tab)
      names(res$paths) <- dat$labels
      res
    })

    ## reset button
    observeEvent(input$reset, {
      path("reset the image")
      pa <- reset_path()
      path(pa)
    })
    
    ## download button
    clustersDownload <- reactive({
      if(exists("input") && length(names(input)) > 0){
        cluster_data <- input[["download"]]
        return(cluster_data)
      } else {
        return("no input")
      }
    })
    
    output$download <- downloadHandler(
      filename = function() paste("protoshiny-hc-", gsub(":", "", gsub("\\s", "_", Sys.time())), ".RData", sep=""),
      content = function(file) {
        cl <- clustersDownload()
        
        df <- list()
        for(i in seq_along(cl)) {
          df <- c(df, list(unlist(cl[[i]])))
        }
        names(df) <- names(cl)
        df <- as.data.frame(df)
        df$merge_id <- as.integer(df$merge_id)
        
        hc <- data()
        terminal <- df$terminal | df$merge_id < 0
        cl <- get_cut_from_merge_id(hc, df$merge_id[terminal])
        
        hc$protoshiny <- list(clusters = cl, save_time = Sys.time())
        class(hc) <- c("protoshiny", class(hc))
        
        save(hc, file = file)
    })
  })
}

#' UI function
#'
#' @importFrom shiny shinyUI
#' @importFrom shiny tabPanel
#' @importFrom shiny div
#' @importFrom shiny icon
#' @importFrom shiny actionLink
#' @importFrom shiny downloadLink
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny br
#' @importFrom shiny column
#' @importFrom shiny wellPanel
#' @importFrom shiny tags
#' @importFrom shiny a
#' @importFrom shiny selectizeInput
#' @importFrom shiny uiOutput
#' @importFrom shiny includeScript
#' @importFrom shiny includeCSS
#' @importFrom shinythemes shinytheme
#' @importFrom shinycssloaders withSpinner
#' @importFrom shiny navbarPage
get_ui <- function() {
  shinyUI(
    navbarPage("protoshiny",
               id="top-nav",
               tabPanel(title="", icon=icon("home", "fa-2x"),
                        div(style = "position:absolute;right:1em;visibility:hidden;",
                            downloadLink('download', 'Download'),
                            actionLink("help_cluster_download", icon("info-circle")),
                            " | ",
                            actionLink('reset', 'Reset'), 
                            actionLink("help_reset", icon("info-circle")),
                            class = "link_buttons"),
                        tabsetPanel(id = "tabs",
                                    tabPanel("Data", id = "data_tab",
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
                                                      "Click on the 'Visualization' tab. Click on prototypes to expand the subtrees they represent."),
                                                    wellPanel(
                                                      selectizeInput("upload",
                                                                     HTML("Data source", as.character(actionLink("help_source", icon("info-circle")))),
                                                                     choices = list("Preloaded" = FALSE, "Upload user data" = TRUE)),
                                                      uiOutput("choose_dataset"),
                                                      uiOutput("choose_object")

                                                    )
                                             ),
                                             column(4,
                                                    wellPanel(
                                                      uiOutput("choose_display_options")
                                                    )),
                                             column(4,
                                                    uiOutput("table_output")
                                             )
                                    ),
                                    tabPanel("Visualization", id = "viz_tab",
                                             column(12,
                                                    withSpinner(dynGraph(inputoutputId = 'd3io'),  type=7),
                                                    selectJS(inputoutputId = "select_label"))
                                    )
                        )
               ),
               tabPanel(title="", value="http://github.com/andeek/protoshiny", icon=icon("github", "fa-2x")),
               footer = tagList(
                 includeScript(system.file("dendrogram/lib/d3", "d3.v3.min.js", package = "protoshiny"), charset="utf-8"),
                 includeScript(system.file("dendrogram", "top-nav-links.js", package = "protoshiny")),
                 includeScript(system.file("dendrogram/lib/selectize", "selectize.js", package = "protoshiny")),
                 includeScript(system.file("dendrogram/lib/selectize", "selectize-plugin-a11y.js", package = "protoshiny")),
                 includeCSS(system.file("dendrogram", "dendrogram.css", package = "protoshiny")),
                 includeCSS(system.file("dendrogram/lib/selectize", "selectize.bootstrap3.css", package = "protoshiny")),
                 includeScript(system.file("dendrogram", "dendrogram.js", package = "protoshiny")),
                 includeScript(system.file("dendrogram", "select_label.js", package = "protoshiny"))
               ),
               theme = shinytheme("spacelab")
    )
  )
}
