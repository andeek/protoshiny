###
### Additional Server functions
###

#' Load an object into a new environment
#' @param file object to load into the new environment
load_obj <- function(file) {
  env <- new.env()
  nm <- load(file, env)
  return(list(env = env, objects = nm))
}

#' Convert a protoclust object to a JSON tree
#' @param proto_object protoclust object including merge, labels, and height
protoclust_to_json <- function(proto_object) {
  ##must be a protoclust object
  stopifnot("protoclust" %in% class(proto_object))

  ##extract important parts of protoclust object
  merge <- data.frame(proto_object$merge)
  names(merge) <- c("node1", "node2")
  n <- nrow(merge) + 1
  height <- proto_object$height
  protos <- proto_object$protos
  labels <- gsub('\"', '\\\\\\"', as.character(proto_object$labels)) # try to keep escaped quotes

  ##image labels
  image_labels <- "img" %in% names(proto_object)
  if(image_labels) images <- proto_object$img

  if(image_labels){
    leaves <- vapply(1:n,
                     FUN = function(i) return(paste0('{ "name" : "', labels[i], '", "img" : "', images[i], '", "proto" : ', i, ',  "height" : 0 }')),
                     FUN.VALUE = "character()")
  } else {
    leaves <- vapply(1:n,
                     FUN = function(i) return(paste0('{ "name" : "', labels[i], '", "proto" : ', i, ',  "height" : 0 }')),
                     FUN.VALUE = "character()")
  }

  clusters <- character()
  for(i in 1:(n-1)) {
    if(merge[i, 1] < 0) {
      a <- leaves[-merge[i,1]]
    } else {
      a <- clusters[[merge[i,1]]]
      clusters[merge[i,1]] <- NA
    }
    if(merge[i, 2] < 0) {
      b <- leaves[-merge[i,2]]
    } else {
      b <- clusters[[merge[i,2]]]
      clusters[merge[i,2]] <- NA
    }
    if(image_labels){
      clusters[i] <- paste0('{ "name" : "', labels[protos[i]], '", "img" : "', images[protos[i]], '", "proto" : ', protos[i], ',  "height" : ', height[i], ', "children" : [' , paste(a, b, sep = ", "), ']}')
    } else {
      clusters[i] <- paste0('{ "name" : "', labels[protos[i]], '", "proto" : ', protos[i], ',  "height" : ', height[i], ', "children" : [' , paste(a, b, sep = ", "), ']}')
    }
  }
  return(clusters[n-1])
}

#' Fix uploaded files are getting renamed
#' @param x uploaded object
fixUploadedFilesNames <- function(x) {
  if (is.null(x)) {
    return()
  }
  oldNames = x$datapath

  # copy to save location
  newcopyNames = file.path(dirname(x$datapath),
                           paste0("copy_", x$name))
  file.copy(from = oldNames, to = newcopyNames, overwrite = TRUE)

  # remove old files
  file.remove(oldNames)

  # copy to correct location
  newNames = file.path(dirname(x$datapath),
                       x$name)
  file.copy(from = newcopyNames, to = newNames, overwrite = TRUE)

  # remove copy
  file.remove(newcopyNames)

  x$datapath <- newNames
  x
}

#' Get the labels of each interior node
#' @param hc hclust object
#' @param dc a vector of length n with integers between 0 and num_clusters such as
#'     is outputted by dynamicTreeCut::cutreeDynamicTree
#' @return Returns an n-vector giving the labels of each interior node. A positive value
#'     means that all nodes in that node's subtree have that label.  A value of -1 means
#'     that this node's children have different labels.  A value of -2 means that at least
#'     one child has a value of -1.  And so forth.
get_nodes_to_expand_info <- function(hc, dc) {
  n <- length(dc)
  stopifnot(length(hc$height) == n - 1)
  lab <- rep(-1, n - 1)
  lab_sibs <- rep(NA, 2)
  for (i in seq(n - 1)) {
    sibs <- hc$merge[i, ]
    for (j in 1:2) {
      lab_sibs[j] <- ifelse(sibs[j] < 0,
                            dc[-sibs[j]], # it's a leaf
                            lab[sibs[j]] # it's an interior node
      )
    }
    if (min(lab_sibs) < 0) #
      lab[i] <- min(lab_sibs) - 1
    else if (lab_sibs[1] == lab_sibs[2])
      lab[i] <- lab_sibs[1]
  }
  lab
}


###
### Additional UI functions
###

#' Dynamic element of class "d3graph"
#' @param inputoutputId ID of the input/output element
dynGraph <- function(inputoutputId)
{
  div(id = inputoutputId, class="d3graph")
}

#' Dynamic element of class "select_custon"
#' @param inputoutputId ID of the input/output element
selectJS <- function(inputoutputId)
{
  div(id = inputoutputId, class="select_custom")
}
