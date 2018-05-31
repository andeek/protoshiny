##protoclust_to_json
##convert a protoclust object to a JSON tree
##input: protoclust object including merge, labels, and height
##output: Valid JSON string
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

# from https://github.com/daattali/advanced-shiny/blob/master/upload-file-names/app.R
# uploaded files are getting renamed
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
