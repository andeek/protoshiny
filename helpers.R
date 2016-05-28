##protoclust_to_list
##convert a protoclust object to a tree for creating a JSON object
##input: protoclust object including merge, labels, and height
##output: list of the top down tree structure
protoclust_to_list <- function(proto_object) {
  ##libraries
  require(dplyr)
  
  ##must be a protoclust object
  stopifnot("protoclust" %in% class(proto_object))
  
  ##extract important parts of protoclust object
  merge <- proto_object$merge %>% data.frame
  names(merge) <- c("node1", "node2")
  n <- nrow(merge) + 1
  height <- proto_object$height
  protos <- proto_object$protos
  
  leaves <- lapply(1:n, function(i) return(list(proto = i, height = 0)))
  clusters <- list()
  for(i in 1:(n-1)) {
    if(merge[i, 1] < 0) {
      a <- leaves[[-merge[i,1]]]
    } else {
      a <- clusters[[merge[i,1]]]
      clusters[[merge[i,1]]] <- NA
    }
    if(merge[i, 2] < 0) {
      b <- leaves[[-merge[i,2]]]
    } else {
      b <- clusters[[merge[i,2]]]
      clusters[[merge[i,2]]] <- NA
    }
    clusters[[i]] <- list(name = i, proto = protos[i], height = height[i], children = list(a, b))
  }
  return(clusters[[n-1]])
}
