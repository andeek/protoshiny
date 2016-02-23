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
  merge$height <- proto_object$height
  labels <- proto_object$labels
  protos <- proto_object$protos
  
  ##grow the list items
  nodes <- merge_to_list_items(merge, labels, protos)
  
  ##organize the list items
  nested_nodes <- nest_items(nodes, list())

}

merge_to_list_items <- function(merge, labels, protos) {
  elements <- list()
  for(i in 1:nrow(merge)) {
    if(merge[i, "node1"] > 0 & merge[i, "node2"] > 0) {
      #merging two groups
      elements[[i]] <- list(name = labels[protos[i]], 
                            height = merge[i, "height"],
                            children = c(list(name = labels[protos[merge[i, "node1"]]]),
                                         list(name = labels[protos[merge[i, "node2"]]])))
    } else if(merge[i, "node1"] < 0 & merge[i, "node2"] > 0) {
      ##merging one leaf and one group
      elements[[i]] <- list(name = labels[protos[i]], 
                            height = merge[i, "height"],
                            children = c(list(name = labels[-merge[i, "node1"]]),
                                         list(name = labels[protos[merge[i, "node2"]]])))
    } else if(merge[i, "node1"] > 0 & merge[i, "node2"] < 0) {
      ##merging one leaf and one group
      elements[[i]] <- list(name = labels[protos[i]], 
                            height = merge[i, "height"],
                            children = c(list(name = labels[protos[merge[i, "node1"]]]),
                                         list(name = labels[-merge[i, "node2"]])))
    } else {
      ##merging two leafs
      elements[[i]] <- list(name = labels[protos[i]], 
                            height = merge[i, "height"],
                            children = c(list(name = labels[-merge[i, "node1"]]),
                                         list(name = labels[-merge[i, "node2"]])))
    }
  }
  names(elements) <- labels[protos]
  return(elements)
}

nest_items <- function(nodes, nested) {
  ##need to locate where in nested we should start adding
  if(nested$name == names(nodes)[length(nodes)])
  
  nested <- nodes[[length(nodes)]]
  children <- nodes[unlist(nodes[[length(nodes)]]$children)]
  nested$children <- c(children[[1]], 
                       children[[2]]) #dendrogram, each split has two children
  
  remaining <- nodes[!(names(nodes) %in% c(unlist(nodes[[length(nodes)]]$children), nodes[[length(nodes)]]$name))]
}