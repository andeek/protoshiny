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
  nodes <- list()
  ## http://www.coppelia.io/2014/07/converting-an-r-hclust-object-into-a-d3-js-dendrogram/
  for (i in (1:nrow(merge))) {
    
#     if (merge[i, 1] < 0 & merge[i, 2] < 0) {
#       nodes[[i]] <- list(name = paste0("node", i), 
#                          children = list(list(name = labels[-merge[i, 1]]), list(name = labels[-merge[i, 2]])))
#     } else if (merge[i, 1] > 0 & merge[i, 2] < 0) {
#       nodes[[i]] <- list(name = paste0("node", i), 
#                    children = list(list(name = nodes[[merge[i, 1]]]), list(name = labels[-merge[i, 2]])))
#     } else if (merge[i, 1] < 0 & merge[i, 2] > 0) {
#       nodes[[i]] <- list(name = paste0("node", i), 
#                          children = list(list(name = labels[-merge[i, 1]], list(name = nodes[[merge[i, 2]]]))))
#     } else if (merge[i, 1] > 0 & merge[i, 2] > 0) {
#       nodes[[i]] <- list(name = paste0("node", i), 
#                          children = list(list(name = nodes[[merge[i, 1]]], list(name = nodes[[merge[i, 2]]]))))
#     }
    
      
      if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
      else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
      else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
      else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
   
  }
  eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge), ")")))
  
  #return(nodes[[length(nodes)]])

}