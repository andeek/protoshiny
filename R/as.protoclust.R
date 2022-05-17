#' Convert a generic hclust object to a protoclust object.
#'
#' @export
#' @param hc A generic hclust object using any linkage.
#' @param protos A vector of prototype label indices for each branch in the dendrogram.
#'               The i-th element is the index of the prototype corresponding to the cluster 
#'               formed on the i-th merge.
#' @return An object of class \code{\link{protoclust}} with a user specified vector of representative prototypes for each cluster.
#'         This object is just like \code{hclust} but has an identical element:
#'         \item{merge, height,order}{identical to the values returned by \code{\link{hclust}}}
#'         \item{protos}{a vector of length n - 1.  The i-th element is the index of
#'                       the prototype corresponding to the cluster formed on the i-th merge.}
as.protoclust <- function(hc, protos) {
  UseMethod("as.protoclust", hc)
}

#' @export
#' @method as.protoclust default
as.protoclust.default <- function(hc, protos){
  warning(paste("as.protoclust does not know how to handle object of class ", 
                class(hc), 
                "and can only be used on hclust objects."))
}


#' @export
#' @method as.protoclust hclust
as.protoclust.hclust <- function(hc, protos) {
  stopifnot("hclust" %in% class(hc))
  
  if(length(protos) != nrow(hc$merge)) stop("Prototype vector must be the same length as the number of branches in the dendrogram.")
  
  if(!(class(protos) %in% c("numeric", "integer"))) stop("Prototype vector must provide index of each element to label.")
  
  hc$protos <- protos
  class(hc) <- c("protoclust", class(hc))
  
  return(hc)
}