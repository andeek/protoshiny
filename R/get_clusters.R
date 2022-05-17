#' Extract cluster labels
#'
#' @export
#' @param hc A protoshiny object downloaded from the web interface.
#' @return A vector of saved cluster assignments resulting from interaction in the protoshiny application.
get_clusters <- function(hc) {
  if(!("protoshiny" %in% class(hc))) stop("Please pass in a saved protoshiny object.")
  hc$protoshiny$clusters
}
