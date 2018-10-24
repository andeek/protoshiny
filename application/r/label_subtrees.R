
get_nodes_to_expand_info <- function(hc, dc) {
  # hc: hclust object
  # dc: a vector of length n with integers between 0 and 
  #   num_clusters such as is outputted by dynamicTreeCut::cutreeDynamicTree
  #
  # Returns an n-vector giving the labels of each interior node. A positive value
  # means that all nodes in that node's subtree have that label.  A value of -1 means
  # that this node's children have different labels.  A value of -2 means that at least
  # one child has a value of -1.  And so forth.
  # 
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
