#' Get the upper cut of a protoclust object
#' 
#' Gets the upper part of the tree when you cut at height h (or alternately to 
#' have k leaves).  Returns a valid protoclust object.  This new tree has a
#' different indexing from the original tree.  i.original can be used to recover
#' the old indexing and merge.id indicates for each leaf of new tree that had
#' been an interior node, the merge.id of it.
#' 
#' @param hc protoclust object
#' @param h cut height
#' @param k number of leaves that upper cut should have
#' 
#' @return 
#' \describe{
#'   \item{hc}{protoclust object representing the upper cut}
#'   \item{i.original}{index of each element in original tree}
#'   \item{merge.id.leaf}{merge.id from original tree of each leaf}
#'   \item{merge.id.interior}{merge.id from original tree of each interior node}
#'   \item{h}{cut height}
#'   \item{n.original}{number of objects in original tree}
#' }
#' @keywords internal
upper <- function(hc, h = NULL, k = NULL) {
  n = length(hc$order)
  if (is.null(h)) {
    if (is.null(k))
      stop("Must input either k or h.")
    if (k < 3 || round(k) != k)
      stop("Invalid k")
    if (k > n - 1)
      h = 0
    else
      h = hc$height[n - k]
  } 
  else if (!is.null(k))
    warning("Ignoring k since h is provided.")
  
  if (h >= hc$height[n - 2])
    stop("Cutting too high...")
  m = which(hc$height > h)[1]
  if (!is.null(k)) {
    if (k != n - m + 1)
      cat("k (=", k, ") provided is not possible because of ties.\n")
  }
  ii = seq(m, n - 1)
  merge = hc$merge[ii, , drop = F]
  
  # convert interior nodes to leaves:
  iless = which(merge < m & merge > 0) # the new leaves
  mm.ids.leaf = merge[iless]
  merge[iless] = -hc$proto[mm.ids.leaf] # labeled by prototype
  # interior nodes that are still interior should be shifted
  imore = which(merge >= m)
  merge[imore] = merge[imore] - m + 1
  
  # get all the new leaf indices and extract their order
  # from original order vector.
  mm = -merge[merge < 0]
  ord = hc$order[hc$order %in% mm]
  
  # the index of each element in the original tree
  i.original = sort(mm)
  # the merge.id in the original tree of each leaf in this tree
  # set to NA if the leaf was also a leaf originally
  
  is.protoleaf = i.original %in% -merge[iless]
  merge.id.leaf = rep(NA, length(ord))
  o = order(hc$protos[mm.ids.leaf])
  merge.id.leaf[is.protoleaf] = mm.ids.leaf[o]
  
  # relabel leaf indices to be 1:k...
  merge[merge < 0] = -rank(mm)
  ord = rank(ord)
  
  #cl = protocut(hc,h=h)$cl
  #clustSizes = table(cl)
  
  hc$height = hc$height[ii]
  hc$protos = hc$protos[ii]
  hc$merge = merge
  hc$order = as.integer(ord)
  
  if ("labels" %in% names(hc)) hc$labels <- hc$labels[i.original]
  list(hc = hc, i.original = i.original, merge.id.leaf = merge.id.leaf,
       merge.id.interior = ii, h = h, n.original = n)
}

#' Get an hclust object consisting of the branch with root given by a particular interior node
#' @param hc protoclust object
#' @param merge.id a particular interior node
#' @return an hclust object consisting of the branch with root given by merge.id.
#' @keywords internal
branch <- function(hc, merge.id) {
  n = length(hc$order)
  ii = seq(merge.id)
  merge = hc$merge[ii,]
  root = hc$protos[merge.id]
  
  ll = merge.id
  keep = NULL # rows of merge having to do with this branch
  while(length(ll)>0)
  {
    keep = c(keep,ll[1])
    row = rev(merge[ll[1],])
    ll = c(ll,row[row>0])
    ll = ll[-1]
  }
  keep = sort(keep)
  merge = merge[keep,]
  merge[merge>0] = rank(merge[merge>0])
  
  # get the order...
  mm = -merge[merge<0]
  ord = hc$order[hc$order%in%mm]
  
  # relabel leaves to have 1:k:
  merge[merge<0] = -rank(mm)
  ord = rank(ord)
  # the labels of each element in the original tree
  i.original = sort(mm)
  
  hc$merge = merge
  hc$height = hc$height[keep]
  hc$protos = match(hc$protos[keep],i.original)
  hc$order = ord
  if(is.null(hc$labels))
    hc$labels = i.original
  else
    hc$labels = hc$labels[i.original]
  if(is.null(hc$imlabels))
    hc$imlabels = i.original # can I get rid of this?
  else if(is.list(hc$imlabels))
    hc$imlabels$ims = hc$imlabels$ims[i.original,]
  hc$root = root
  
  hc
}
