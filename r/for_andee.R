library(protoclust)
source("revised_hclust_functions.R")

# generate some data
set.seed(1)
n <- 100
p <- 2
x <- matrix(rnorm(n * p), n, p)
rownames(x) <- paste("A", 1:n, sep="")
d <- dist(x)

load_depth <- 20
trigger_depth <- 3

# perform minimax linkage clustering:
hc <- protoclust(d)

# just for reference, here's the full tree
plot(hc)

# when the app loads, we load into d3 only the upper part of the tree (which I
# call the "upper cut"), which has "load_depth" leaves. Each leaf of the upper
# cut is a prototype for a branch below it
up <- upper(hc, k = load_depth)
abline(h = up$h, lty=2)
plot(up$hc)
# the indexing of objects within this tree is different from the indexing in the
# original tree, but we can get back to the original indexing... the upper cut
# tree has "load_depth" objects that had the following indices in the original
# tree:
up$i.original
# notice, for example that the third node in the upper cut is labeled
up$hc$labels[3]
# which we could have also gotten by using the original indexing and the labels
# from the original tree
hc$labels[up$i.original[3]]
# Every interior node in the original tree is numbered from 1 to n-1, specifying
# on which step it was merged (i.e., 1 represents the first merge to occur). The
# lowest interior node in the upper cut (i.e., the first merge in the upper cut)
# has the following merge id
up$merge.id.interior[1]
# in the original tree, meaning that this was the 981st merge to occur in the
# original tree. Remember that in the upper cut, many leaves are in fact
# interior nodes in the original tree.
up$merge.id.leaf[1]
# gives the merge id (in the original tree) of the first leaf in the upper cut. 
# The second leaf's merge.id is NA:
which(is.na(up$merge.id.leaf))

# this means that it corresponds to a leaf in the original tree.

# So far, we have been looking at what is first loaded when the app starts 
# however, initially, the user just sees a single root node and starts clicking,
# uncollapsing nodes... when the upper cut is loaded into d3, we will want to
# record the "depth" of the node, meaning how far this node is from the horizon
# of previously loaded elements (e.g., leaves of the upper cut would be 0, nodes
# with children that are leaves would be 1, etc). Eventually the user will click
# on a node that has depth < "trigger_depth".  This means that we should load
# the upper cut of the branch that the user may be soon to look at. To do this,
# we can use the function "branch". When the user clicks on a node of depth
# trigger_depth (we'll call that the "triggering node"), we will want to do the
# following: 
#  (a) for all the leaves that have this node as ancestor, get the merge.id from
#      the original tree.  Recall that this is given by 
#      up$merge.id.leaf[leaves_with_triggering_node_as_ancestor],
#      where "up" is the upper cut that contains the current triggering node. 
#  (b) for each of the merge.id's gotten in (a), call branch (getting the branch
#      of the original tree that has that merge.id as its root and then get the 
#      upper cut (to load_depth) of that branch.
#  (c) update the depths of all nodes in trigger_node's branch (I  think one 
#      just adds load_depth to the current depth of each)

# As an example, suppose someone clicks on the 11th merge of the upper cut
up$hc$merge[11,]
# (which corresponds to the 41st merge in the overall tree)
up$merge.id.interior[11]
# and this is a triggering node (meaning we need to load more of the tree).
# For part (a), we need to find all leaves in this upper cut that are descended
# from this triggering node.  The easiest way is to use branch on this upper cut:
b <- branch(up$hc, merge.id = 11) # notice we use the merge id from the upper cut
# now we get the merge ids (relative to the original tree) of these descendants:
merge.id.to.load <- up$merge.id.leaf[up$hc$labels %in% b$labels]
# and then for part (b), for each index (that's not NA, since these are leaves 
# in original tree) we'd call branch and upper cut and add this to our list of
# loaded upper cuts...
up2 <- upper(branch(hc, merge.id.to.load[1]), k = load_depth)

# thoughts about data structures:
loaded <- list()
loaded[[n-1]] <- upper(hc, k = load_depth) # upper cut of original tree is "up" from above
# loaded: a list of upper cuts of length n - 1, where loaded[[m]] would be the upper cut that
# has root with merge.id m in the original tree.
depths <- rep(0, n - 1)
# depths[m] has the depth of the node having merge.id in original tree
# this is updated in step (c).
