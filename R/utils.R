# Helpers to avoid warnings in computations
# Are all values NA?
all_na <- function(x) {
  if (!anyNA(x)) {
    return(FALSE)
  }
  all(is.na(x))
}
# Min but returns NA if only has NA
min_na <- function(x) {
  if (all_na(x)) {
    return(NA)
  }
  min(x, na.rm = TRUE)
}
# max but returns NA if only has NA
max_na <- function(x) {
  if (all_na(x)) {
    return(NA)
  }
  max(x, na.rm = TRUE)
}

utils::globalVariables(c("prediction", "variable"))

# Depth of each node of a single tree.
# The input is a matrix with left and right child nodes in 1:nrow(childs).
tree_depth <- function(childs) {
  n <- nrow(childs)
  depth <- rep(NA, times = n)
  j <- depth[1L] <- 0
  ix <- 1L  # current nodes, initialized with root node index

  # j loops over tree depth
  while(anyNA(depth) && j < n) {  # The second condition is never used
    ix <- as.integer(childs[ix, ])
    ix <- ix[!is.na(ix)]  # leaf nodes do not have childs
    j <- j + 1
    depth[ix] <- j
  }

  return(depth)
}

# Unifies the getTree() and treeInfo() functions and calculates tree depth.
# Returns df with tree id, node id, split variable, child ids, and depth
tree2df <- function(x, k = 1) {
  stopifnot(inherits(x, c("randomForest", "ranger")))
  if (inherits(x, "randomForest")) {
    M <- randomForest::getTree(x, k = k)[, 1:3, drop = FALSE]
    M[M == 0] <- NA
    v <- rownames(x[["importance"]])[M[, 3L]]  # as in getTree()
    childs <- M[, 1:2, drop = FALSE]
  } else { # ranger
    df <- ranger::treeInfo(x, tree = k)
    v <- df[["splitvarName"]]
    childs <- as.matrix(df[, c("leftChild", "rightChild")]) + 1  # zero based
  }
  depth <- tree_depth(childs)
  data.table::data.table(
    tree = k,
    number = seq_along(v),
    variable = v,
    left_child = childs[, 1L],
    right_child = childs[, 2L],
    depth = depth,
    check.names = FALSE
  )
}

# How many trees does the forest have?
ntrees <- function(x) {
  stopifnot(inherits(x, c("randomForest", "ranger")))
  if (inherits(x, "randomForest")) x$ntree else x$num.trees
}

# Helper function that extracts feature names from fitted random forest
# Used in plot_predict_interaction()
get_feature_names <- function(x) {
  stopifnot(inherits(x, c("randomForest", "ranger")))
  if (inherits(x, "randomForest")) {
    rownames(x[["importance"]])
  } else { # ranger
    x[[c("forest", "independent.variable.names")]]
  }
}

# Applies tree2df() to each tree and stacks the results
forest2df <- function(x) {
  rbindlist(lapply(seq_len(ntrees(x)), function(i) tree2df(x, i)))
}
