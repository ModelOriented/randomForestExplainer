# Calculate mean minimal depth using mean_sample from a properly prepared min_depth_frame
measure_min_depth <- function(min_depth_frame, mean_sample){
  frame <- get_min_depth_means(min_depth_frame, min_depth_count(min_depth_frame), mean_sample)
  colnames(frame)[2] <- "mean_min_depth"
  frame$variable <- as.character(frame$variable)
  return(frame)
}

# Calculate the number of nodes split on each variable for a data frame with the whole forest
# randomForest
measure_no_of_nodes <- function(forest_table){
  `split var` <- NULL
  frame <- dplyr::group_by(forest_table, `split var`) %>% dplyr::summarize(n())
  colnames(frame) <- c("variable", "no_of_nodes")
  frame <- as.data.frame(frame[!is.na(frame$variable),])
  frame$variable <- as.character(frame$variable)
  return(frame)
}

# Calculate the number of nodes split on each variable for a data frame with the whole forest
# randomForest
measure_no_of_nodes_ranger <- function(forest_table){
  splitvarName <- NULL
  frame <- dplyr::group_by(forest_table, splitvarName) %>% dplyr::summarize(n())
  colnames(frame) <- c("variable", "no_of_nodes")
  frame <- as.data.frame(frame[!is.na(frame$variable),])
  frame$variable <- as.character(frame$variable)
  return(frame)
}

# Extract randomForest variable importance measures
# randomForest
measure_vimp <- function(forest, only_nonlocal = FALSE){
  if(forest$type %in% c("classification", "unsupervised")){
    if(dim(forest$importance)[2] == 1){
      if(only_nonlocal == FALSE){
        print("Warning: your forest does not contain information on local importance so 'accuracy_decrease' measure cannot be extracted. To add it regrow the forest with the option localImp = TRUE and run this function again.")
      }
      frame <- as.data.frame(forest$importance[, "MeanDecreaseGini"])
      colnames(frame) <- "gini_decrease"
    } else {
      frame <- as.data.frame(forest$importance[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")])
      colnames(frame) <- c("accuracy_decrease", "gini_decrease")
    }
  } else if(forest$type =="regression"){
    frame <- as.data.frame(forest$importance)
    if(dim(forest$importance)[2] == 1){
      if(only_nonlocal == FALSE){
        print("Warning: your forest does not contain information on local importance so 'mse_increase' measure cannot be extracted. To add it regrow the forest with the option localImp = TRUE and run this function again.")
      }
      colnames(frame) <- "node_purity_increase"
    } else{
      colnames(frame) <- c("mse_increase", "node_purity_increase")
    }
  }
  frame$variable <- rownames(frame)
  return(frame)
}

# Extract randomForest variable importance measures
# ranger
measure_vimp_ranger <- function(forest){
  if (forest$importance.mode == "none"){
    stop("No variable importance available, regenerate forest by ranger(..., importance='impurity').")
  }
  frame <- data.frame(importance=forest$variable.importance,
                      variable=names(forest$variable.importance),
                      stringsAsFactors = FALSE)
  colnames(frame)[1] <- forest$importance.mode
  # possible values are: impurity, 'impurity_corrected', 'permutation'.
  return(frame)
}

# Calculate the number of trees using each variable for splitting
measure_no_of_trees <- function(min_depth_frame){
  variable <- NULL
  frame <- dplyr::group_by(min_depth_frame, variable) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  colnames(frame)[2] <- "no_of_trees"
  frame$variable <- as.character(frame$variable)
  return(frame)
}

# Calculate the number of times each variable is split on the root node
measure_times_a_root <- function(min_depth_frame){
  variable <- NULL
  frame <- min_depth_frame[min_depth_frame$minimal_depth == 0, ] %>%
    dplyr::group_by(variable) %>% dplyr::summarize(count = n()) %>% as.data.frame()
  colnames(frame)[2] <- "times_a_root"
  frame$variable <- as.character(frame$variable)
  return(frame)
}

# Calculate p_value for an importance frame with "no_of_nodes" column
measure_p_value <- function(importance_frame){
  total_no_of_nodes <- sum(importance_frame$no_of_nodes)
  p_value <- unlist(lapply(importance_frame$no_of_nodes,
                           function(x) stats::binom.test(x, total_no_of_nodes, 1/nrow(importance_frame),
                                                         alternative = "greater")$p.value))
  return(p_value)
}

#' Importance of variables in a random forest
#'
#' Get a data frame with various measures of importance of variables in a random forest
#'
#' @param forest A random forest produced by the function randomForest with option localImp = TRUE
#' @param mean_sample The sample of trees on which mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"
#' @param measures A vector of names of importance measures to be calculated - if equal to NULL then all are calculated;
#' if "p_value" is to be calculated then "no_of_nodes" will be too. Suitable measures for \code{classification} forests are:
#' \code{mean_min_depth}, \code{accuracy_decrease}, \code{gini_decrease}, \code{no_of_nodes},
#' \code{times_a_root}. For \code{regression} forests choose from: \code{mean_min_depth},
#' \code{mse_increase}, \code{node_purity_increase}, \code{no_of_nodes}, \code{times_a_root}.
#'
#' @return A data frame with rows corresponding to variables and columns to various measures of importance of variables
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 300)
#' measure_importance(forest)
#'
#' @export
measure_importance <- function(forest, mean_sample = "top_trees", measures = NULL){
  UseMethod("measure_importance")
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
measure_importance.randomForest <- function(forest, mean_sample = "top_trees", measures = NULL){
  tree <- NULL; `split var` <- NULL; depth <- NULL
  if(is.null(measures)){
    if(forest$type %in% c("classification", "unsupervised")){
      measures <- c("mean_min_depth", "no_of_nodes", "accuracy_decrease",
                    "gini_decrease", "no_of_trees", "times_a_root", "p_value")
    } else if(forest$type =="regression"){
      measures <- c("mean_min_depth", "no_of_nodes", "mse_increase", "node_purity_increase",
                    "no_of_trees", "times_a_root", "p_value")
    }
  }
  if(("p_value" %in% measures) && !("no_of_nodes" %in% measures)){
    measures <- c(measures, "no_of_nodes")
  }
  importance_frame <- data.frame(variable = rownames(forest$importance), stringsAsFactors = FALSE)
  # Get objects necessary to calculate importance measures based on the tree structure
  if(any(c("mean_min_depth", "no_of_nodes", "no_of_trees", "times_a_root", "p_value") %in% measures)){
    if (is.null(forest$forest)) {
      stop("Make sure forest has been saved when calling randomForest by randomForest(..., keep.forest = TRUE).")
    }
    forest_table <-
      lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
               mutate_if(is.factor, as.character) %>%
               calculate_tree_depth() %>% cbind(tree = i)) %>% rbindlist()
    min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
      dplyr::summarize(min(depth))
    colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
    min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  }
  # Add each importance measure to the table (if it was requested)
  if("mean_min_depth" %in% measures){
    importance_frame <- merge(importance_frame, measure_min_depth(min_depth_frame, mean_sample), all = TRUE)
  }
  if("no_of_nodes" %in% measures){
    importance_frame <- merge(importance_frame, measure_no_of_nodes(forest_table), all = TRUE)
    importance_frame[is.na(importance_frame$no_of_nodes), "no_of_nodes"] <- 0
  }
  if(forest$type %in% c("classification", "unsupervised")){
    vimp <- c("accuracy_decrease", "gini_decrease")
  } else if(forest$type =="regression"){
    vimp <- c("mse_increase", "node_purity_increase")
  }
  if(all(vimp %in% measures)){
    importance_frame <- merge(importance_frame, measure_vimp(forest), all = TRUE)
  } else if (vimp[1] %in% measures){
    importance_frame <- merge(importance_frame, measure_vimp(forest)[, c("variable", vimp[1])], all = TRUE)
  } else if (vimp[2] %in% measures){
    importance_frame <- merge(importance_frame,
                              measure_vimp(forest, only_nonlocal = TRUE)[, c("variable", vimp[2])], all = TRUE)
  }
  if("no_of_trees" %in% measures){
    importance_frame <- merge(importance_frame, measure_no_of_trees(min_depth_frame), all = TRUE)
    importance_frame[is.na(importance_frame$no_of_trees), "no_of_trees"] <- 0
  }
  if("times_a_root" %in% measures){
    importance_frame <- merge(importance_frame, measure_times_a_root(min_depth_frame), all = TRUE)
    importance_frame[is.na(importance_frame$times_a_root), "times_a_root"] <- 0
  }
  if("p_value" %in% measures){
    importance_frame$p_value <- measure_p_value(importance_frame)
    importance_frame$variable <- as.factor(importance_frame$variable)
  }
  return(importance_frame)
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
measure_importance.ranger <- function(forest, mean_sample = "top_trees", measures = NULL){
  tree <- NULL; splitvarName <- NULL; depth <- NULL
  if(is.null(measures)){
    measures <- c("mean_min_depth", "no_of_nodes", forest$importance.mode, "no_of_trees", "times_a_root", "p_value")
  }
  if(("p_value" %in% measures) && !("no_of_nodes" %in% measures)){
    measures <- c(measures, "no_of_nodes")
  }
  importance_frame <- data.frame(variable = names(forest$variable.importance), stringsAsFactors = FALSE)
  # Get objects necessary to calculate importance measures based on the tree structure
  if(any(c("mean_min_depth", "no_of_nodes", "no_of_trees", "times_a_root", "p_value") %in% measures)){
    forest_table <-
      lapply(1:forest$num.trees, function(i) ranger::treeInfo(forest, tree = i) %>%
               calculate_tree_depth_ranger() %>% cbind(tree = i)) %>% rbindlist()
    min_depth_frame <- dplyr::group_by(forest_table, tree, splitvarName) %>%
      dplyr::summarize(min(depth))
    colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
    min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  }
  # Add each importance measure to the table (if it was requested)
  if("mean_min_depth" %in% measures){
    importance_frame <- merge(importance_frame, measure_min_depth(min_depth_frame, mean_sample), all = TRUE)
  }
  if("no_of_nodes" %in% measures){
    importance_frame <- merge(importance_frame, measure_no_of_nodes_ranger(forest_table), all = TRUE)
    importance_frame[is.na(importance_frame$no_of_nodes), "no_of_nodes"] <- 0
  }
  if(forest$importance.mode %in% measures){
    importance_frame <- merge(importance_frame, measure_vimp_ranger(forest), all = TRUE)
  }
  if("no_of_trees" %in% measures){
    importance_frame <- merge(importance_frame, measure_no_of_trees(min_depth_frame), all = TRUE)
    importance_frame[is.na(importance_frame$no_of_trees), "no_of_trees"] <- 0
  }
  if("times_a_root" %in% measures){
    importance_frame <- merge(importance_frame, measure_times_a_root(min_depth_frame), all = TRUE)
    importance_frame[is.na(importance_frame$times_a_root), "times_a_root"] <- 0
  }
  if("p_value" %in% measures){
    importance_frame$p_value <- measure_p_value(importance_frame)
    importance_frame$variable <- as.factor(importance_frame$variable)
  }
  return(importance_frame)
}

#' Extract k most important variables in a random forest
#'
#' Get the names of k variables with highest sum of rankings based on the specified importance measures
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest or a randomForest object
#' @param measures A character vector specifying the measures of importance to be used
#' @param k The number of variables to extract
#' @param ties_action One of three: c("none", "all", "draw"); specifies which variables to pick when ties occur. When set to "none" we may get less than k variables, when "all" we may get more and "draw" makes us get exactly k.
#'
#' @return A character vector with names of k variables with highest sum of rankings
#'
#' @importFrom data.table frankv
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 300)
#' important_variables(measure_importance(forest), k = 2)
#'
#' @export
important_variables <- function(importance_frame, k = 15,
                                measures = names(importance_frame)[2:min(5, ncol(importance_frame))],
                                ties_action = "all"){
  if("randomForest" %in% class(importance_frame)){
    importance_frame <- measure_importance(importance_frame)
    if("predicted" %in% measures){
      measures <- names(importance_frame)[2:5]
    }
  } else if ("ranger" %in% class(importance_frame)){
    importance_frame <- measure_importance(importance_frame)
  }
  rankings <- data.frame(variable = importance_frame$variable, mean_min_depth =
                           frankv(importance_frame$mean_min_depth, ties.method = "dense"),
                         p_value =
                           frankv(importance_frame$p_value, ties.method = "dense"),
                         apply(importance_frame[, !colnames(importance_frame) %in% c("variable", "mean_min_depth", "p_value")], 2,
                               function(x) frankv(x, order = -1, ties.method = "dense")))
  rankings$index <- rowSums(rankings[, measures])
  vars <- as.character(rankings[order(rankings$index), "variable"])[1:min(k, nrow(rankings))]
  if(length(rankings[rankings$index == rankings[rankings$variable == vars[length(vars)], "index"], "index"]) > 1 &
     length(rankings[rankings$index <= rankings[rankings$variable == vars[length(vars)], "index"], "variable"]) > k){
    draw <- rankings[rankings$index == rankings[rankings$variable == vars[length(vars)], "index"], ]
    if(ties_action == "none"){
      vars <- as.character(rankings[rankings$index < draw$index[1], "variable"])
    } else if(ties_action == "all"){
      vars <- as.character(rankings[rankings$index <= draw$index[1], "variable"])
    } else if(ties_action == "draw"){
      vars <- as.character(rankings[rankings$index < draw$index[1], "variable"])
      vars <- c(vars, sample(as.character(draw$variable), size = k - length(vars)))
    }
  }
  return(vars)
}

#' Multi-way importance plot
#'
#' Plot two or three measures of importance of variables in a random fores. Choose importance measures from the colnames(importance_frame).
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest or a randomForest object
#' @param x_measure The measure of importance to be shown on the X axis
#' @param y_measure The measure of importance to be shown on the Y axis
#' @param size_measure The measure of importance to be shown as size of points (optional)
#' @param min_no_of_trees The minimal number of trees in which a variable has to be used for splitting to be used for plotting
#' @param no_of_labels The approximate number of best variables (according to all measures plotted) to be labeled (more will be labeled in case of ties)
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import ggrepel
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)
#' plot_multi_way_importance(measure_importance(forest))
#'
#' @export
plot_multi_way_importance <- function(importance_frame, x_measure = "mean_min_depth",
                                      y_measure = "times_a_root", size_measure = NULL,
                                      min_no_of_trees = 0, no_of_labels = 10,
                                      main = "Multi-way importance plot"){
  variable <- NULL
  if(any(c("randomForest", "ranger") %in% class(importance_frame))){
    importance_frame <- measure_importance(importance_frame)
  }
  data <- importance_frame[importance_frame$no_of_trees > min_no_of_trees, ]
  data_for_labels <- importance_frame[importance_frame$variable %in%
                                        important_variables(importance_frame, k = no_of_labels,
                                                            measures = c(x_measure, y_measure, size_measure)), ]
  if(!is.null(size_measure)){
    if(size_measure == "p_value"){
      data$p_value <- cut(data$p_value, breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
                          labels = c("<0.01", "[0.01, 0.05)", "[0.05, 0.1)", ">=0.1"), right = FALSE)
      plot <- ggplot(data, aes_string(x = x_measure, y = y_measure)) +
        geom_point(aes_string(color = size_measure), size = 3) +
        geom_point(data = data_for_labels, color = "black", stroke = 2, aes(alpha = "top"), size = 3, shape = 21) +
        geom_label_repel(data = data_for_labels, aes(label = variable), show.legend = FALSE) +
        theme_bw() + scale_alpha_discrete(name = "variable", range = c(1, 1))
    } else {
      plot <- ggplot(data, aes_string(x = x_measure, y = y_measure, size = size_measure)) +
        geom_point(aes(colour = "black")) + geom_point(data = data_for_labels, aes(colour = "blue")) +
        geom_label_repel(data = data_for_labels, aes(label = variable, size = NULL), show.legend = FALSE) +
        scale_colour_manual(name = "variable", values = c("black", "blue"), labels = c("non-top", "top")) +
        theme_bw()
      if(size_measure == "mean_min_depth"){
        plot <- plot + scale_size(trans = "reverse")
      }
    }
  } else {
    plot <- ggplot(data, aes_string(x = x_measure, y = y_measure)) +
      geom_point(aes(colour = "black")) + geom_point(data = data_for_labels, aes(colour = "blue")) +
      geom_label_repel(data = data_for_labels, aes(label = variable, size = NULL), show.legend = FALSE) +
      scale_colour_manual(name = "variable", values = c("black", "blue"), labels = c("non-top", "top")) +
      theme_bw()
  }
  if(x_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root")){
    plot <- plot + scale_x_sqrt()
  } else if(y_measure %in% c("no_of_nodes", "no_of_trees", "times_a_root")){
    plot <- plot + scale_y_sqrt()
  }
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' Plot importance measures with ggpairs
#'
#' Plot selected measures of importance of variables in a forest using ggpairs
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest or a randomForest object
#' @param measures A character vector specifying the measures of importance to be used
#' @param main A string to be used as title of the plot
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 200)
#' frame <- measure_importance(forest, measures = c("mean_min_depth", "times_a_root"))
#' plot_importance_ggpairs(frame, measures = c("mean_min_depth", "times_a_root"))
#'
#' @export
plot_importance_ggpairs <- function(importance_frame, measures = NULL,
                                    main = "Relations between measures of importance"){
  if(any(c("randomForest", "ranger") %in% class(importance_frame))){
    importance_frame <- measure_importance(importance_frame)
  }
  if (is.null(measures)){
    default_measures <- c("gini_decrease", "node_purity_increase", # randomForest
                          "impurity", "impurity_corrected", "permutation", # ranger
                          "mean_min_depth", "no_of_trees", "no_of_nodes", "p_value")
    measures <- intersect(default_measures, colnames(importance_frame))
  }
  plot <- ggpairs(importance_frame[, measures]) + theme_bw()
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' Plot importance measures rankings with ggpairs
#'
#' Plot against each other rankings of variables according to various measures of importance
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest or a randomForest object
#' @param measures A character vector specifying the measures of importance to be used.
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 300)
#' frame <- measure_importance(forest, measures = c("mean_min_depth", "times_a_root"))
#' plot_importance_ggpairs(frame, measures = c("mean_min_depth", "times_a_root"))
#'
#' @export
plot_importance_rankings <- function(importance_frame, measures = NULL,
                                     main = "Relations between rankings according to different measures"){
  if(any(c("randomForest", "ranger") %in% class(importance_frame))){
    importance_frame <- measure_importance(importance_frame)
  }
  if (is.null(measures)){
    default_measures <- c("gini_decrease", "node_purity_increase", # randomForest
                          "impurity", "impurity_corrected", "permutation", # ranger
                          "mean_min_depth", "no_of_trees", "no_of_nodes", "p_value")
    measures <- intersect(default_measures, colnames(importance_frame))
  }
  rankings <- data.frame(variable = importance_frame$variable,
                         apply(importance_frame[, !colnames(importance_frame) %in% c("variable", "mean_min_depth", "p_value")], 2,
                               function(x) frankv(x, order = -1, ties.method = "dense")))
  if ("mean_min_depth" %in% measures){
    rankings$mean_min_depth = frankv(importance_frame$mean_min_depth, ties.method = "dense")
  }
  if ("p_value" %in% measures){
    rankings$p_value = frankv(importance_frame$p_value, ties.method = "dense")
  }
  plot <- ggpairs(rankings[, measures], lower = list(continuous = function(data, mapping){
    ggplot(data = data, mapping = mapping) + geom_point() +  geom_smooth(method = "loess")
  })) + theme_bw()
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
