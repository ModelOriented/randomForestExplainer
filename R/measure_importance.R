#' Importance of variables in a random forest
#'
#' Get a data frame with various measures of importance of variables in a random forest
#'
#' @param forest A random forest produced by the function randomForest with option localImp = TRUE
#' @param mean_sample The sample of trees on which mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"
#'
#' @return A data frame with rows corresponding to variables and columns to various measures of importance of variables
#'
#' @import dplyr
#' @importFrom data.table rbindlist
#'
#' @examples
#' measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE))
#'
#' @export
measure_importance <- function(forest, mean_sample = "top_trees"){
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>% rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  importance_frame <- get_min_depth_means(min_depth_frame, min_depth_count(min_depth_frame), mean_sample)
  colnames(importance_frame)[2] <- "mean_min_depth"
  importance_frame$variable <- as.character(importance_frame$variable)
  nodes_occurrences <- dplyr::group_by(forest_table, `split var`) %>%
    dplyr::summarize(n())
  colnames(nodes_occurrences) <- c("variable", "no_of_nodes")
  nodes_occurrences <- as.data.frame(nodes_occurrences[!is.na(nodes_occurrences$variable),])
  nodes_occurrences$variable <- as.character(nodes_occurrences$variable)
  if(forest$type == "classification"){
    vimp_frame <- as.data.frame(forest$importance[, c("MeanDecreaseAccuracy", "MeanDecreaseGini")])
    colnames(vimp_frame) <- c("accuracy_decrease", "gini_decrease")
  } else if(forest$type =="regression"){
    vimp_frame <- as.data.frame(forest$importance)
    colnames(vimp_frame) <- c("mse_increase", "node_purity_increase")
  }
  vimp_frame$variable <- rownames(vimp_frame)
  trees_occurrences <- dplyr::group_by(min_depth_frame, variable) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  colnames(trees_occurrences)[2] <- "no_of_trees"
  trees_occurrences$variable <- as.character(trees_occurrences$variable)
  root_count <- min_depth_frame[min_depth_frame$minimal_depth == 0, ] %>%
    dplyr::group_by(variable) %>% dplyr::summarize(count = n()) %>% as.data.frame()
  colnames(root_count)[2] <- "times_a_root"
  root_count$variable <- as.character(root_count$variable)
  importance_frame <- merge(importance_frame, nodes_occurrences, all = TRUE)
  importance_frame <- merge(importance_frame, vimp_frame, all = TRUE)
  importance_frame <- merge(importance_frame, trees_occurrences, all = TRUE)
  importance_frame <- merge(importance_frame, root_count, all = TRUE)
  importance_frame[is.na(importance_frame$no_of_nodes), c("no_of_nodes", "no_of_trees")] <- 0
  importance_frame[is.na(importance_frame$times_a_root), "times_a_root"] <- 0
  total_no_of_nodes <- sum(importance_frame$no_of_nodes)
  importance_frame$p_value <-
    unlist(lapply(importance_frame$no_of_nodes,
                  function(x) binom.test(x, total_no_of_nodes, 1/nrow(importance_frame),
                                         alternative = "greater")$p.value))
  importance_frame$variable <- as.factor(importance_frame$variable)
  return(importance_frame)
}

#' Extract k most important variables in a random forest
#'
#' Get the names of k variables with highest sum of rankings based on the specified importance measures
#'
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param k The number of variables to extract
#' @param ties_action One of three: c("none", "all", "draw"); specifies which variables to pick when ties occur. When set to "none" we may get less than k variables, when "all" whe may get more and "draw" makes us get exactly k.
#'
#' @return A character vector with names of k variables with highest sum of rankings
#'
#' @importFrom data.table frankv
#'
#' @examples
#' important_variables(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
important_variables <- function(importance_frame, k = 15, measures = names(importance_frame)[2:5],
                                ties_action = "all"){
  rankings <- data.frame(variable = importance_frame$variable, mean_min_depth =
                           frankv(importance_frame$mean_min_depth, ties.method = "dense"),
                         p_value =
                           frankv(importance_frame$p_value, ties.method = "dense"),
                         apply(importance_frame[, -c(1, 2, 8)], 2,
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
#' @param importance_frame A result of using the function measure_importance() to a random forest
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
#' plot_multi_way_importance(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_multi_way_importance <- function(importance_frame, x_measure = "mean_min_depth",
                                      y_measure = "times_a_root", size_measure = NULL,
                                      min_no_of_trees = 0, no_of_labels = 10,
                                      main = "Multi-way importance plot"){
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
        geom_point(data = data_for_labels, color = "black", stroke = 2, size = 3, aes(fill = "top"), shape = 21) +
        geom_label_repel(data = data_for_labels, aes(label = variable), show.legend = FALSE) +
        theme_bw() + scale_fill_discrete(name = "variable")
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
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param main A string to be used as title of the plot
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' plot_importance_ggpairs(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_importance_ggpairs <- function(importance_frame, measures =
                                      names(importance_frame)[c(2, 4, 5, 3, 7)],
                                    main = "Relations between measures of importance"){
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
#' @param importance_frame A result of using the function measure_importance() to a random forest
#' @param measures A character vector specifying the measures of importance to be used
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import GGally
#'
#' @examples
#' plot_importance_ggpairs(measure_importance(randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)))
#'
#' @export
plot_importance_rankings <- function(importance_frame, measures =
                                       names(importance_frame)[c(2, 4, 5, 3, 7)],
                                     main = "Relations between rankings according to different measures"){
  rankings <- data.frame(variable = importance_frame$variable, mean_min_depth =
                           frankv(importance_frame$mean_min_depth, ties.method = "dense"),
                         apply(importance_frame[, -c(1, 2)], 2,
                               function(x) frankv(x, order = -1, ties.method = "dense")))
  plot <- ggpairs(rankings[, measures], lower = list(continuous = function(data, mapping){
    ggplot(data = data, mapping = mapping) + geom_point() +  geom_smooth(method = loess)
    }))+ theme_bw()
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
