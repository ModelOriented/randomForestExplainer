# Calculate the depth of each node in a single tree obtained from a forest with randomForest::getTree
calculate_tree_depth <- function(frame){
  if(!all(c("right daughter", "left daughter") %in% names(frame))){
    stop("The data frame has to contain columns called 'right daughter' and 'left daughter'!
          It should be a product of the function getTree(..., labelVar = T).")
  }
  frame$depth <- NA
  frame$depth[1] <- 0
  for(i in 2:nrow(frame)){
    frame[i, "depth"] <-
      frame[frame[, "left daughter"] == as.numeric(rownames(frame[i,])) |
            frame[, "right daughter"] == as.numeric(rownames(frame[i,])), "depth"] + 1
  }
  return(frame)
}

# Calculate the depth of each node in a single tree obtained from a forest with ranger::treeInfo
calculate_tree_depth_ranger <- function(frame){
  if(!all(c("rightChild", "leftChild") %in% names(frame))){
    stop("The data frame has to contain columns called 'rightChild' and 'leftChild'!
         It should be a product of the function ranger::treeInfo().")
  }
  frame$depth <- NA
  frame$depth[1] <- 0
  for(i in 2:nrow(frame)){
    frame[i, "depth"] <-
      frame[(!is.na(frame[, "leftChild"]) & frame[, "leftChild"] == frame[i, "nodeID"]) |
              (!is.na(frame[, "rightChild"]) & frame[, "rightChild"] == frame[i, "nodeID"]), "depth"] + 1
  }
  return(frame)
}

#' Calculate minimal depth distribution of a random forest
#'
#' Get minimal depth values for all trees in a random forest
#'
#' @param forest A randomForest or ranger object
#'
#' @return A data frame with the value of minimal depth for every variable in every tree
#'
#' @examples
#' min_depth_distribution(randomForest::randomForest(Species ~ ., data = iris, ntree = 100))
#' min_depth_distribution(ranger::ranger(Species ~ ., data = iris, num.trees = 100))
#'
#' @export
min_depth_distribution <- function(forest){
  UseMethod("min_depth_distribution")
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
min_depth_distribution.randomForest <- function(forest){
  tree <- NULL; `split var` <- NULL; depth <- NULL
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             mutate_if(is.factor, as.character) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>% rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  return(min_depth_frame)
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
min_depth_distribution.ranger <- function(forest){
  tree <- NULL; splitvarName <- NULL; depth <- NULL
  forest_table <-
    lapply(1:forest$num.trees, function(i) ranger::treeInfo(forest, tree = i) %>%
             calculate_tree_depth_ranger() %>% cbind(tree = i)) %>% rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, splitvarName) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  return(min_depth_frame)
}

# Count the trees in which each variable had a given minimal depth
min_depth_count <- function(min_depth_frame){
  tree <- NULL; minimal_depth <- NULL; variable <- NULL
  mean_tree_depth <- dplyr::group_by(min_depth_frame, tree) %>%
    dplyr::summarize(depth = max(minimal_depth) + 1) %>% as.data.frame()
  mean_tree_depth <- mean(mean_tree_depth$depth)
  min_depth_count <- dplyr::group_by(min_depth_frame, variable, minimal_depth) %>%
    dplyr::summarize(count = n()) %>% as.data.frame()
  occurrences <- stats::aggregate(count ~ variable, data = min_depth_count, sum)
  colnames(occurrences)[2] <- "no_of_occurrences"
  min_depth_count <-
    data.frame(variable = occurrences$variable, minimal_depth = NA,
               count = max(min_depth_frame$tree) - occurrences$no_of_occurrences) %>%
    rbind(min_depth_count)
  min_depth_count <- min_depth_count[order(min_depth_count$variable, min_depth_count$minimal_depth),]
  rownames(min_depth_count) <- 1:nrow(min_depth_count)
  return(list(min_depth_count, occurrences, mean_tree_depth))
}

# Get a data frame with means of minimal depth calculated using sample = c("all_trees", "top_trees", "relevant_trees")
get_min_depth_means <- function(min_depth_frame, min_depth_count_list, mean_sample){
  .SD <- NULL; variable <- NULL
  if(mean_sample == "all_trees"){
    min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "minimal_depth"] <- min_depth_count_list[[3]]
    min_depth_means <-
      data.table::as.data.table(min_depth_count_list[[1]])[, stats::weighted.mean(.SD[["minimal_depth"]], .SD[["count"]]),
                                               by = variable] %>% as.data.frame()
  } else if(mean_sample == "top_trees"){
    min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"] <-
      min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"] -
      min(min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "count"])
    min_depth_count_list[[1]][is.na(min_depth_count_list[[1]]$minimal_depth), "minimal_depth"] <- min_depth_count_list[[3]]
    min_depth_means <-
      data.table::as.data.table(min_depth_count_list[[1]])[, stats::weighted.mean(.SD[["minimal_depth"]], .SD[["count"]]),
                                               by = variable] %>% as.data.frame()
  } else if(mean_sample == "relevant_trees"){
    min_depth_means <- stats::aggregate(minimal_depth ~ variable, data = min_depth_frame, mean)
  }
  colnames(min_depth_means)[2] <- "mean_minimal_depth"
  return(min_depth_means)
}

#' Plot the distribution of minimal depth in a random forest
#'
#' @param min_depth_frame A data frame output of min_depth_distribution function or a randomForest object
#' @param k The maximal number of variables with lowest mean minimal depth to be used for plotting
#' @param min_no_of_trees The minimal number of trees in which a variable has to be used for splitting to be used for plotting
#' @param mean_sample The sample of trees on which mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"
#' @param mean_scale Logical: should the values of mean minimal depth be rescaled to the interval [0,1]?
#' @param mean_round The number of digits used for displaying mean minimal depth
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 300)
#' plot_min_depth_distribution(min_depth_distribution(forest))
#'
#' @export
plot_min_depth_distribution <- function(min_depth_frame, k = 10, min_no_of_trees = 0,
                                        mean_sample = "top_trees", mean_scale = FALSE, mean_round = 2,
                                        main = "Distribution of minimal depth and its mean"){
  minimal_depth <- NULL; mean_minimal_depth_label <- NULL; mean_minimal_depth <- NULL
  if(any(c("randomForest", "ranger") %in% class(min_depth_frame))){
    min_depth_frame <- min_depth_distribution(min_depth_frame)
  }
  min_depth_count_list <- min_depth_count(min_depth_frame)
  min_depth_means <- get_min_depth_means(min_depth_frame, min_depth_count_list, mean_sample)
  frame_with_means <- merge(min_depth_count_list[[1]], min_depth_means)
  frame_with_means[is.na(frame_with_means$minimal_depth), "count"] <-
    frame_with_means[is.na(frame_with_means$minimal_depth), "count"] -
    min(frame_with_means[is.na(frame_with_means$minimal_depth), "count"])
  if(mean_scale == TRUE){
    frame_with_means$mean_minimal_depth <-
      (frame_with_means$mean_minimal_depth - min(frame_with_means$mean_minimal_depth))/
      (max(frame_with_means$mean_minimal_depth) - min(frame_with_means$mean_minimal_depth))
  }
  frame_with_means$mean_minimal_depth_label <-
    (frame_with_means$mean_minimal_depth - min(frame_with_means$mean_minimal_depth))/
    (max(frame_with_means$mean_minimal_depth) - min(frame_with_means$mean_minimal_depth)) *
    max(min_depth_count_list[[2]]$no_of_occurrences)
  variables <- min_depth_count_list[[2]][min_depth_count_list[[2]]$no_of_occurrences >= min_no_of_trees, "variable"]
  frame_with_means <- frame_with_means[frame_with_means$variable %in% variables, ]
  frame_with_means <-
    within(frame_with_means, variable <-
             factor(variable, levels = unique(frame_with_means[order(frame_with_means$mean_minimal_depth), "variable"])))
  data <- frame_with_means[frame_with_means$variable %in% levels(frame_with_means$variable)[
    1:min(k, length(unique(frame_with_means$variable)))], ]
  data$variable <- droplevels(data$variable)
  data_for_labels <- unique(data[, c("variable", "mean_minimal_depth", "mean_minimal_depth_label")])
  data_for_labels$mean_minimal_depth <- round(data_for_labels$mean_minimal_depth, digits = mean_round)
  plot <- ggplot(data, aes(x = variable, y = count)) +
    geom_col(position = position_stack(reverse = TRUE), aes(fill = as.factor(minimal_depth))) + coord_flip() +
    scale_x_discrete(limits = rev(levels(data$variable))) +
    geom_errorbar(aes(ymin = mean_minimal_depth_label, ymax = mean_minimal_depth_label), size = 1.5) +
    xlab("Variable") + ylab("Number of trees") + guides(fill = guide_legend(title = "Minimal depth")) +
    theme_bw() + geom_label(data = data_for_labels,
                            aes(y = mean_minimal_depth_label, label = mean_minimal_depth))
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
