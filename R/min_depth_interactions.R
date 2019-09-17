# Calculate conditional depth in a tree with respect to all variables from vector vars
# randomForest
conditional_depth <- function(frame, vars){
  `.SD` <- NULL; depth <- NULL; `split var` <- NULL
  index <- data.table::as.data.table(frame)[, .SD[which.min(depth), "number"], by = `split var`]
  index <- index[!is.na(index$`split var`), ]
  if(any(index$`split var` %in% vars)){
    for(j in vars){
      begin <- as.numeric(index[index$`split var` == j, "number"])
      if(!is.na(begin)){
        df <- frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))]
        df[[j]][1] <- 0
        for(k in 2:nrow(df)){
          if(length(df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                       df[, "right daughter"] == as.numeric(df[k, "number"]), j]) != 0){
            df[k, j] <-
              df[df[, "left daughter"] == as.numeric(df[k, "number"]) |
                   df[, "right daughter"] == as.numeric(df[k, "number"]), j] + 1
          }
        }
        frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))] <- df
      }
    }
  }
  frame[frame == 0] <- NA
  return(frame)
}

# Calculate conditional depth in a tree with respect to all variables from vector vars
# ranger
conditional_depth_ranger <- function(frame, vars){
  `.SD` <- NULL; depth <- NULL; splitvarName <- NULL
  index <- data.table::as.data.table(frame)[, .SD[which.min(depth), "number"], by = splitvarName]
  index <- index[!is.na(index$splitvarName), ]
  if(any(index$splitvarName %in% vars)){
    for(j in vars){
      begin <- as.numeric(index[index$splitvarName == j, "number"])
      if(!is.na(begin)){
        df <- frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))]
        df[[j]][1] <- 0
        for(k in 2:nrow(df)){
          if(length(df[(!is.na(df[, "leftChild"]) & df[, "leftChild"] == as.numeric(df[k, "number"]) - 1) |
                       (!is.na(df[, "rightChild"]) & df[, "rightChild"] == as.numeric(df[k, "number"]) - 1), j]) != 0){
            df[k, j] <-
              df[(!is.na(df[, "leftChild"]) & df[, "leftChild"] == as.numeric(df[k, "number"]) - 1) |
                   (!is.na(df[, "rightChild"]) & df[, "rightChild"] == as.numeric(df[k, "number"]) - 1), j] + 1
          }
        }
        frame[begin:nrow(frame), setdiff(names(frame), setdiff(vars, j))] <- df
      }
    }
  }
  frame[frame == 0] <- NA
  return(frame)
}

# Get a data frame with values of minimal depth conditional on selected variables for the whole forest
# randomForest
min_depth_interactions_values <- function(forest, vars){
  `.` <- NULL; .SD <- NULL; tree <- NULL; `split var` <- NULL
  interactions_frame <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             mutate_if(is.factor, as.character) %>%
             calculate_tree_depth() %>% cbind(., tree = i, number = 1:nrow(.))) %>%
    data.table::rbindlist() %>% as.data.frame()
  interactions_frame[vars] <- as.numeric(NA)
  interactions_frame <-
    data.table::as.data.table(interactions_frame)[, conditional_depth(as.data.frame(.SD), vars), by = tree] %>% as.data.frame()
  mean_tree_depth <- dplyr::group_by(interactions_frame[, c("tree", vars)], tree) %>%
    dplyr::summarize_at(vars, funs(max(., na.rm = TRUE))) %>% as.data.frame()
  mean_tree_depth[mean_tree_depth == -Inf] <- NA
  mean_tree_depth <- colMeans(mean_tree_depth[, vars, drop = FALSE], na.rm = TRUE)
  min_depth_interactions_frame <-
    interactions_frame %>% dplyr::group_by(tree, `split var`) %>%
    dplyr::summarize_at(vars, funs(min(., na.rm = TRUE))) %>% as.data.frame()
  min_depth_interactions_frame[min_depth_interactions_frame == Inf] <- NA
  min_depth_interactions_frame <- min_depth_interactions_frame[!is.na(min_depth_interactions_frame$`split var`), ]
  colnames(min_depth_interactions_frame)[2] <- "variable"
  min_depth_interactions_frame[, -c(1:2)] <- min_depth_interactions_frame[, -c(1:2)] - 1
  return(list(min_depth_interactions_frame, mean_tree_depth))
}

# Get a data frame with values of minimal depth conditional on selected variables for the whole forest
# ranger
min_depth_interactions_values_ranger <- function(forest, vars){
  `.` <- NULL; .SD <- NULL; tree <- NULL; splitvarName <- NULL
  interactions_frame <-
    lapply(1:forest$num.trees, function(i) ranger::treeInfo(forest, tree = i) %>%
             calculate_tree_depth_ranger() %>% cbind(., tree = i, number = 1:nrow(.))) %>%
    data.table::rbindlist() %>% as.data.frame()
  interactions_frame[vars] <- as.numeric(NA)
  interactions_frame <-
    data.table::as.data.table(interactions_frame)[, conditional_depth_ranger(as.data.frame(.SD), vars), by = tree] %>% as.data.frame()
  mean_tree_depth <- dplyr::group_by(interactions_frame[, c("tree", vars)], tree) %>%
    dplyr::summarize_at(vars, funs(max(., na.rm = TRUE))) %>% as.data.frame()
  mean_tree_depth[mean_tree_depth == -Inf] <- NA
  mean_tree_depth <- colMeans(mean_tree_depth[, vars, drop = FALSE], na.rm = TRUE)
  min_depth_interactions_frame <-
    interactions_frame %>% dplyr::group_by(tree, splitvarName) %>%
    dplyr::summarize_at(vars, funs(min(., na.rm = TRUE))) %>% as.data.frame()
  min_depth_interactions_frame[min_depth_interactions_frame == Inf] <- NA
  min_depth_interactions_frame <- min_depth_interactions_frame[!is.na(min_depth_interactions_frame$splitvarName), ]
  colnames(min_depth_interactions_frame)[2] <- "variable"
  min_depth_interactions_frame[, -c(1:2)] <- min_depth_interactions_frame[, -c(1:2)] - 1
  return(list(min_depth_interactions_frame, mean_tree_depth))
}

#' Calculate mean conditional minimal depth
#'
#' Calculate mean conditional minimal depth with respect to a vector of variables
#'
#' @param forest A randomForest object
#' @param vars A character vector with variables with respect to which conditional minimal depth will be calculated; by default it is extracted by the important_variables function but this may be time consuming
#' @param mean_sample The sample of trees on which conditional mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"
#' @param uncond_mean_sample The sample of trees on which unconditional mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"
#'
#' @return A data frame with each observation giving the means of conditional minimal depth and the size of sample for a given interaction
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
#' min_depth_interactions(forest, c("Petal.Width", "Petal.Length"))
#'
#' @export
min_depth_interactions <- function(forest, vars = important_variables(measure_importance(forest)),
                                   mean_sample = "top_trees", uncond_mean_sample = mean_sample){
  UseMethod("min_depth_interactions")
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
min_depth_interactions.randomForest <- function(forest, vars = important_variables(measure_importance(forest)),
                                                mean_sample = "top_trees", uncond_mean_sample = mean_sample){
  variable <- NULL; `.` <- NULL; tree <- NULL; `split var` <- NULL; depth <- NULL
  min_depth_interactions_frame <- min_depth_interactions_values(forest, vars)
  mean_tree_depth <- min_depth_interactions_frame[[2]]
  min_depth_interactions_frame <- min_depth_interactions_frame[[1]]
  interactions_frame <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_at(vars, funs(mean(., na.rm = TRUE))) %>% as.data.frame()
  interactions_frame[is.na(as.matrix(interactions_frame))] <- NA
  occurrences <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_at(vars, funs(sum(!is.na(.)))) %>% as.data.frame()
  if(mean_sample == "all_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- forest$ntree - occurrences[, -1]
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))/forest$ntree
  } else if(mean_sample == "top_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- forest$ntree - occurrences[, -1]
    minimum_non_occurrences <- min(non_occurrences[, -1])
    non_occurrences[, -1] <- non_occurrences[, -1] - minimum_non_occurrences
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))/(forest$ntree - minimum_non_occurrences)
  }
  interactions_frame <- reshape2::melt(interactions_frame, id.vars = "variable")
  colnames(interactions_frame)[2:3] <- c("root_variable", "mean_min_depth")
  occurrences <- reshape2::melt(occurrences, id.vars = "variable")
  colnames(occurrences)[2:3] <- c("root_variable", "occurrences")
  interactions_frame <- merge(interactions_frame, occurrences)
  interactions_frame$interaction <- paste(interactions_frame$root_variable, interactions_frame$variable, sep = ":")
  forest_table <-
    lapply(1:forest$ntree, function(i) randomForest::getTree(forest, k = i, labelVar = T) %>%
             mutate_if(is.factor, as.character) %>%
             calculate_tree_depth() %>% cbind(tree = i)) %>% rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, `split var`) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  importance_frame <- get_min_depth_means(min_depth_frame, min_depth_count(min_depth_frame), uncond_mean_sample)
  colnames(importance_frame)[2] <- "uncond_mean_min_depth"
  interactions_frame <- merge(interactions_frame, importance_frame)
}

#' @import dplyr
#' @importFrom data.table rbindlist
#' @export
min_depth_interactions.ranger <- function(forest, vars = important_variables(measure_importance(forest)),
                                          mean_sample = "top_trees", uncond_mean_sample = mean_sample){
  variable <- NULL; `.` <- NULL; tree <- NULL; splitvarName <- NULL; depth <- NULL
  min_depth_interactions_frame <- min_depth_interactions_values_ranger(forest, vars)
  mean_tree_depth <- min_depth_interactions_frame[[2]]
  min_depth_interactions_frame <- min_depth_interactions_frame[[1]]
  interactions_frame <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_at(vars, funs(mean(., na.rm = TRUE))) %>% as.data.frame()
  interactions_frame[is.na(as.matrix(interactions_frame))] <- NA
  occurrences <-
    min_depth_interactions_frame %>% dplyr::group_by(variable) %>%
    dplyr::summarize_at(vars, funs(sum(!is.na(.)))) %>% as.data.frame()
  if(mean_sample == "all_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- forest$num.trees - occurrences[, -1]
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))/forest$num.trees
  } else if(mean_sample == "top_trees"){
    non_occurrences <- occurrences
    non_occurrences[, -1] <- forest$num.trees - occurrences[, -1]
    minimum_non_occurrences <- min(non_occurrences[, -1])
    non_occurrences[, -1] <- non_occurrences[, -1] - minimum_non_occurrences
    interactions_frame[is.na(as.matrix(interactions_frame))] <- 0
    interactions_frame[, -1] <- (interactions_frame[, -1] * occurrences[, -1] +
                                   as.matrix(non_occurrences[, -1]) %*% diag(mean_tree_depth, nrow = length(mean_tree_depth)))/(forest$num.trees - minimum_non_occurrences)
  }
  interactions_frame <- reshape2::melt(interactions_frame, id.vars = "variable")
  colnames(interactions_frame)[2:3] <- c("root_variable", "mean_min_depth")
  occurrences <- reshape2::melt(occurrences, id.vars = "variable")
  colnames(occurrences)[2:3] <- c("root_variable", "occurrences")
  interactions_frame <- merge(interactions_frame, occurrences)
  interactions_frame$interaction <- paste(interactions_frame$root_variable, interactions_frame$variable, sep = ":")
  forest_table <-
    lapply(1:forest$num.trees, function(i) ranger::treeInfo(forest, tree = i) %>%
             calculate_tree_depth_ranger() %>% cbind(tree = i)) %>% rbindlist()
  min_depth_frame <- dplyr::group_by(forest_table, tree, splitvarName) %>%
    dplyr::summarize(min(depth))
  colnames(min_depth_frame) <- c("tree", "variable", "minimal_depth")
  min_depth_frame <- as.data.frame(min_depth_frame[!is.na(min_depth_frame$variable),])
  importance_frame <- get_min_depth_means(min_depth_frame, min_depth_count(min_depth_frame), uncond_mean_sample)
  colnames(importance_frame)[2] <- "uncond_mean_min_depth"
  interactions_frame <- merge(interactions_frame, importance_frame)
}

#' Plot the top mean conditional minimal depth
#'
#' @param interactions_frame A data frame produced by the min_depth_interactions() function or a randomForest object
#' @param k The number of best interactions to plot, if set to NULL then all plotted
#' @param main A string to be used as title of the plot
#'
#' @return A ggplot2 object
#'
#' @import ggplot2
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
#' plot_min_depth_interactions(min_depth_interactions(forest, c("Petal.Width", "Petal.Length")))
#'
#' @export
plot_min_depth_interactions <- function(interactions_frame, k = 30,
                                        main = paste0("Mean minimal depth for ",
                                                      paste0(k, " most frequent interactions"))){
  mean_min_depth <- NULL; occurrences <- NULL; uncond_mean_min_depth <- NULL
  if(any(c("randomForest", "ranger") %in% class(interactions_frame))){
    interactions_frame <- min_depth_interactions(interactions_frame)
  }
  interactions_frame$interaction <- factor(interactions_frame$interaction, levels =
                                             interactions_frame[
                                               order(interactions_frame$occurrences, decreasing = TRUE), "interaction"])
  minimum <- min(interactions_frame$mean_min_depth, na.rm = TRUE)
  if(is.null(k)) k <- length(levels(interactions_frame$interaction))
  plot <- ggplot(interactions_frame[interactions_frame$interaction %in% levels(interactions_frame$interaction)[1:k] &
                                      !is.na(interactions_frame$mean_min_depth), ],
                 aes(x = interaction, y = mean_min_depth, fill = occurrences)) +
    geom_bar(stat = "identity") +
    geom_pointrange(aes(ymin = pmin(mean_min_depth, uncond_mean_min_depth), y = uncond_mean_min_depth,
                        ymax = pmax(mean_min_depth, uncond_mean_min_depth), shape = "unconditional"), fatten = 2, size = 1) +
    geom_hline(aes(yintercept = minimum, linetype = "minimum"), color = "red", size = 1.5) +
    scale_linetype_manual(name = NULL, values = 1) + theme_bw() +
    scale_shape_manual(name = NULL, values = 19) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' Plot the prediction of the forest for a grid of values of two numerical variables
#'
#' @param forest A randomForest or ranger object
#' @param data The data frame on which forest was trained
#' @param variable1 A character string with the name a numerical predictor that will on X-axis
#' @param variable2 A character string with the name a numerical predictor that will on Y-axis
#' @param grid The number of points on the one-dimensional grid on x and y-axis
#' @param main A string to be used as title of the plot
#' @param time A numeric value specifying the time at which to predict survival probability, only
#'  applies to survival forests. If not specified, the time closest to predicted median survival
#'  time is used
#'
#' @return A ggplot2 object
#'
#' @examples
#' forest <- randomForest::randomForest(Species ~., data = iris)
#' plot_predict_interaction(forest, iris, "Petal.Width", "Sepal.Width")
#' forest_ranger <- ranger::ranger(Species ~., data = iris)
#' plot_predict_interaction(forest, iris, "Petal.Width", "Sepal.Width")
#'
#' @export
plot_predict_interaction <- function(forest, data, variable1, variable2, grid = 100,
                                     main = paste0("Prediction of the forest for different values of ",
                                                   paste0(variable1, paste0(" and ", variable2))),
                                     time = NULL){
  UseMethod("plot_predict_interaction")
}

#' @import ggplot2
#' @importFrom stats predict
#' @importFrom stats terms
#' @importFrom stats as.formula
#' @export
plot_predict_interaction.randomForest <- function(forest, data, variable1, variable2, grid = 100,
                                                  main = paste0("Prediction of the forest for different values of ",
                                                                paste0(variable1, paste0(" and ", variable2))),
                                                  time = NULL){
  if (forest$type == "unsupervised") {
    warning("plot_predict_interaction cannot be performed on unsupervised random forests.")
    return(NULL)
  }
  newdata <- expand.grid(seq(min(data[[variable1]]), max(data[[variable1]]), length.out = grid),
                         seq(min(data[[variable2]]), max(data[[variable2]]), length.out = grid))
  colnames(newdata) <- c(variable1, variable2)
  if(as.character(forest$call$formula)[3] == "."){
    other_vars <- setdiff(names(data), as.character(forest$call$formula)[2])
  } else {
    other_vars <- labels(terms(as.formula(forest$call$formula)))
  }
  other_vars <- setdiff(other_vars, c(variable1, variable2))
  n <- nrow(data)
  for(i in other_vars){
    newdata[[i]] <- data[[i]][sample(1:n, nrow(newdata), replace = TRUE)]
  }
  if(forest$type == "regression"){
    newdata$prediction <- predict(forest, newdata, type = "response")
    plot <- ggplot(newdata, aes_string(x = variable1, y = variable2, fill = "prediction")) +
      geom_raster() + theme_bw() +
      scale_fill_gradient2(midpoint = min(newdata$prediction) + 0.5 * (max(newdata$prediction) - min(newdata$prediction)),
                           low = "blue", high = "red")
  } else if(forest$type == "classification"){
    id_vars <- colnames(newdata)
    if(length(forest$classes) == 2){
      newdata[, paste0("probability_", forest$classes[-1])] <- predict(forest, newdata, type = "prob")[, -1]
    } else {
      newdata[, paste0("probability_", forest$classes)] <- predict(forest, newdata, type = "prob")
    }
    newdata <- reshape2::melt(newdata, id.vars = id_vars)
    newdata$prediction <- newdata$value
    plot <- ggplot(newdata, aes_string(x = variable1, y = variable2, fill = "prediction")) +
      geom_raster() + theme_bw() + facet_wrap(~ variable) +
      scale_fill_gradient2(midpoint = min(newdata$prediction) + 0.5 * (max(newdata$prediction) - min(newdata$prediction)),
                           low = "blue", high = "red")
  }
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}

#' @import ggplot2
#' @importFrom stats predict
#' @importFrom stats terms
#' @importFrom stats as.formula
#' @export
plot_predict_interaction.ranger <- function(forest, data, variable1, variable2, grid = 100,
                                            main = paste0("Prediction of the forest for different values of ",
                                                          paste0(variable1, paste0(" and ", variable2))),
                                            time = NULL){
  newdata <- expand.grid(seq(min(data[[variable1]]), max(data[[variable1]]), length.out = grid),
                         seq(min(data[[variable2]]), max(data[[variable2]]), length.out = grid))
  colnames(newdata) <- c(variable1, variable2)
  if(as.character(forest$call[[2]])[3] == "."){
    other_vars <- setdiff(names(data), as.character(forest$call[[2]])[2])
  } else {
    other_vars <- labels(terms(as.formula(forest$call[[2]])))
  }
  other_vars <- setdiff(other_vars, c(variable1, variable2))
  n <- nrow(data)
  for(i in other_vars){
    newdata[[i]] <- data[[i]][sample(1:n, nrow(newdata), replace = TRUE)]
  }
  if(forest$treetype == "Regression"){
    newdata$prediction <- predict(forest, newdata, type = "response")$predictions
    plot <- ggplot(newdata, aes_string(x = variable1, y = variable2, fill = "prediction")) +
      geom_raster() + theme_bw() +
      scale_fill_gradient2(midpoint = min(newdata$prediction) + 0.5 * (max(newdata$prediction) - min(newdata$prediction)),
                           low = "blue", high = "red")
  } else if(forest$treetype == "Probability estimation"){
    id_vars <- colnames(newdata)
    pred <- predict(forest, newdata)$predictions
    if(ncol(pred) == 2){
      newdata[, paste0("probability_", colnames(pred)[-1])] <- pred[, -1]
    } else {
      newdata[, paste0("probability_", colnames(pred))] <- pred
    }
    newdata <- reshape2::melt(newdata, id.vars = id_vars)
    newdata$prediction <- newdata$value
    plot <- ggplot(newdata, aes_string(x = variable1, y = variable2, fill = "prediction")) +
      geom_raster() + theme_bw() + facet_wrap(~ variable) +
      scale_fill_gradient2(midpoint = min(newdata$prediction) + 0.5 * (max(newdata$prediction) - min(newdata$prediction)),
                           low = "blue", high = "red")
  } else if(forest$treetype == "Classification"){
    stop("Ranger forest for classification needs to be generated by ranger(..., probability = TRUE).")
  } else if(forest$treetype == "Survival"){
    pred <- predict(forest, newdata, type = "response")
    if (is.null(time)) {
      time <- pred$unique.death.times[which.min(abs(colMeans(pred$survival, na.rm = TRUE) - 0.5))]
      message(sprintf("Using unique death time %s which is the closest to predicted median survival time.", time))
    } else if (!time %in% pred$unique.death.times) {
      new_time <- pred$unique.death.times[which.min(abs(pred$unique.death.times - time))]
      message(sprintf("Using closest unique death time %s instead of %s.", new_time, time))
      time <- new_time
    }
    newdata$prediction <- pred$survival[, pred$unique.death.times == time, drop = TRUE]
    plot <- ggplot(newdata, aes_string(x = variable1, y = variable2, fill = "prediction")) +
      geom_raster() + theme_bw() +
      scale_fill_gradient2(midpoint = min(newdata$prediction) + 0.5 * (max(newdata$prediction) - min(newdata$prediction)),
                           low = "blue", high = "red")
  } else {
    stop(sprintf("Ranger forest type '%s' is currently not supported.", forest$treetype))
  }
  if(!is.null(main)){
    plot <- plot + ggtitle(main)
  }
  return(plot)
}
