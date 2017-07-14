## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, fig.width = 7, fig.height = 5)

## ------------------------------------------------------------------------
library(randomForest)
# devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)

## ------------------------------------------------------------------------
data(Boston, package = "MASS")
Boston$chas <- as.logical(Boston$chas)
str(Boston)

## ------------------------------------------------------------------------
set.seed(2017)
forest <- randomForest(medv ~ ., data = Boston, localImp = TRUE)

## ------------------------------------------------------------------------
forest

## ------------------------------------------------------------------------
# min_depth_frame <- min_depth_distribution(forest)
# save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

## ------------------------------------------------------------------------
# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
plot_min_depth_distribution(min_depth_frame)

## ----fig.height = 7------------------------------------------------------
plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

## ------------------------------------------------------------------------
# importance_frame <- measure_importance(forest)
# save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame

## ------------------------------------------------------------------------
# plot_multi_way_importance(forest, size_measure = "no_of_nodes") # gives the same result as below but takes longer
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

## ------------------------------------------------------------------------
plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)

## ------------------------------------------------------------------------
# plot_importance_ggpairs(forest) # gives the same result as below but takes longer
plot_importance_ggpairs(importance_frame)

## ------------------------------------------------------------------------
# plot_importance_rankings(forest) # gives the same result as below but takes longer
plot_importance_rankings(importance_frame)

## ------------------------------------------------------------------------
# (vars <- important_variables(forest, k = 5, measures = c("mean_min_depth", "no_of_trees"))) # gives the same result as below but takes longer
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

## ------------------------------------------------------------------------
# interactions_frame <- min_depth_interactions(forest, vars)
# save(interactions_frame, file = "interactions_frame.rda")
load("interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

## ------------------------------------------------------------------------
# plot_min_depth_interactions(forest) # calculates the interactions_frame for default settings so may give different results than the function below depending on our settings and takes more time
plot_min_depth_interactions(interactions_frame)

## ------------------------------------------------------------------------
# interactions_frame <- min_depth_interactions(forest, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
# save(interactions_frame, file = "interactions_frame_relevant.rda")
load("interactions_frame_relevant.rda")
plot_min_depth_interactions(interactions_frame)

## ------------------------------------------------------------------------
plot_predict_interaction(forest, Boston, "rm", "lstat")

## ---- eval = FALSE-------------------------------------------------------
#  explain_forest(forest, interactions = TRUE, data = Boston)

