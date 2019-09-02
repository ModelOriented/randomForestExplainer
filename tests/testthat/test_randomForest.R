library(randomForest)
library(dplyr)
set.seed(12345)

context("Test randomForest classification forests")
forest <- randomForest(Species ~ ., data = iris, localImp = TRUE, ntree = 2)

test_that("measure_importance works", {
  imp_df <- measure_importance(forest, mean_sample = "all_trees",
                               measures = c("mean_min_depth","accuracy_decrease",
                                            "gini_decrease", "no_of_nodes", "times_a_root"))
  expect_equal(imp_df$variable, c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
})

test_that("important_variables works", {
  imp_vars <- important_variables(forest, k = 3,
                                  measures = c("mean_min_depth","accuracy_decrease",
                                               "gini_decrease", "no_of_nodes", "times_a_root"))
  expect_equal(imp_vars, c("Petal.Width", "Petal.Length", "Sepal.Length"))
})

test_that("min_depth_distribution works", {
  min_depth_dist <- min_depth_distribution(forest)
  print(min_depth_dist)
  expect_equivalent(arrange(min_depth_dist, tree, minimal_depth, variable),
                    data.frame("tree" = c(1, 1, 1, 1, 2, 2, 2),
                               "variable"=c("Petal.Width", "Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width", "Sepal.Length", "Petal.Length"),
                               "minimal_depth"=c(0, 1, 2, 4, 0, 1, 3)))
})

test_that("min_depth_interactions works", {
  min_depth_int <- min_depth_interactions(forest, vars = c("Petal.Width"))
  expect_equal(as.character(min_depth_int$variable), c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
})
