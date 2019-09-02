library(ranger)
library(dplyr)
set.seed(12345)

context("Test ranger classification forests")
forest <- ranger(Species ~ ., data = iris, probability = TRUE, num.trees = 2, importance = "impurity")

test_that("measure_importance works", {
  imp_df <- measure_importance(forest, mean_sample = "all_trees",
                               measures = c("mean_min_depth", "impurity",
                                            "no_of_nodes", "times_a_root"))
  expect_equal(imp_df$variable, c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
})

test_that("important_variables works", {
  imp_vars <- important_variables(forest, k = 3,
                                  measures = c("mean_min_depth", "impurity",
                                               "no_of_nodes", "times_a_root"))
  expect_equal(imp_vars, c("Petal.Width", "Sepal.Length", "Petal.Length"))
})

test_that("min_depth_distribution works", {
  min_depth_dist <- min_depth_distribution(forest)
  print(min_depth_dist)
  expect_equivalent(arrange(min_depth_dist, tree, minimal_depth, variable),
                    data.frame("tree" = c(1, 1, 1, 2, 2, 2),
                               "variable"=c("Petal.Width", "Sepal.Length", "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"),
                               "minimal_depth"=c(0, 2, 3, 0, 3, 3), stringsAsFactors = FALSE))
})

test_that("min_depth_interactions works", {
  min_depth_int <- min_depth_interactions(forest, vars = c("Petal.Width"))
  expect_equal(as.character(min_depth_int$variable), c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
})
