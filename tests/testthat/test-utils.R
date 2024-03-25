test_that("functions work as expected without warnings", {
  expect_equal(min_na(NA), NA)
  expect_equal(max_na(NA), NA)
})

test_that("get_feature_names() work with '.' features", {
  fit_rf <- randomForest(Sepal.Width ~ ., data = iris)
  fit_ranger <- ranger(Sepal.Width ~ ., data = iris)

  expected <- setdiff(colnames(iris), "Sepal.Width")

  expect_equal(get_feature_names(fit_rf), expected)
  expect_equal(get_feature_names(fit_ranger), expected)
})

test_that("get_feature_names() work with explicit features", {
  form <- Sepal.Width ~ Sepal.Length + Species
  fit_rf <- randomForest(form, data = iris)
  fit_ranger <- ranger(form, data = iris)

  expected <- c("Sepal.Length", "Species")

  expect_equal(get_feature_names(fit_rf), expected)
  expect_equal(get_feature_names(fit_ranger), expected)
})

test_that("get_feature_names() work with xy interface of ranger", {
  xvars <- setdiff(colnames(iris), "Sepal.Width")
  fit_ranger <- ranger(y = iris$Sepal.Width, x = iris[xvars])

  expect_equal(get_feature_names(fit_ranger), xvars)
})
