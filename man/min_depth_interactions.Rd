% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/min_depth_interactions.R
\name{min_depth_interactions}
\alias{min_depth_interactions}
\title{Calculate mean conditional minimal depth}
\usage{
min_depth_interactions(
  forest,
  vars = important_variables(measure_importance(forest)),
  mean_sample = "top_trees",
  uncond_mean_sample = mean_sample
)
}
\arguments{
\item{forest}{A randomForest object}

\item{vars}{A character vector with variables with respect to which conditional minimal depth will be calculated; by default it is extracted by the important_variables function but this may be time consuming}

\item{mean_sample}{The sample of trees on which conditional mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"}

\item{uncond_mean_sample}{The sample of trees on which unconditional mean minimal depth is calculated, possible values are "all_trees", "top_trees", "relevant_trees"}
}
\value{
A data frame with each observation giving the means of conditional minimal depth and the size of sample for a given interaction
}
\description{
Calculate mean conditional minimal depth with respect to a vector of variables
}
\examples{
forest <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100)
min_depth_interactions(forest, c("Petal.Width", "Petal.Length"))
}
