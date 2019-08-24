#' Explain a random forest
#'
#' Explains a random forest in a html document using plots created by randomForestExplainer
#'
#' @param forest A randomForest object created with the option localImp = TRUE
#' @param interactions Logical value: should variable interactions be considered (this may be time-consuming)
#' @param data The data frame on which forest was trained - necessary if interactions = TRUE
#' @param vars A character vector with variables with respect to which interactions will be considered if NULL then they will be selected using the important_variables() function
#' @param no_of_pred_plots The number of most frequent interactions of numeric variables to plot predictions for
#' @param pred_grid The number of points on the grid of plot_predict_interaction (decrease in case memory problems)
#' @param measures A character vector specifying the importance measures to be used for plotting ggpairs
#'
#' @return A html document in your working directory
#'
#' @import DT
#'
#' @examples
#' \dontrun{
#' forest <- randomForest::randomForest(Species ~ ., data = iris, localImp = TRUE)
#' explain_forest(forest, interactions = TRUE)
#' }
#'
#' @export
explain_forest <- function(forest, interactions = FALSE, data = NULL, vars = NULL, no_of_pred_plots = 3, pred_grid = 100,
                           measures = NULL){
  if(is.null(measures)){
    if("randomForest" %in% class(forest)){
      if(forest$type %in% c("classification", "unsupervised")){
        measures <- c("mean_min_depth", "accuracy_decrease", "gini_decrease", "no_of_nodes", "times_a_root")
      } else{
        measures <- c("mean_min_depth", "mse_increase", "node_purity_increase", "no_of_nodes", "times_a_root")
      }
    } else if("ranger" %in% class(forest)){
      measures <- c("mean_min_depth", forest$importance.mode, "no_of_nodes", "times_a_root")
    }
  }
  if("randomForest" %in% class(forest) && dim(forest$importance)[2] == 1){
    stop(paste("Your forest does not contain information on local importance so",
               ifelse(forest$type %in% c("classification", "unsupervised"), "accuracy_decrease", "mse_increase"),
               "measure cannot be extracted.",
               "To add it regrow the forest with the option localImp = TRUE and run this function again."))
  }
  if("ranger" %in% class(forest) && forest$importance.mode == "none"){
   stop(paste("Your forest does not contain importance information so",
              "importance cannot be extracted.",
              "To add it regrow the forest with the option importance other than 'none' and run this function again."))
  }
  environment <- new.env()
  environment$forest <- forest
  environment$data <- data
  environment$interactions <- interactions
  environment$vars <- vars
  environment$no_of_pred_plots <- no_of_pred_plots
  environment$pred_grid <- pred_grid
  environment$measures <- measures
  directory <- getwd()
  path_to_templates <- file.path(path.package("randomForestExplainer"), "templates")
  template_name <- grep('explain_forest_template.rmd', list.files(path_to_templates),
                        ignore.case = TRUE, value = TRUE)

  rmarkdown::render(file.path(path_to_templates, template_name),
                    "html_document", output_file = paste0(directory, "/Your_forest_explained.html"),
                    envir = environment)
}
