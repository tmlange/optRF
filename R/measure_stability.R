#' @title Measure the stability of random forest
#'
#' @description Measure the stability of random forest for a certain data set with a certain number of trees
#'
#' @param num.trees Either a single value or a vector containing the numbers of trees for which the stability should be analysed (default = 500).
#' @param method Either "prediction" (default) or "importance" specifying if random forest should be used for prediction or to estimate the variable importance.
#' @param X_Test If method is "prediction", a data frame containing the explanatory variables of the test data set. If not entered, the out of bag data will be used.
#' @param alpha If method is "prediction", the number of best individuals to be selected in the test data set (default = 0.15), if method is "importance", the number of most important variables to be selected (default = 0.05).
#' @param select_for If method is "prediction", what should be selected? In random forest classification, this must be set to a vector containing the values of the desired classes. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param importance If method is "importance", the variable importance mode, one of "permutation" (default), "impurity" or "impurity_corrected".
#' @param verbose Show computation status.
#' @param ... Any other argument from the ranger function.
#' @inheritParams number_rep_helper
#' @inheritParams response_type_helper
#' @inheritParams prediction_shared_parameters
#'
#' @return A data frame summarising the estimated stability for the given num.trees values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' stability_result = measure_stability(y = SNPdata[,1], X=SNPdata[,-1], num.trees=500)
#' stability_result # Stability of random forest with 500 trees
#' }
#'
#' @export

measure_stability = function(y, X, method=c("prediction","importance"), X_Test=NULL,
                             number_repetitions = 10, alpha = NULL,
                             num.trees_values = c(250, 500, 750, 1000), 
                             importance = c("permutation", "impurity", "impurity_corrected"), 
                             select_for = c("high", "low", "zero"),
                             rank_based = FALSE,
                             response_type = NULL,
                             stability_metric = NULL,
                             verbose = TRUE, ...){

  # (I) Input validation
  method = match.arg(method)
  if(method == "importance"){
    importance = match.arg(importance)
    if(!is.logical(rank_based)) stop("'rank_based' must be TRUE or FALSE.")
    stability_metric = if(rank_based) "kendall" else "icc"
    alpha = 0.05
  } else{
    importance = "none"
    alpha = 0.15
  }
  
  # (II) Run the analysis
  rf_result = .run_rf_engine(y = y, X = X, X_Test = X_Test, method = method,
                             number_repetitions = number_repetitions,
                             num.trees_values = num.trees_values,
                             alpha = alpha, select_for = select_for,
                             rec_thresh = rec_thresh, stability_metric = stability_metric, 
                             response_type = response_type, importance = importance, 
                             verbose = verbose, ...)
  return(rf_result[[1]])
}


