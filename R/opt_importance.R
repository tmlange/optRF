#' @title Optimise random forest variable importance estimations
#'
#' @description Optimising random forest for estimating the importance of variables by calculating the variable importance stability with certain numbers of trees
#'
#' @param y A vector containing the response variable.
#' @param X A data frame containing the explanatory variables. The number of rows must be equal to the number of elements in y.
#' @param alpha The amount of most important variables to be selected based on their estimated variable importance. If < 1, alpha will be considered the relative amount of variables in the data set.
#' @param importance Variable importance mode, one of "permutation" (default), "impurity" or "impurity_corrected". The "impurity" measure is the Gini index for classification and the variance of the responses for regression.
#' @param visualisation Can be set to "importance" to draw a plot of the variable importance stability or to "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param recommendation If set to "importance" (default) or "selection", a recommendation will be given based on optimised variable importance or selection stability.
#' @param rank_based Should the variable importance stability be defined by the similarity of the estimated variable importance values via the intraclass correlation coefficient (rank_based == FALSE, default) or by the rankings of the variables via Kendall's W (rank_based == TRUE)?
#' @inheritParams round_rec_helper
#' @inheritParams number_rep_helper
#' @inheritParams rec_thresh_helper
#' @inheritParams num.trees_values_helper
#' @inheritParams response_type_helper
#' @inheritParams opt_shared_parameters
#'
#' @return An opt_importance_object containing the recommended number of trees, based on which measure the recommendation was given (importance or selection), a matrix summarising the estimated stability and computation time of a random forest with the recommended numbers of trees, a matrix containing the calculated stability and computation time for the analysed numbers of trees, and the parameters used to model the relationship between stability and numbers of trees.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optimp = opt_importance(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' summary(result_optimp)
#' }
#'
#' @export

opt_importance = function(y, X, number_repetitions = 10, alpha = 0.05, 
                          num.trees_values = c(250, 500, 750, 1000, 2000),
                          importance = c("permutation", "impurity", "impurity_corrected"),
                          visualisation = c("none","importance","selection"), 
                          recommendation = c("importance","selection"),
                          rec_thresh = 1e-6, 
                          round_recommendation = c("thousand","hundred","ten","none"), 
                          rank_based = FALSE, response_type = NULL,
                          verbose = TRUE, ...){
  
  # (I) Input validation
  round_rec = round_rec_helper(round_recommendation)
  rec_thresh = rec_thresh_helper(rec_thresh)
  visualisation = match.arg(visualisation)
  recommendation = match.arg(recommendation)
  importance = match.arg(importance)
  if(!is.logical(rank_based)) stop("'rank_based' must be TRUE or FALSE.")
  stability_metric = if(rank_based) "kendall" else "icc"
  
  # (II) Run the analysis
  rf_result = .run_rf_engine(y = y, X = X, X_Test = NULL, method = "importance",
                                  number_repetitions = number_repetitions,
                                  num.trees_values = num.trees_values,
                                  alpha = alpha, select_for = "high",
                                  rec_thresh = rec_thresh, stability_metric = stability_metric, 
                                  response_type = response_type, importance = importance, 
                                  verbose = verbose, ...)
  
  # (III) Fit stability models and create output
  config = list(method = "Variable_importance", col = "variable_importance_stability", vis_key = "importance")
  return(.postprocess_rf_results(rf_result, X, config, visualisation, recommendation, rec_thresh, round_rec, verbose))
}
