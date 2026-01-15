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
  importance = match.arg(importance)
  visualisation = match.arg(visualisation)
  recommendation = match.arg(recommendation)
  number_repetitions = number_rep_helper(number_repetitions)
  rec_thresh = rec_thresh_helper(rec_thresh)
  num.trees_values = num.trees_values_helper(num.trees_values)
  
  if(!is.logical(rank_based)) stop("'rank_based' must be TRUE or FALSE.")
  VI_definition = if(rank_based) "Kendalls_W" else "ICC"
  if(nrow(X) != length(y)) stop("Invalid input. Number of rows in 'X' does not match length of 'y'.")
  
  # Check value of y and response_type
  response_result = response_type_helper(response_type, y)
  y = response_result$y
  response_type = response_result$response_type
  
  # Determine selection size
  if(!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= ncol(X)){
    stop("'alpha' must be a single positive number (proportion or count)")
  }
  selection_size = if(alpha < 1) round(ncol(X)*alpha) else round(alpha)
  
  # Create test sequence
  variable_number = round(ncol(X), -2)
  test_seq = if(variable_number < 100000) seq(10, 1e6, 10) else seq(10, round((variable_number*100), -1), 10)
  
  # (II) Run the analysis
  
  summary_result = data.frame()
  importanceStab = NULL
  selectionStab = NULL
  
  for(num.trees_value in num.trees_values){
    repeatedResult = .run_rf_repeated(y, X, X_Test = NULL, method = "importance", rowCount = ncol(X), num.trees_value, response_type, importance, number_repetitions, verbose, ...)
    vi_mat = repeatedResult[["result_mat"]]
    avg_time_taken = repeatedResult[["timeTaken"]]
    sel_mat = .compute_selection_mat(result_mat = vi_mat, response_type = "metric", select_for = "high", selection_size, alpha = NULL)
    vi_stability = if(rank_based) irr::kendall(vi_mat)$value else irr::icc(vi_mat)$value
    sel_stability = irr::kappam.fleiss(sel_mat)$value
    tmp_res = data.frame(num.trees_values = num.trees_value,
                         VI_stability = vi_stability,
                         selection_stability = sel_stability,
                         computation_time = avg_time_taken)
    summary_result = rbind(summary_result, tmp_res)
  }
  # Optional visualisation
  if(visualisation == "importance") create_stability_plot(summary_result$VI_stability, summary_result$num.trees_values, "variable importance stability")
  if(visualisation == "selection") create_stability_plot(summary_result$selection_stability, summary_result$num.trees_values, "selection stability")
  # If there are more than four data points, fit stability models
  if(nrow(summary_result) >= 4){
    importanceStab = fit_stability_model(summary_result, "VI_stability", test_seq, visualisation == "importance")
    selectionStab = fit_stability_model(summary_result, "selection_stability", test_seq, visualisation == "selection")
  }
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = summary_result)
  
  # (III) Prepare the output
  return(.create_output(method = "Variable_importance", stability_definition = VI_definition, result_table = summary_result, primaryStab = importanceStab, selectionStab, runtime_model, rec_thresh, round_rec, recommendation, verbose))
}
