#' @title Optimise random forest predictions
#'
#' @description Optimising random forest predictions by calculating the prediction stability with certain numbers of trees
#'
#' @param X_Test A data frame containing the explanatory variables of the test data set. If not entered, the out of bag data will be used.
#' @param alpha The number of best individuals to be selected in the test data set based on their predicted response values. If < 1, alpha will be considered to be the relative amount of individuals in the test data set.
#' @param visualisation Can be set to "prediction" to draw a plot of the prediction stability or "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param select_for What should be selected? In random forest classification, this must be set to a vector containing the values of the desired classes. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param recommendation If set to "prediction" (default) or "selection", a recommendation will be given based on optimised prediction or selection stability.
#' @param stability_metric Define the prediction stability metric that should be used. Valid options are "icc", "kendall", or "krippendorff" for a metric response, "fleiss_kappa" or "krippendorff" for a categorical response and "krippendorff" for an ordinal response.
#' @inheritParams round_rec_helper
#' @inheritParams number_rep_helper
#' @inheritParams rec_thresh_helper
#' @inheritParams num.trees_values_helper
#' @inheritParams response_type_helper
#' @inheritParams opt_shared_parameters
#' @inheritParams prediction_shared_parameters
#'
#' @return An opt_prediction_object containing the recommended number of trees, based on which measure the recommendation was given (prediction or selection), a matrix summarising the estimated stability and computation time of a random forest with the recommended numbers of trees, a matrix containing the calculated stability and computation time for the analysed numbers of trees, and the parameters used to model the relationship between stability and numbers of trees.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' summary(result_optpred)
#' }
#'
#' @export
#' @importFrom stats predict

opt_prediction = function(y, X, X_Test=NULL,
                          number_repetitions = 10, alpha = 0.15,
                          num.trees_values = c(250, 500, 750, 1000, 2000), 
                          visualisation = c("none","prediction","selection"), 
                          select_for = c("high", "low", "zero"),
                          recommendation = c("prediction","selection"),
                          rec_thresh = 1e-6, round_recommendation = c("thousand","hundred","ten","none"), 
                          response_type = NULL,
                          stability_metric = NULL,
                          verbose = TRUE, ...){
  
  # (I) Input validation
  
  round_rec = round_rec_helper(round_recommendation)
  visualisation = match.arg(visualisation)
  recommendation = match.arg(recommendation)

  # (II) Run the analysis
  rf_result = .run_rf_engine(y = y, X = X, X_Test = X_Test, method = "prediction",
                             number_repetitions = number_repetitions,
                             num.trees_values = num.trees_values,
                             alpha = alpha, select_for = select_for,
                             rec_thresh = rec_thresh, stability_metric = stability_metric, 
                             response_type = response_type, importance = "none", 
                             verbose = verbose, ...)
  summary_result = rf_result[[1]]
  stability_definition = rf_result[[2]]
  
  # (III) Fit stability models
  # Optional visualisation
  if(visualisation == "prediction") create_stability_plot(summary_result$prediction_stability, summary_result$num.trees_values, "prediction stability")
  if(visualisation == "selection") create_stability_plot(summary_result$selection_stability, summary_result$num.trees_values, "selection stability")
  # If there are more than four data points, fit stability models
  # Create test sequence
  variable_number = round(ncol(X), -2)
  test_seq = if(variable_number < 100000) seq(10, 1e6, 10) else seq(10, round((variable_number*100), -1), 10)
  predictionStab = NULL
  selectionStab = NULL
  if(nrow(summary_result) >= 4){
    predictionStab = fit_stability_model(summary_result, "prediction_stability", test_seq, visualisation == "prediction")
    selectionStab = fit_stability_model(summary_result, "selection_stability", test_seq, visualisation == "selection")
  }
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = summary_result)
  
  # (IV) Prepare the output
  return(.create_output(method = "Prediction", stability_definition = stability_definition, result_table = summary_result, primaryStab = predictionStab, selectionStab, runtime_model, rec_thresh, round_rec, recommendation, verbose))
}
