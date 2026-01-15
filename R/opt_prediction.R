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
  number_repetitions = number_rep_helper(number_repetitions)
  rec_thresh = rec_thresh_helper(rec_thresh)
  num.trees_values = num.trees_values_helper(num.trees_values)
  
  if(nrow(X) != length(y)) stop("Invalid input. Number of rows in 'X' does not match length of 'y'.")
  
  # Check value of y and response_type
  response_result = response_type_helper(response_type, y)
  y <- response_result$y
  response_type <- response_result$response_type
  
  # Check the select_for variable
  select_for = select_for_helper(y, response_type, select_for, alpha)
  
  # Check value of stability_metric
  if(is.null(stability_metric)){
    if(response_type == "metric") stability_metric = "icc"
    if(response_type == "ordinal") stability_metric = "krippendorff"
    if(response_type == "categorical") stability_metric = "fleiss_kappa"
  } else{
    stability_metric = match.arg(stability_metric, c("icc", "kendall", "krippendorff", "fleiss_kappa"))
  }
  if(response_type == "metric" && stability_metric %in% c("fleiss_kappa")){
    stop("For metric response, stability_metric must be 'icc', 'kendall' or 'krippendorff'")
  }
  if(response_type == "ordinal" && stability_metric %in% c("kendall", "icc")){
    stop("For ordinal response, stability_metric must be 'krippendorff' or 'fleiss_kappa'")
  }
  if(response_type == "categorical" && stability_metric %in% c("icc", "kendall")){
    stop("For categorical response, stability_metric must be 'fleiss_kappa' or 'krippendorff'")
  }
  # Verify variables of the test data set
  if(is.null(X_Test)){
    if(verbose){
      message("No test data were entered. Out of bag data will be used.")
    }
    sample_IDs = paste0("ID_", c(1:nrow(X)))
  }
  else{
    if(ncol(X) != ncol(X_Test) | !all(colnames(X) %in% colnames(X_Test))){
      stop("X_Test needs to contain the same variables as X.")
    }
    sample_IDs = paste0("ID_", c(1:nrow(X_Test)))
  }
  
  # Create test sequence
  variable_number = round(ncol(X), -2)
  test_seq = if(variable_number < 100000) seq(10, 1e6, 10) else seq(10, round((variable_number*100), -1), 10)
  
  # Calculate selection_size
  if(response_type == "metric"){
    # Defining the number of individuals to be selected from the data set
    if(alpha < 1){
      selection_size = round(length(sample_IDs)*alpha)
    }
    else{
      selection_size = round(alpha)
    }
  }
  
  # Check if the test data set consists of multiple objects
  if(length(sample_IDs) < 2){
    stop("The test data needs to have at least 2 objects in order to calculate stability.")
  }

  # (II) Run the analysis
  
  summary_result = data.frame()
  predictionStab = NULL
  selectionStab = NULL
  
  for(num.trees_value in num.trees_values){
    repeatedResult = .run_rf_repeated(y, X, X_Test, method = "prediction", rowCount = ifelse(is.null(X_Test), nrow(X), nrow(X_Test)), num.trees_value, response_type, importance = "none", number_repetitions, verbose, ...)
    pred_mat = repeatedResult[["result_mat"]]
    avg_time_taken = repeatedResult[["timeTaken"]]
    sel_mat = .compute_selection_mat(result_mat = pred_mat, response_type, select_for, selection_size, alpha)
    # Calculate the stability values
    if(stability_metric == "icc"){
      pred_stability = irr::icc(pred_mat)$value
      ps_definition = "ICC"
    } else if(stability_metric == "kendall"){
      pred_stability = irr::kendall(pred_mat)$value
      ps_definition = "Kendalls_W"
    } else if(stability_metric == "fleiss_kappa"){
      pred_stability = irr::kappam.fleiss(pred_mat)$value
      ps_definition = "Fleiss_Kappa"
    } else{
      pred_mat = t(pred_mat)
      if(response_type == "metric"){
        pred_stability = irr::kripp.alpha(pred_mat, method="interval")$value
      } else if(response_type == "ordinal"){
        pred_stability = irr::kripp.alpha(pred_mat, method="ordinal")$value
      } else{
        pred_stability = irr::kripp.alpha(pred_mat, method="nominal")$value
      }
      ps_definition = "Krippendorffs_alpha"
    }
    tmp_res = data.frame(num.trees_values = num.trees_value,
                         pred_stability = pred_stability,
                         selection_stability = kappam.fleiss(sel_mat)$value,
                         computation_time = avg_time_taken)
    summary_result = rbind(summary_result, tmp_res)
  }
  # Optional visualisation
  if(visualisation == "prediction") create_stability_plot(summary_result$pred_stability, summary_result$num.trees_values, "prediction stability")
  if(visualisation == "selection") create_stability_plot(summary_result$selection_stability, summary_result$num.trees_values, "selection stability")
  # If there are more than four data points, fit stability models
  if(nrow(summary_result) >= 4){
    predictionStab = fit_stability_model(summary_result, "pred_stability", test_seq, visualisation == "prediction")
    selectionStab = fit_stability_model(summary_result, "selection_stability", test_seq, visualisation == "selection")
  }
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = summary_result)
  
  # (III) Prepare the output
  return(.create_output(method = "Prediction", stability_definition = ps_definition, result_table = summary_result, primaryStab = predictionStab, selectionStab, runtime_model, rec_thresh, round_rec, recommendation, verbose))
}
