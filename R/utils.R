#' @param verbose Show computation status
#' @param ... Any other argument from the ranger or ordfor functions.
#' @name opt_shared_parameters
NULL

#' @param y A vector containing the response variable in the training data set.
#' @param X A data frame containing the explanatory variables in the training data set. The number of rows must be equal to the number of elements in y.
#' @name prediction_shared_parameters
NULL

#' @param optRF_object An optRF_object, either the result from the \link{opt_importance} or the \link{opt_prediction} function.
#' @name estimate_plot_shared_parameters
NULL

#' @param round_recommendation Setting to what number the recommended number of trees should be rounded to. Options: "none", "ten", "hundred", "thousand" (default).
round_rec_helper = function(round_recommendation = c("thousand","hundred","ten","none")){

  round_recommendation = match.arg(round_recommendation)

  switch(
    round_recommendation,
    none = 0,
    ten = -1,
    hundred = -2,
    thousand = -3
  )
}

#' @param number_repetitions Number of repetitions of random forest to estimate the stability. It needs to be at least 2. Default is 10.
number_rep_helper = function(number_repetitions){
  if(!is.numeric(number_repetitions) | number_repetitions[1] < 2){
    stop("number_repetitions needs to be a number >= 2.")
  }
  number_repetitions = ceiling(number_repetitions[1])
}

#' @param rec_thresh If the number of trees leads to an increase of stability smaller or equal to the value specified, this number of trees will be recommended. Default is 1e-6.
rec_thresh_helper = function(rec_thresh){
  if(!is.numeric(rec_thresh) | rec_thresh <= 0){
    stop("rec_thresh needs to be a positive number.")
  }
  rec_thresh = rec_thresh[1]
}

#' @param num.trees_values A vector containing the numbers of trees to be analysed. If not specified, 250, 500, 750, 1000, and 2000 trees will be analysed.
num.trees_values_helper = function(num.trees_values){
  if(!is.numeric(num.trees_values) || any(num.trees_values < 1)){
    stop("Invalid input. The parameter 'num.trees_values' needs to be a single positive number or a vector of positive numbers.")
  }
  return(ceiling(num.trees_values))
}

#' @param response_type What data type is the response variable? Either "metric", "ordinal", or "categorical" are possible. If not set, the data type will be guessed 
response_type_helper = function(response_type, y, max_ordinal_levels = 10){
  #Basic validity checks for y
  if(is.null(y) || length(y) == 0){
    stop("The response variable 'y' is empty or NULL.")
  }
  if(anyNA(y)){
    stop("Missing values detected in the response variable 'y'. ",
         "Please remove or impute them before calling this function.")
  }
  if(is.character(y) || is.logical(y)){
    y = factor(y)
  }
  if(!is.numeric(y) && !is.factor(y)){
    stop("The response variable 'y' must be numeric, factor, character, or logical.")
  }
  if(length(unique(y)) < 2){
    stop("The response variable 'y' must contain at least two distinct values.")
  }
  
  # Guess response_type if it is NULL
  if(is.null(response_type)){
    if(is.ordered(y)){
      response_type = "ordinal"
    } else if(is.factor(y)){
      response_type = "categorical"
    } else{
      unique_vals = unique(y)
      if(length(unique_vals) <= max_ordinal_levels && all(y %% 1 == 0)){
        response_type = "ordinal"
      } else{
        response_type = "metric"
      }
    }
    message("'response_type' has been set to '", response_type,"'.")
  }
  response_type <- match.arg(response_type, c("metric", "ordinal", "categorical"))
  
  # Check if response_type and y match
  if(response_type == "metric" && !is.numeric(y)) {
    stop("response_type = 'metric' requires 'y' to be numeric.")
  }
  if(response_type == "categorical" && !is.factor(y)){
    message("Numeric response converted to factor for categorical analysis.")
    y = as.factor(y)
  }
  if(response_type == "ordinal"){
    if(is.factor(y) && !is.ordered(y)){
      stop("response_type = 'ordinal' requires 'y' to be an ordered factor.")
    }
    if(is.numeric(y)){
      sorted_unique_vals = sort(unique(y))
      if(length(sorted_unique_vals) <= max_ordinal_levels){
        message("Numeric response converted to ordered factor for ordinal analysis.")
        message("Please use response_type = 'metric' if you intended to interpret them as numeric.")
        y <- factor(y, levels = sorted_unique_vals, ordered = TRUE)
      } else{
        stop("Numeric 'y' has too many unique values to be treated as ordinal.")
      }
    }
  }
  list(
    y = y,
    response_type = response_type
  )
}

select_for_helper = function(y, response_type, select_for = c("high", "low", "zero"), alpha, rowCount){
  if(response_type == "metric"){
    # Validate select_for for numeric y
    select_for = match.arg(select_for)
    if(!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= rowCount){
      stop("For metric responses, 'alpha' must be a single positive number (proportion or count).")
    }
  }
  else if(response_type == "categorical"){
    # Validate select_for for categorical y
    if(missing(select_for) || !all(select_for %in% levels(y))){
      stop("For a categorical response variable, 'select_for' must be a subset of its classes.")
    }
    select_for = unique(select_for)
    # Ensure select_for does not include all levels of y.
    if(length(select_for) == length(levels(y))){
      stop("'select_for' cannot include all classes of the categorical response variable.")
    }
  } else{
    # Validate select_for for ordinal y
    if(length(select_for) != 1 || !select_for %in% c("high", "low")){
      stop("For a ordinal response variable, 'select_for' must be either 'high' or 'low'.")
    }
    if(length(alpha) != 1 || !alpha %in% levels(y)){
      stop("For a ordinal response variable, 'alpha' must be exactly one of the response levels.")
    }
    y_levels = levels(y)
    if((select_for == "high" && alpha == y_levels[1]) ||
       (select_for == "low" && alpha == y_levels[length(y_levels)])){
      stop("The chosen 'alpha' and 'select_for' would result in all individuals being selected.")
    }
  }
  return(select_for)
}

TwoPLmodel = function(vec, p1, p2){
  1 / (1+(p1/vec)^p2)
}

TwoPLmodel_inv = function(vec, p1, p2){
  p1 / (((1/vec)-1)^(1/p2))
}

#' Creates a plot of stability dependent on the number of trees, which is used by the \link{opt_importance} and the \link{opt_prediction} functions.
#'
#' @param stability_values A numeric vector containing the values for the y axis
#' @param num.tree_values A numeric vector containing the values for the x axis
#' @param label A character string indicating which stability measure is depicted and should be used for title and axis label
#' @noRd
create_stability_plot = function(stability_values, num.tree_values, label){
  plot(stability_values ~ num.tree_values, main=paste0('Relationship between\n', label, ' and number of trees'),
       ylab=label, xlab="number of trees",
       col="black", cex=1.5, pch=20,
       ylim=c((min(stability_values)-0.001), (max(stability_values)+0.001)),
       xlim=c(min(num.tree_values),max(num.tree_values)),
       cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
}

#' Performs non linear modelling between stability values and the number of trees
#'
#' @param summary.result A data.frame containing the number of trees and the stability as columns
#' @param variable A character string indicating the name of the column containing the stability values
#' @param test_seq A numeric vector containing the values for the x axis that should visualized
#' @param visualisation A boolean value indicating whether the model should be visualized in the current plot
#'
#' @return The non linear model as the output of the nlsLM function
#' @noRd
non_linear_modelling = function(summary_result, variable){
  start_val_p1 = summary_result$num.trees_values[round((nrow(summary_result)/2))]
  non.lin.mod <- minpack.lm::nlsLM(summary_result[,variable] ~ 1 / (1+(p1/num.trees_values)^p2), data=summary_result,
                          start=c(p1=start_val_p1, p2=0.5),
                          control = minpack.lm::nls.lm.control(maxiter = 1024))
  return(non.lin.mod)
}

fit_stability_model = function(summary_result, variable, test_seq, visualisation){
  tryCatch({
    nl_model = non_linear_modelling(summary_result, variable)
    estimates = data.frame(num.trees = test_seq,
                           estimated_stability = TwoPLmodel(test_seq, nl_model$m$getPars()[1], nl_model$m$getPars()[2]))
    if(visualisation){
      graphics::lines(estimates$estimated_stability ~ test_seq,
                      col="navyblue", lwd=3)
    }
    list(model = nl_model, estimates = estimates)
  }, error=function(e) NULL)
}

find_recommendation = function(estimates, model, rec_thresh, round_rec){
  # Calculate the increase of stability per increase of trees
  estimates$diff = c(NA,diff(estimates$estimated_stability)/10)
  estimates = estimates[-1,]
  
  # Finally, make a recommendation
  new.rec_thresh = rec_thresh
  trust.rec = FALSE
  while(trust.rec == FALSE){
    recommended_num.trees = round(estimates[estimates$diff<new.rec_thresh,]$num.trees[1], round_rec)
    # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
    if(recommended_num.trees >= model$m$getPars()[1]){
      trust.rec = TRUE
    } else{
    # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
      new.rec_thresh = new.rec_thresh*0.1
    }
  }
  return(recommended_num.trees)
}

#' Internal helper to safely print metrics from a stability table
#' 
#' @param stats A data frame or matrix containing the metrics.
#' @param label The string to display to the user.
#' @param row_name The row name to look for in the table.
#' @noRd
.print_metric = function(stats, label, row_name) {
  if (row_name %in% rownames(stats)) {
    cat(label, ": ", stats[row_name, 1], "\n", sep = "")
  }
}

#' Validates and returns the correct measure name
#'
#' @param measure User-specified name of measure
#' @param is_pred Boolean whether the object is of type opt_prediction_object or opt_importance_object
#'
#' @returns Measure name
#' @noRd
get_target_measure = function(measure, is_pred){
  allowed_measures = if(is_pred){
    c("prediction", "selection") 
  } else{
    c("importance", "selection")
  } 
  if(!measure %in% allowed_measures){
    stop(sprintf("The measure '%s' is not available for this object. Allowed: %s", measure, paste(allowed_measures, collapse = ", ")))
  }
  target = switch(measure,
                  "importance" = "Variable_importance_stability",
                  "selection" = "Selection_stability",
                  "prediction" = "Prediction_stability")
  return(target)
}

.run_rf = function(y, X, X_Test, method,num.trees_value, response_type, importance, ...){
  if(response_type == "ordinal"){
    myForest <- ordinalForest::ordfor(depvar = "y", data = data.frame(y = y, X),
                                      nsets = num.trees_value, ...)
    if(method == "importance"){
      return(myForest$varimp)
    } else{
      if(is.null(X_Test)){
        return(myForest$ypred_oob)
      } else {
        return(predict(myForest, newdata = X_Test)$ypred)
      }
    }
  } else{
    myForest <- ranger::ranger(x = X,
                               y = y,
                               num.trees = num.trees_value,
                               importance = importance,
                               verbose = FALSE, ...)
    if(method == "importance"){
      return(myForest$variable.importance)
    } else{
      runif(1, 0, .Machine$integer.max) #Uncomment to reproduce original results
      if(is.null(X_Test)){
        return(myForest$predictions)
      } else{
        return(predict(myForest, data=X_Test)$predictions)
      }
    }
  }
}

.run_rf_repeated = function(y, X, X_Test, method, rowCount, num.trees_value, response_type, importance, number_repetitions, verbose, ...){
  res_mat = matrix(NA, nrow = rowCount, ncol = number_repetitions)
  time_taken_vec = numeric(number_repetitions)
  for(rep in 1:number_repetitions){
    if(verbose){
      message(paste0("Analysing random forest with ", num.trees_value, " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
    }
    start.time = Sys.time()
    resultVec = .run_rf(y, X, X_Test, method,num.trees_value, response_type, importance, ...)
    time_taken_vec[rep] = as.numeric(difftime(Sys.time(), start.time, units = "secs"))
    res_mat[, rep] = resultVec
  }
  return(list(result_mat = res_mat, timeTaken = mean(time_taken_vec)))
}

.compute_selection_mat = function(result_mat, response_type, select_for, selection_size, alpha){
  if(response_type == "metric"){
    ranks_mat = if(select_for == "high") apply(-result_mat, 2, rank, ties.method = "first") else
      if(select_for == "low") apply(result_mat, 2, rank, ties.method = "first") else
        apply(abs(result_mat), 2, rank, ties.method = "first")
    return(ifelse(ranks_mat <= selection_size, "selected", "rejected"))
  } else if(response_type == "ordinal"){
    if(select_for == "high"){
      return(ifelse(result_mat >= alpha, "selected", "rejected"))
    } else{
      return(ifelse(result_mat <= alpha, "selected", "rejected"))
    }
  } else{ # categorical
    return(ifelse(matrix(result_mat %in% select_for, nrow = nrow(result_mat)), "selected", "rejected"))
  }
}

.create_output = function(method, stability_definition, result_table, primaryStab, selectionStab, runtime_model, rec_thresh, round_rec, recommendation, verbose){
  recommended_num.trees = NA
  rec_Stab = if(recommendation == "prediction" || recommendation == "importance") primaryStab else selectionStab
  if(!is.null(rec_Stab)){
    recommended_num.trees = find_recommendation(rec_Stab$estimates, rec_Stab$model, rec_thresh, round_rec)
    # If the recommended number of trees is for some reason lower than 500 (default), set it to be 500
    if(recommended_num.trees < 500) recommended_num.trees = 500
  } else{
    warning("A recommendation cannot be given because the relationship between the requested stability and numbers of trees could not be modelled.")
  }
  # Base output
  output = list(result_table = result_table)
  output[[paste0(method,"_stability_definition")]] = stability_definition
  # Add model parameters if available
  model_params = list()
  if(!is.null(primaryStab)) model_params[[paste0(method,"_stability")]] = primaryStab$model$m$getPars()
  if(!is.null(selectionStab)) model_params[["Selection_stability"]] = selectionStab$model$m$getPars()
  if(length(model_params) > 0){
    output$model_parameters = do.call(rbind, model_params)
    colnames(output$model_parameters) = c("Inflection_point", "Slope")
  }
  # Add recommendation if available
  if(!is.na(recommended_num.trees)){
    if(verbose) message("\n Recommended number of trees: ", recommended_num.trees)
    output$recommendation = recommended_num.trees
    output$recommendation_for = recommendation
    # Calculate expected stability for recommended number of trees
    stab_values = c()
    if(!is.null(primaryStab)) stab_values[paste0(method,"_stability")] = primaryStab$estimates[primaryStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    if(!is.null(selectionStab)) stab_values["Selection_stability"] = selectionStab$estimates[selectionStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    stab_values["Computation_time"] = predict(runtime_model, newdata = data.frame(num.trees_values = recommended_num.trees))
    output$expected_RF_stability <- matrix(stab_values, ncol = 1, dimnames = list(names(stab_values), "Value"))
  }
  class(output) = ifelse(method == "Prediction","opt_prediction_object","opt_importance_object")
  return(output)
}

.run_rf_engine = function(y, X, X_Test = NULL, method = c("prediction", "importance"),
                         number_repetitions,
                         num.trees_values,
                         alpha, select_for,
                         rec_thresh = 1e-6, stability_metric, 
                         response_type, importance, verbose = TRUE, ...
                         ){
  
  # (I) Input validation
  
  number_repetitions = number_rep_helper(number_repetitions)
  num.trees_values = num.trees_values_helper(num.trees_values)
  if(nrow(X) != length(y)) stop("Invalid input. Number of rows in 'X' does not match length of 'y'.")
  
  # Check value of y and response_type
  response_result = response_type_helper(response_type, y)
  y <- response_result$y
  response_type <- response_result$response_type
  
  if(method == "prediction"){
    # Verify variables of the test data set
    if(is.null(X_Test)){
      if(verbose){
        message("No test data were entered. Out of bag data will be used.")
      }
      rowCount = nrow(X)
    }
    else{
      if(ncol(X) != ncol(X_Test) | !all(colnames(X) %in% colnames(X_Test))){
        stop("X_Test needs to contain the same variables as X.")
      }
      rowCount = nrow(X_Test)
    }
    # Check the select_for variable
    select_for = select_for_helper(y, response_type, select_for, alpha, rowCount)
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
    # Calculate selection_size
    if(response_type == "metric"){
      # Defining the number of individuals to be selected from the data set
      if(alpha < 1){
        selection_size = round(rowCount*alpha)
      }
      else{
        selection_size = round(alpha)
      }
    }
    # Check if the test data set consists of multiple objects
    if(rowCount < 2){
      stop("The test data needs to have at least 2 objects in order to calculate stability.")
    }
  } else{ # Importance
    rowCount = ncol(X)
    # Determine selection size
    if(!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= ncol(X)){
      stop("'alpha' must be a single positive number (proportion or count)")
    }
    selection_size = if(alpha < 1) round(ncol(X)*alpha) else round(alpha)
  }
  
  # (II) Run the analysis
  summary_result = data.frame()
  
  for(num.trees_value in num.trees_values){
    repeatedResult = .run_rf_repeated(y, X, X_Test = X_Test, method = method, rowCount = rowCount, num.trees_value, response_type, importance, number_repetitions, verbose, ...)
    result_mat = repeatedResult[["result_mat"]]
    avg_time_taken = repeatedResult[["timeTaken"]]
    if(method == "prediction"){
      sel_mat = .compute_selection_mat(result_mat, response_type, select_for, selection_size, alpha)
    } else{
      sel_mat = .compute_selection_mat(result_mat, response_type = "metric", select_for = "high", selection_size, alpha = NULL)  
    }
    if(stability_metric == "icc"){
      result_stability = irr::icc(result_mat)$value
      stability_definition = "ICC"
    } else if(stability_metric == "kendall"){
      result_stability = irr::kendall(result_mat)$value
      stability_definition = "Kendalls_W"
    } else if(stability_metric == "fleiss_kappa"){
      result_stability = irr::kappam.fleiss(result_mat)$value
      stability_definition = "Fleiss_Kappa"
    } else{
      result_mat = t(result_mat)
      if(response_type == "metric"){
        result_stability = irr::kripp.alpha(result_mat, method="interval")$value
      } else if(response_type == "ordinal"){
        result_stability = irr::kripp.alpha(result_mat, method="ordinal")$value
      } else{
        result_stability = irr::kripp.alpha(result_mat, method="nominal")$value
      }
      stability_definition = "Krippendorffs_alpha"
    }
     
    tmp_res = data.frame(num.trees_values = num.trees_value,
                         primary_stability = result_stability,
                         selection_stability = irr::kappam.fleiss(sel_mat)$value,
                         computation_time = avg_time_taken)
    summary_result = rbind(summary_result, tmp_res)
  }
  colnames(summary_result)[2] = ifelse(method == "prediction", "prediction_stability","variable_importance_stability")
  return(list(summary_result, stability_definition))
}

.postprocess_rf_results = function(rf_result, X, config, visualisation, recommendation, rec_thresh, round_rec, verbose){
  summary_result = rf_result[[1]]
  stability_definition = rf_result[[2]]
  # (I) Visualisation
  if(visualisation == config$vis_key) create_stability_plot(summary_result[[config$col]], summary_result$num.trees_values, gsub("_", " ", config$col))
  if(visualisation == "selection") create_stability_plot(summary_result$selection_stability, summary_result$num.trees_values, "selection stability")
  # (II) Fit stability models
  predictionStab = NULL
  selectionStab = NULL
  if(nrow(summary_result) >= 4){
    variable_number = round(ncol(X), -2)
    test_seq = if(variable_number < 100000) seq(10, 1e6, 10) else seq(10, round((variable_number*100), -1), 10)
    primaryStab = fit_stability_model(summary_result, config$col, test_seq, visualisation == config$vis_key)
    selectionStab = fit_stability_model(summary_result, "selection_stability", test_seq, visualisation == "selection")
  }
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = summary_result)
  
  # (II) Prepare the output
  return(.create_output(method = config$method, stability_definition = stability_definition, result_table = summary_result, primaryStab, selectionStab, runtime_model, rec_thresh, round_rec, recommendation, verbose))
}