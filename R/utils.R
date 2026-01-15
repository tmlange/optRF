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

select_for_helper = function(y, response_type, select_for = c("high", "low", "zero"), alpha){
  if(response_type == "metric"){
    # Validate select_for for numeric y
    select_for = match.arg(select_for)
    if(!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= length(y)){
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

.run_rf_engine = function(y, X, X_Test = NULL, method = c("prediction", "importance"),
                         number_repetitions,
                         num.trees_values,
                         visualisation = c("primary","selection"),
                         recommendation = c("primary","selection"),
                         rec_thresh = 1e-6, round_recommendation = c("thousand","hundred","ten","none"),
                         stability_metric, verbose = TRUE, ...
                         ){
  
  # (I) Input validation
  
  round_rec = round_rec_helper(round_recommendation)
  number_repetitions = number_rep_helper(number_repetitions)
  rec_thresh = rec_thresh_helper(rec_thresh)
  num.trees_values = num.trees_values_helper(num.trees_values)
  if(nrow(X) != length(y)) stop("Invalid input. Number of rows in 'X' does not match length of 'y'.")
  
  # Check value of y and response_type
  response_result = response_type_helper(response_type, y)
  y <- response_result$y
  response_type <- response_result$response_type
  # Create test sequence
  variable_number = round(ncol(X), -2)
  test_seq = if(variable_number < 100000) seq(10, 1e6, 10) else seq(10, round((variable_number*100), -1), 10)
  
  # (II) Run the analysis
  summary_result = data.frame()
  predictionStab = NULL
  selectionStab = NULL
  
  for(i in 1:length(num.trees_values)){
    pred_mat = matrix(NA, nrow = length(sample_IDs), ncol = number_repetitions)
    sel_mat = matrix("rejected", nrow = length(sample_IDs), ncol = number_repetitions)
    time_taken_vec = numeric(number_repetitions)
    for(rep in 1:number_repetitions){
      
      if(verbose){
        message(paste0("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
      }
      
      start.time = Sys.time()
      if(response_type == "ordinal"){
        myForest <- ordinalForest::ordfor(depvar = "y", data = data.frame(y = y, X),
                                          nsets = num.trees_values[i], ...)
        
        if(is.null(X_Test)){
          predictions <- myForest$ypred_oob
        } else {
          predictions <- predict(myForest, newdata = X_Test)$ypred
        }
      } else{
        myForest <- ranger::ranger(x = X,
                                   y = y,
                                   num.trees = num.trees_values[i],
                                   verbose = FALSE, ...)
        runif(1, 0, .Machine$integer.max) #Uncomment to reproduce original results
        if(is.null(X_Test)){
          predictions = myForest$predictions
        } else{
          predictions <- predict(myForest, data=X_Test)$predictions
        }
      }
      time_taken_vec[rep] = as.numeric(difftime(Sys.time(), start.time, units = "secs"))
      pred_mat[, rep] = predictions
      
      # Selection logic
      if(response_type == "metric"){
        ranks = if(select_for == "high") rank(-predictions, ties.method = "first") else
          if(select_for == "low") rank(predictions, ties.method = "first") else
            rank(abs(predictions), ties.method = "first")
        selected_idx = which(ranks <= selection_size)
      } else if(response_type == "ordinal"){
        selected_idx = if(select_for == "high") which(predictions >= alpha) else which(predictions <= alpha)
      } else{ # categorical
        selected_idx = which(predictions %in% select_for)
      }
      sel_mat[selected_idx, rep] = "selected"
    }
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
    tmp_res = data.frame(num.trees_values = num.trees_values[i],
                         pred_stability = pred_stability,
                         selection_stability = kappam.fleiss(sel_mat)$value,
                         computation_time = mean(time_taken_vec))
    summary_result = rbind(summary_result, tmp_res)
  }
}