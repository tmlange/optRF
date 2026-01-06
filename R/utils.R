#' @param num.trees_values A vector containing the numbers of trees to be analysed. If not specified, 250, 500, 750, 1000, and 2000 trees will be analysed.
#' @param verbose Show computation status
#' @param response_type What data type is the response variable? Either "metric", "ordinal", or "categorical" are possible. If not set, the data type will be guessed 
#' @param ... Any other argument from the ranger function.
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

TwoPLmodel = function(vec, p1, p2){
  1 / (1+(p1/vec)^p2)
}

TwoPLmodel_inv = function(vec, p1, p2){
  p1 / (((1/vec)-1)^(1/p2))
}

estimate_runtime = function(vec, p1, p2){
  as.numeric(p1) + vec*as.numeric(p2)
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

