#' @title Estimate the stability of random forest
#'
#' @description Estimate the stability of random forest with certain numbers of trees
#'
#' @param with_num.trees Either a single num.trees value or a vector containing multiple num.trees values for which the stability should be estimated.
#' @inheritParams estimate_plot_shared_parameters
#'
#' @return A data frame summarising the estimated stability and run time in seconds for the given num.trees values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' estimate_stability(result_optpred, with_num.trees=c(1000, 5000, 10000, 50000, 100000))
#' }
#'
#' @export


estimate_stability = function(optRF_object, with_num.trees = c(1000, 5000, 10000, 50000, 100000)){

  # Check if the correct object was inserted
  if(!inherits(optRF_object, c("opt_prediction_object", "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  if(!is.numeric(with_num.trees) || any(with_num.trees < 1)){
    stop("The with_num.trees parameter needs to be a single positive number or a vector of positive numbers")
  }
  with_num.trees = ceiling(with_num.trees)

  model_params = optRF_object$model_parameters
  
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = optRF_object$result_table)

  # If no model could be produced, give an error message
  if(is.null(model_params) || nrow(model_params) == 0){ 
    func_name = ifelse(inherits(optRF_object, "opt_prediction_object"), "opt_prediction", "opt_importance")
    stop(paste0("The function ", func_name, " could not model the relationship between the number of trees and the stability."))
  }
  
  # Estimate RF stability
  est_list = lapply(seq_len(nrow(model_params)), function(i){
    TwoPLmodel(with_num.trees, model_params[i,1], model_params[i,2])
  })
  names(est_list) = rownames(model_params)
  D_est = data.frame(num.trees = with_num.trees, 
                     est_list, 
                     computation_time = estimate_runtime(with_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
  return(D_est)
}
