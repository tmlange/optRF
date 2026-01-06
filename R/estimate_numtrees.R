#' @title Estimate the required number of trees to reach a certain stability
#'
#' @description Estimate the number of trees required to achieve certain stability of random forest
#'
#' @param measure A character string indicating which stability measure is to be analysed. One of "selection" (default, analyses selection stability), "prediction" (analyses prediction stability) or "importance" (analyses variable importance stability).
#' @param for_stability Either a single stability value or a vector containing multiple stability values for which the number of trees should be estimated.
#' @inheritParams estimate_plot_shared_parameters
#'
#' @return A data frame summarising the estimated num.trees values and run time in seconds for the given stability values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' estimate_numtrees(result_optpred, measure="prediction", for_stability=0.95)
#' }
#'
#' @export


estimate_numtrees = function(optRF_object, measure = c("selection","importance","prediction"), for_stability = 0.95){

  if(!inherits(optRF_object, c("opt_prediction_object", "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  # Check value of measure
  measure = match.arg(measure)
  is_pred = inherits(optRF_object, "opt_prediction_object")
  target_measure = get_target_measure(measure, is_pred)
  
  # Check value of for_stability
  if(!is.numeric(for_stability) || any(for_stability < 0) || any(for_stability > 1)){
    stop("The for_stability parameter needs to be a single positive number or a vector of positive numbers between 0 and 1.")
  }

  model_params = optRF_object$model.parameters
  
  runtime_model = stats::lm(computation_time ~ num.trees_values, data = optRF_object$result.table)

  # If no model could be produced, give an error message
  if(is.null(model_params) || nrow(model_params) == 0){ 
    func_name = ifelse(inherits(optRF_object, "opt_prediction_object"), "opt_prediction", "opt_importance")
    stop(paste0("The function ", func_name, " could not model the relationship between the number of trees and the stability."))
  }
  
  # Check if model for selected stability measure exists
  if(target_measure %in% rownames(model_params)){
    use_measure = target_measure
  } else{
    # Give a warning if the non-selected stability is used for estimation
    use_measure = rownames(model_params)[1]
    warning(paste0("The optimal number of trees could not be estimated with the requested '", measure, 
                   "' stability. Estimation was instead performed using the '", 
                   gsub("_", "' ", use_measure), "."))
  }
  
  # Estimate required number of trees for RF
  opt_numtrees = TwoPLmodel_inv(for_stability, model_params[use_measure,1], model_params[use_measure,2])
  D_est = data.frame(stability = for_stability, 
                     opt_numtrees = ceiling(opt_numtrees),
                     computation_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
  colnames(D_est)[1] = use_measure
  return(D_est)
}
