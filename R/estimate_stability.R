#' @title Estimate the stability of random forest
#'
#' @description Estimate the stability of random forest with certain numbers of trees
#'
#' @param optRF_object An optRF_object, either the result from the \link{opt_importance} or the \link{opt_prediction} function.
#' @param with_num.trees Either a single num.trees value or a vector containing multiple num.trees values for which the stability should be estimated.
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
#' @importFrom methods is
#' @importFrom stats lm
#' @export


estimate_stability = function(optRF_object, with_num.trees = c(1000, 5000, 10000, 50000, 100000)){

  # Check if the correct object was inserted
  if(!(is(optRF_object, "opt_prediction_object")) & !(is(optRF_object, "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  if(!is.numeric(with_num.trees) | any(with_num.trees < 1)){
    stop("The with_num.trees parameter needs to be a vector of positive numbers")
  }

  TwoPLmodel = function(with_num.trees, p1, p2){
    1 / (1+(p1/with_num.trees)^p2)
  }
  estimate_runtime = function(with_num.trees, p1, p2){
    as.numeric(p1 + with_num.trees*p2)
  }
  runtime_model = lm(optRF_object$result.table$run.time ~ optRF_object$result.table$num.trees_values)

  # estimate RF stability for prediction estimation
  if(is(optRF_object, "opt_prediction_object")){

    if(nrow(optRF_object$model.parameters) == 2){ # If a model for prediction and selection could be produced, produce estimates for both measures
      D_est = data.frame(num.trees = with_num.trees,
                         prediction_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                         selection_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                         run_time = estimate_runtime(with_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      return(D_est)
    }

    if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
      stop("The function opt_prediction could not model the relationship between the number of trees and prediction or selection stability.\n")
    }

    if(nrow(optRF_object$model.parameters) == 1){ # If only one model could be produced, estimate only the stability for the measure that could be modelled
      D_est = data.frame(num.trees = with_num.trees,
                         selection_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                         run_time = estimate_runtime(with_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      if(row.names(optRF_object$model.parameters) == "Prediction_stability"){
        colnames(D_est)[2] = "prediction_stability"
      }
      return(D_est)
    }

  }
  else{  # estimate RF stability for importance estimation
    if(nrow(optRF_object$model.parameters) == 2){ # If a model for variable importance and selection could be produced, produce a estimate both measures
      D_est = data.frame(num.trees = with_num.trees,
                         VI_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                         selection_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                         run_time = estimate_runtime(with_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2]))

      return(D_est)
    }

    if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
      stop("The function opt_prediction could not model the relationship between the number of trees and variable importance or selection stability.\n")
    }

    if(nrow(optRF_object$model.parameters) == 1){ # If only one model could be produced, estimate only the stability for the measure that could be modelled
      D_est = data.frame(num.trees = with_num.trees,
                         selection_stability = TwoPLmodel(with_num.trees, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                         run_time = estimate_runtime(with_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      if(row.names(optRF_object$model.parameters) == "VI_stability"){
        colnames(D_est)[2] = "VI_stability"
      }
      return(D_est)
    }
  }
}
