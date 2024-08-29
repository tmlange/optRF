#' @title Estimate the required number of trees
#'
#' @description Estimate the number of trees required to achieve certain stability of random forest
#'
#' @param optRF_object An optRF_object, either the result from the \link{opt_importance} or the \link{opt_prediction} function.
#' @param measure A character string indicating which stability measure is to be analysed. One of "selection" (default, analyses selection stability), "prediction" (analyses prediction stability) or "importance" (analyses variable importance stability).
#' @param for_stability Either a single stability value or a vector containing multiple stability values for which the number of trees should be estimated.
#'
#' @return A data frame summarising the estimated stability and run time in seconds for the given num.trees values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' estimate_numtrees(result_optpred, measure="prediction", for_stability=0.95)
#' }
#'
#' @importFrom methods is
#' @importFrom stats lm
#' @export


estimate_numtrees = function(optRF_object, measure = c("selection","importance","prediction"), for_stability = 0.95){

  if(!(is(optRF_object, "opt_prediction_object")) & !(is(optRF_object, "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  # If the measure argument is invalid, give an error message
  if(identical(measure, c("selection", "importance", "prediction"))){
    measure = "selection"
  }
  if(length(measure) != 1 || !(measure %in% c("selection","importance","prediction"))){
    stop("Invalid input for measure. The measure must be either \"selection\", \"importance\", or \"prediction\".")
  }

  if(!is.numeric(for_stability) | any(for_stability < 0)){
    stop("The for_stability parameter needs to be a vector of positive numbers")
  }

  TwoPLmodel.inv = function(for_stability, p1, p2){
    p1/((1/for_stability)-1)^p2
  }
  estimate_runtime = function(at, p1, p2){
    as.numeric(p1 + at*p2)
  }
  runtime_model = lm(optRF_object$result.table$run.time ~ optRF_object$result.table$num.trees_values)

  # estimate RF stability for prediction estimation
  if(is(optRF_object, "opt_prediction_object")){
    # If the measure was set to be importance, this will not work
    if(measure == "importance"){
      stop("The variable importance stability cannot be plotted with an object created with the function opt_prediction.\nPlease set the measure argument to either \"prediction\" or \"selection\". \n")
    }

    if(nrow(optRF_object$model.parameters) == 2){ # If a model for prediction and selection could be produced, produce estimates for both measures

      if(measure == "prediction"){
      opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2])
      D_est = data.frame(prediction_stability = for_stability,
                         opt_numtrees = ceiling(opt_numtrees),
                         run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      return(D_est)
      }

      if(measure == "selection"){
        opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2])
        D_est = data.frame(selection_stability = for_stability,
                           opt_numtrees = ceiling(opt_numtrees),
                           run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
        return(D_est)
      }
    }

    if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
      stop("The function opt_prediction could not model the relationship between the number of trees and prediction or selection stability.\n")
    }

    if(nrow(optRF_object$model.parameters) == 1){ # If only one model could be produced, estimate only the stability for the measure that could be modelled
      opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2])
      D_est = data.frame(selection_stability = for_stability,
                         opt_numtrees = ceiling(opt_numtrees),
                         run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      if(row.names(optRF_object$model.parameters) == "Prediction_stability"){
        colnames(D_est)[2] = "prediction_stability"
      }
      return(D_est)
    }
  }
  else{ # estimate RF stability for importance estimation
    # If the measure was set to be importance, this will not work
    if(measure == "prediction"){
      stop("The prediction stability cannot be plotted with an object created with the function opt_importance.\nPlease set the measure argument to either \"importance\" or \"selection\". \n")
    }

    if(nrow(optRF_object$model.parameters) == 2){ # If a model for prediction and selection could be produced, produce estimates for both measures

      if(measure == "importance"){
        opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2])
        D_est = data.frame(VI_stability = for_stability,
                           opt_numtrees = ceiling(opt_numtrees),
                           run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
        return(D_est)
      }

      if(measure == "selection"){
        opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2])
        D_est = data.frame(selection_stability = for_stability,
                           opt_numtrees = ceiling(opt_numtrees),
                           run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
        return(D_est)
      }
    }

    if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
      stop("The function opt_importance could not model the relationship between the number of trees and variable importance or selection stability.\n")
    }

    if(nrow(optRF_object$model.parameters) == 1){ # If only one model could be produced, estimate only the stability for the measure that could be modelled
      opt_numtrees = TwoPLmodel.inv(for_stability, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2])
      D_est = data.frame(selection_stability = for_stability,
                         opt_numtrees = ceiling(opt_numtrees),
                         run_time = estimate_runtime(opt_numtrees, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      if(row.names(optRF_object$model.parameters) == "VI_stability"){
        colnames(D_est)[2] = "VI_stability"
      }
      return(D_est)
    }
  }
}
