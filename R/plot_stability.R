#' @title Plot random forest stability
#'
#' @description Plot the estimated stability of random forest against certain numbers of trees
#'
#' @param measure A character string indicating which stability measure is to be plotted. One of "selection" (default, visualises selection stability), "prediction" (visualises prediction stability) or "importance" (visualises variable importance stability).
#' @param from Smallest num.trees value to be plotted.
#' @param to Greatest num.trees value to be plotted.
#' @param add_recommendation When set as TRUE, if a recommendation was stated within the opt_prediction or opt_importance function, the recommended num.trees value as well as the expected random forest stability will be highlighted in the graph
#' @param add If FALSE, a new plot will be created, if TRUE, the graph will be added to an existing plot.
#' @param ... Any other arguments from the plot function.
#' @inheritParams estimate_plot_shared_parameters
#'
#' @return A plot showing the estimated stability of random forest for the given num.trees values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' plot_stability(result_optpred, measure = "prediction", add_recommendation = TRUE, add=FALSE)
#' plot_stability(result_optpred, measure = "selection",  add_recommendation = FALSE, add=TRUE)
#' }
#'
#' @export

plot_stability = function(optRF_object, measure = c("selection","importance","prediction"),
                          from = 1, to = 100000, add_recommendation = TRUE,
                          add = FALSE, ...){

  # Check if the correct object was inserted
  if(!inherits(optRF_object, c("opt_prediction_object", "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  # Check value of measure
  measure = match.arg(measure)
  is_pred = inherits(optRF_object, "opt_prediction_object")
  target_measure = get_target_measure(measure, is_pred)

  # Check values of from and to
  if (!is.numeric(from) || length(from) != 1 || from < 1) {
    stop("Invalid input: 'from' must be a single positive number.")
  }
  if (!is.numeric(to) || length(to) != 1 || to <= from) {
    stop("Invalid input: 'to' must be a single positive number greater than the 'from' value.")
  }
  
  model_params = optRF_object$model_parameters
  
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
                   "' stability. Plotting was instead performed using the '", 
                   gsub("_", "' ", use_measure), "."))
  }
  
  # Calculate values and plot them
  plot_seq = seq(from, to, 1)
  stability_values = TwoPLmodel(plot_seq, model_params[use_measure,1], model_params[use_measure,2])
  
  if(add == FALSE){
    plot(stability_values ~ plot_seq,
         type = "l", ylab = paste0("RF ", gsub("_", " ", use_measure)), xlab = "Number of trees", main = paste0("Relationship between\n RF ", gsub("_", " ", use_measure)," and number of trees"), ...)
  } else{
    graphics::lines(stability_values ~ plot_seq, ...)
  }
  if(add_recommendation == TRUE && !is.null(optRF_object$recommendation)){
    graphics::abline(v = optRF_object$recommendation, col=grDevices::rgb(1, 0, 0, 0.5))
    graphics::abline(h = TwoPLmodel(optRF_object$recommendation, model_params[use_measure,1], model_params[use_measure,2]),
           col=grDevices::rgb(1, 0, 0, 0.5))
  }
}
