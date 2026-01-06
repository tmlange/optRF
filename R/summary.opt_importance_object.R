#' @export

summary.opt_importance_object = function(object, ...){

  # If the opt_importance function could give expectations for the RF stability, summarise those
  if(!is.null(object$expected_RF_stability)){
    cat("Recommended number of trees: ", object$recommendation, "\n", sep = "")
    .print_metric(object$expected_RF_stability, "Expected variable importance stability", "Variable_importance_stability")
    .print_metric(object$expected_RF_stability, "Expected selection stability", "Selection_stability")
    .print_metric(object$expected_RF_stability, "Expected computation time (sec)", "Computation_time")
  }
  # if the opt_importance function could not give expectations for the RF stability, summarise the result table
  else if(!is.null(object$result.table)){
      cat("Result of the opt_importance function: \n")
      return(object$result.table)
  }
  # If neither the expectations nor the result table exist, give an error message
  else{
    stop("Result of the opt_importance function cannot be summarised.\n Did an error occur while running opt_importance?\n")
  }
}
