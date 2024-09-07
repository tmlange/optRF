#' @export

summary.opt_importance_object = function(object, ...){

  # If the opt_importance function could give expectations for the RF_stability, summarise those
  if(!is.null(object$expected_RF_stability)){
    if(nrow(object$expected_RF_stability) == 4){
      cat("Recommended number of trees: ", object$recommendation, "\n",
          "Expected variable importance stability: ", object$expected_RF_stability[2], "\n",
          "Expected selection stability: ", object$expected_RF_stability[3], "\n",
          "Expected run time (sec): ", object$expected_RF_stability[4], sep = "")
    }
    if(nrow(object$expected_RF_stability) == 3){
      if(row.names(object$expected_RF_stability)[2] =="Variable_importance_stability"){
        cat("Recommended number of trees: ", object$recommendation, "\n",
            "Expected variable importance stability: ", object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", object$expected_RF_stability[3], sep = "")
      }
      if(row.names(object$expected_RF_stability)[2] =="Selection_stability"){
        cat("Recommended number of trees: ", object$recommendation, "\n",
            "Expected selection stability: ", object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", object$expected_RF_stability[3], sep = "")
      }
    }
  }


  # if the opt_importance function could not give expectations for the RF_stability, summarise the result table
  else{
    if(!is.null(object$result.table)){
      cat("Result of the opt_importance function: \n")
      return(object$result.table)
    }

    # If neither the expectations nor the result table exist, give an error message
    else{
      stop("Result of the opt_importance function cannot be summarised.\n Did an error occur while running opt_importance? \n")
    }
  }
}
