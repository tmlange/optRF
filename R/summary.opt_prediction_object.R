#' @export

summary.opt_prediction_object = function(object, ...){

  # If the opt_prediction function could give expectations for the RF_stability, summarise those
  if(!is.null(object$expected_RF_stability)){
    if(nrow(object$expected_RF_stability) == 4){
      cat("Recommended number of trees: ", object$recommendation, "\n",
          "Expected prediction stability: ", object$expected_RF_stability[2], "\n",
          "Expected selection stability: ", object$expected_RF_stability[3], "\n",
          "Expected run time (sec): ", object$expected_RF_stability[4], sep = "")
    }
    if(nrow(object$expected_RF_stability) == 3){
      if(row.names(object$expected_RF_stability)[2] =="Prediction_stability"){
        cat("Recommended number of trees: ", object$recommendation, "\n",
            "Expected prediction stability: ", object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", object$expected_RF_stability[3], "\n", sep = "")

      }
      if(row.names(object$expected_RF_stability)[2] =="Selection_stability"){
        cat("Recommended number of trees: ", object$recommendation, "\n",
            "Expected selection stability: ", object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", object$expected_RF_stability[3], "\n", sep = "")
      }
    }
  }

  # if the opt_prediction function could not give expectations for the RF_stability, summarise the result table
  else{
    if(!is.null(object$result.table)){
      cat("Result of the opt_prediction function: \n")
      return(object$result.table)
    }
    else{
      stop("Result of the opt_prediction function cannot be summarised.\n Did an error occur while running opt_prediction? \n")
    }
  }
}
