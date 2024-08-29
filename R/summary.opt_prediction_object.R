#' @export

summary.opt_prediction_object = function(optRF_object){

  # If the opt_prediction function could give expectations for the RF_stability, summarise those
  if(!is.null(optRF_object$expected_RF_stability)){
    if(nrow(optRF_object$expected_RF_stability) == 4){
      cat("Recommended number of trees: ", optRF_object$recommendation, "\n",
          "Expected prediction stability: ", optRF_object$expected_RF_stability[2], "\n",
          "Expected selection stability: ", optRF_object$expected_RF_stability[3], "\n",
          "Expected run time (sec): ", optRF_object$expected_RF_stability[4], sep = "")
    }
    if(nrow(optRF_object$expected_RF_stability) == 3){
      if(row.names(optRF_object$expected_RF_stability)[2] =="Prediction_stability"){
        cat("Recommended number of trees: ", optRF_object$recommendation, "\n",
            "Expected prediction stability: ", optRF_object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", optRF_object$expected_RF_stability[3], "\n", sep = "")

      }
      if(row.names(optRF_object$expected_RF_stability)[2] =="Selection_stability"){
        cat("Recommended number of trees: ", optRF_object$recommendation, "\n",
            "Expected selection stability: ", optRF_object$expected_RF_stability[2], "\n",
            "Expected run time (sec): ", optRF_object$expected_RF_stability[3], "\n", sep = "")
      }
    }
  }

  # if the opt_prediction function could not give expectations for the RF_stability, summarise the result table
  else{
    if(!is.null(optRF_object$result.table)){
      cat("Result of the opt_prediction function: \n")
      return(optRF_object$result.table)
    }
    else{
      stop("Result of the opt_prediction function cannot be summarised.\n Did an error occur while running opt_prediction? \n")
    }
  }
}
