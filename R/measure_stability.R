#' @title Measure the stability of random forest
#'
#' @description Measure the stability of random forest for a certain data set with a certain number of trees
#'
#' @param num.trees Either a single value or a vector containing the numbers of trees for which the stability should be analysed (default = 500).
#' @param method Either "prediction" (default) or "importance" specifying if random forest should be used for prediction or to estimate the variable importance.
#' @param X_Test If method is "prediction", a data frame containing the explanatory variables of the test data set. If not entered, the out of bag data will be used.
#' @param alpha If method is "prediction", the number of best individuals to be selected in the test data set (default = 0.15), if method is "importance", the number of most important variables to be selected (default = 0.05).
#' @param select_for If method is "prediction", what should be selected? In random forest classification, this must be set to a vector containing the values of the desired classes. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param importance If method is "importance", the variable importance mode, one of "permutation" (default), "impurity" or "impurity_corrected".
#' @param verbose Show computation status.
#' @param ... Any other argument from the ranger function.
#' @inheritParams number_rep_helper
#' @inheritParams prediction_shared_parameters
#'
#' @return A data frame summarising the estimated stability for the given num.trees values.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' stability_result = measure_stability(y = SNPdata[,1], X=SNPdata[,-1], num.trees=500)
#' stability_result # Stability of random forest with 500 trees
#' }
#'
#' @export
#' @importFrom irr icc kappam.fleiss
#' @importFrom stats predict
#' @importFrom ranger ranger


measure_stability = function(y, X, num.trees=500, method=c("prediction","importance"), X_Test=NULL,
                             alpha = NULL, select_for = c("high", "low", "zero"),
                             importance = c("permutation", "impurity", "impurity_corrected"),
                             number_repetitions=10, verbose = TRUE, ...){

  # Check value of method
  method = match.arg(method)

  # Check value of number_repetitions
  number_repetitions = number_rep_helper(number_repetitions)

  # Check if y and X have the same number of observations
  if(!all.equal(nrow(X), length(y))){
    stop("Length of y does not equal number of rows of X \n")
  }

  if(!is.numeric(number_repetitions) | any(number_repetitions < 0)){
    stop("number_repetitions needs to be a positive number.")
  }

  if(!is.numeric(num.trees) | any(num.trees < 1)){
    stop("The num.tree_values need to be a vector of positive numbers.")
  }
  num.trees = ceiling(num.trees)

  # Run the analysis

  summary.result = data.frame()

  if(method=="prediction"){

    # Verify type of response variable y and the value of select_for
    if(is.numeric(y)){
      # Validate select_for for numeric y
      select_for = match.arg(select_for)
    }
    else if(is.factor(y)){
      # Validate select_for for categorical y
      if(missing(select_for) || !all(select_for %in% levels(y))){
        stop("For a categorical response variable, select_for must be a subset of its classes.")
      }
      select_for = unique(select_for)

      # Ensure select_for does not include all levels of y.
      if(length(select_for) == length(levels(y))){
        stop("select_for cannot include all classes of the categorical response variable.")
      }
    }
    else {
      stop("The response variable is neither numeric nor a factor.")
    }

    # Verify variables of the test data set
    if(is.null(X_Test)){
      if(verbose){
        message("No test data were entered. Out of bag data will be used.")
      }
      sample.IDs = paste0("ID_", c(1:nrow(X)))
    }
    else{
      if(ncol(X) != ncol(X_Test) | !all(colnames(X) %in% colnames(X_Test))){
        stop("X_Test needs to contain the same variables as X.")
      }
      sample.IDs = paste0("ID_", c(1:nrow(X_Test)))
    }

    if(is.null(alpha)){
      alpha = 0.15
    }

    if(!is.numeric(alpha) | any(alpha < 0)){
      stop("alpha needs to be a positive number.")
    }
    # Defining the number of individuals to be selected from the data set
    if(alpha < 1){
      selection.size = round(length(sample.IDs)*alpha)
    }
    else{
      selection.size = round(alpha)
    }

    for(i in 1:length(num.trees)){

      D_preds = data.frame(ID= sample.IDs)
      D_selection = data.frame(ID= sample.IDs)

      for(rep in 1:number_repetitions){

        if(verbose){
          message(paste0("Analysing random forest with ", num.trees[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
        }

        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees[i],
                           verbose = FALSE,
                           write.forest = TRUE,
                           keep.inbag = TRUE,
                           ...)

        if(is.null(X_Test)){
          all_predictions = predict(myForest, data = X, predict.all = TRUE)$predictions
          if(is.factor(y)){
            predictions = factor(character(length(y)), levels = levels(y))
          }
          else{
            predictions = numeric(length(y))
          }
          for(observation_number in 1:length(y)){
            inbag_counts = sapply(myForest[["inbag.counts"]], `[`, observation_number)
            keep.predictions = all_predictions[observation_number, inbag_counts == 0]
            if(is.factor(y)){
              predictions[observation_number] = levels(y)[which.max(table(keep.predictions))]
            }
            else{
              predictions[observation_number] = mean(keep.predictions)
            }
          }
        }
        else{
          predictions <- predict(myForest, data=X_Test)$predictions
        }

        # Creating the data frame to estimate the prediction stability (D_preds)
        tmp_D_preds = data.frame(predictions)
        names(tmp_D_preds) = paste0("Predictions_run_", rep)
        D_preds = cbind(D_preds, tmp_D_preds)

        # Creating the data frame to estimate the selection stability (D_selection)
        D_pred_test = data.frame(ID = sample.IDs, pred = predictions)

        if(is.numeric(y)){
          # Perform the selection
          if(select_for == "high"){
            D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=T),]
          }
          else if(select_for == "low"){
            D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=F),]
          }
          else{
            # If it is neither "low" nor "high", it must be "zero"
            # To analyse which predictions are closest to zero, calculate absolute values
            D_pred_test$pred = abs(D_pred_test$pred)
            D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=F),]
          }
          selection = D_pred_test$ID[1:selection.size]
        }
        else{
          selection = D_pred_test[D_pred_test$pred %in% select_for,]$ID
        }
        tmp_D_selection = data.frame(ID = sample.IDs)
        tmp_D_selection$selection = "rejected"
        tmp_D_selection[tmp_D_selection$ID %in% selection,]$selection = "selected"
        names(tmp_D_selection) = c("ID", paste0("Selections_in_run_", rep))
        D_selection = merge(D_selection, tmp_D_selection, by="ID")
      }

      # Removing the column with the IDs so that D_preds is a data frame that contains only the predictions
      D_preds = D_preds[,-1]

      # Removing the column with the IDs so that D_selection is a data frame that contains only the levels "selected" and "not_selected"
      D_selection = D_selection[,-1]

      # Summarising the results
      if(is.numeric(y)){
        pred_stability = icc(D_preds)$value
      }
      else{
        pred_stability = kappam.fleiss(D_preds)$value
      }
      tmp_res = data.frame(num.trees = num.trees[i],
                           pred_stability = pred_stability,
                           selection_stability = kappam.fleiss(D_selection)$value)
      summary.result = rbind(summary.result, tmp_res)
    }
  }
  else{

    # Check value of importance
    importance = match.arg(importance)

    # If y is neither numeric nor a factor, return an error message
    if(!is.numeric(y) & !is.factor(y)){
      stop("The response variable is neither numeric nor a factor")
    }

    if(is.null(alpha)){
      alpha = 0.05
    }
    if(!is.numeric(alpha) | any(alpha < 0)){
      stop("alpha needs to be a positive number.")
    }
    if(alpha < 1){
      selection.size = round(ncol(X)*alpha)
    }
    else{
      selection.size = round(alpha)
    }

    for(i in 1:length(num.trees)){

      D_VI = data.frame(variable.name = names(X))
      D_selection = data.frame(variable.name = names(X))
      for(rep in 1:number_repetitions){

        if(verbose){
          message(paste0("Analysing random forest with ", num.trees[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
        }

        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees[i],
                           importance = importance,
                           verbose = FALSE,
                           write.forest = TRUE,
                           ...)
        VI_result = data.frame(myForest$variable.importance)
        names(VI_result) = paste0("VI_run", rep)
        VI_result$variable.name = row.names(VI_result)

        VI_result = VI_result[order(VI_result$VI, decreasing=T),]
        selection = VI_result$variable.name[1:selection.size]
        tmp_D_selection = data.frame(variable.name = names(X))
        tmp_D_selection$selection = "rejected"
        tmp_D_selection[tmp_D_selection$variable.name %in% selection,]$selection = "selected"
        names(tmp_D_selection) = c("variable.name", paste0("Selections_in_run_", rep))
        D_selection = merge(D_selection, tmp_D_selection, by="variable.name")

        D_VI = merge(D_VI, VI_result, by="variable.name")
      }

      # Removing the column with the variable names so that D_VI is a data frame that contains only variable importance estimates
      D_VI = D_VI[,-1]

      # Removing the column with the IDs so that D_selection is a data frame that contains only the levels "selected" and "not_selected"
      D_selection = D_selection[,-1]

      tmp_res = data.frame(num.trees = num.trees[i],
                           VI_stability = icc(D_VI)$value,
                           selection_stability = kappam.fleiss(D_selection)$value)
      summary.result = rbind(summary.result, tmp_res)
    }
  }
  return(summary.result)
}


