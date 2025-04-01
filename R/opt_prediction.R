#' @title Optimise random forest for prediction
#'
#' @description Optimising random forest predictions by calculating the prediction stability with certain numbers of trees
#'
#' @param X_Test A data frame containing the explanatory variables of the test data set. If not entered, the out of bag data will be used.
#' @param alpha The number of best individuals to be selected in the test data set based on their predicted response values. If < 1, alpha will be considered to be the relative amount of individuals in the test data set.
#' @param visualisation Can be set to "prediction" to draw a plot of the prediction stability or "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param select_for What should be selected? In random forest classification, this must be set to a vector containing the values of the desired classes. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param recommendation If set to "prediction" (default) or "selection", a recommendation will be given based on optimised prediction or selection stability. If set to be "none", the function will analyse the stability of random forest with the inserted numbers of trees without giving a recommendation.
#' @inheritParams round_rec_helper
#' @inheritParams number_rep_helper
#' @inheritParams rec_thresh_helper
#' @inheritParams opt_shared_parameters
#' @inheritParams prediction_shared_parameters
#'
#' @return An opt_prediction_object containing the recommended number of trees, based on which measure the recommendation was given (prediction or selection), a matrix summarising the estimated stability and computation time of a random forest with the recommended numbers of trees, a matrix containing the calculated stability and computation time for the analysed numbers of trees, and the parameters used to model the relationship between stability and numbers of trees.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' summary(result_optpred)
#' }
#'
#' @export
#' @importFrom irr icc kappam.fleiss
#' @importFrom stats predict
#' @importFrom graphics points
#' @importFrom ranger ranger
#' @importFrom minpack.lm nlsLM nls.lm.control




opt_prediction = function(y, X, X_Test=NULL,
                          number_repetitions = 10, alpha = 0.15,
                          num.trees_values = c(250, 500, 750, 1000, 2000), visualisation = c("none","prediction","selection"), select_for = c("high", "low", "zero"),
                          recommendation = c("prediction","selection", "none"),
                          rec_thresh = 1e-6, round_recommendation = c("thousand","hundred","ten","none"), verbose = TRUE, ...){

  rec.num.trees = NA

  # Defining to what number the recommendation of number of trees should be rounded to
  round_rec = round_rec_helper(round_recommendation)

  # Check value of visualisation
  visualisation = match.arg(visualisation)

  # Check value of recommendation
  recommendation = match.arg(recommendation)

  # Check value of number_repetitions
  number_repetitions = number_rep_helper(number_repetitions)

  # Check value of rec_thresh
  rec_thresh = rec_thresh_helper(rec_thresh)

  # Check if y and X have the same number of observations
  if(!all.equal(nrow(X), length(y))){
    stop("Length of y does not equal number of rows of X \n")
  }

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

  variable.number <- round(ncol(X), -2)

  if(!is.numeric(num.trees_values) | any(num.trees_values < 1)){
    stop("The num.tree_values need to be a vector of positive numbers.")
  }
  num.trees_values = ceiling(num.trees_values)

  if(variable.number < 100000){
    test_seq = seq(10, 1000000, 10)
  }
  if(variable.number > 100000){
    test_seq = seq(10, round((variable.number*100), -1), 10)
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

  # Run the analysis

  summary.result = data.frame()
  for(i in 1:length(num.trees_values)){
    D_preds = data.frame(ID= sample.IDs)
    D_selection = data.frame(ID= sample.IDs)
    time.taken = 0
    for(rep in 1:number_repetitions){

      if(verbose){
        message(paste0("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
      }

      start.time = Sys.time()
      myForest <- ranger(x=X,
                         y=y,
                         num.trees = num.trees_values[i],
                         verbose = FALSE,
                         write.forest = TRUE,
                         keep.inbag = TRUE,
                         ...)
      time.taken = time.taken + as.numeric(difftime(Sys.time(), start.time, units = "secs"))
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
    tmp_res = data.frame(num.trees_values = num.trees_values[i],
                         pred_stability = pred_stability,
                         selection_stability = kappam.fleiss(D_selection)$value,
                         computation_time = time.taken/number_repetitions)
    summary.result = rbind(summary.result, tmp_res)

    if(visualisation == "prediction"){
      create_stability_plot(summary.result$pred_stability, summary.result$num.trees_values, "prediction stability")
    }

    if(visualisation == "selection"){
      create_stability_plot(summary.result$selection_stability, summary.result$num.trees_values, "selection stability")
    }

    # If there are more than four data points, model the relationship(s)
    if(nrow(summary.result) >= 4){

      # non linear modelling of the relationship between prediction stability and num.trees values
      tryCatch({
        non.lin.mod.pv = non_linear_modelling(summary.result, "pred_stability", test_seq, visualisation == "prediction")
        D_est.pv = data.frame(num.trees = test_seq,
                              estimated_prediction_stability = TwoPLmodel(test_seq, non.lin.mod.pv$m$getPars()[1], non.lin.mod.pv$m$getPars()[2]))
      }, error=function(e){})

      # non linear modelling of the relationship between selection stability and num.trees values
      tryCatch({
        non.lin.mod.sv = non_linear_modelling(summary.result, "selection_stability", test_seq, visualisation == "selection")
        D_est.sv = data.frame(num.trees = test_seq,
                              estimated_selection_stability = TwoPLmodel(test_seq, non.lin.mod.sv$m$getPars()[1], non.lin.mod.sv$m$getPars()[2]))
      }, error=function(e){})

      # linear modelling of the relationship between run time and num.trees values
      tryCatch({
        runtime_model = lm(summary.result$computation_time ~ summary.result$num.trees_values)
        D_est.rt = data.frame(num.trees = test_seq,
                              estimated_run_time = estimate_runtime(test_seq, runtime_model$coefficients[1], runtime_model$coefficients[2]))
      }, error=function(e){})

    }
  }
  # After all num.trees_values have been analysed, give a recommendation

  # If recommendation should be done with the prediction stability, optimise numbers of trees based on estimated prediction stability
  if(recommendation == "prediction"){

    if(!exists('D_est.pv')){
      warning("A recommendation cannot be given because the relationship between prediction stability and numbers of trees could not be modelled.")
    }

    # Try to perform a recommendation using a non-linear model
    tryCatch({

      # Calculate the increase of prediction stability per increase of trees
      D_est.pv$diff = c(NA,diff(D_est.pv$estimated_prediction_stability)/10)
      D_est.pv = D_est.pv[-1,]

      # Finally, make a recommendation
      new.rec_thresh = rec_thresh
      trust.rec = FALSE
      while(trust.rec == FALSE){

        rec.num.trees = round(D_est.pv[D_est.pv$diff<new.rec_thresh,]$num.trees[1], round_rec)

        # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
        if(rec.num.trees >= non.lin.mod.pv$m$getPars()[1]){
          if(exists('D_est.sv')){
            estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
          }
          estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
          estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
          trust.rec = TRUE
        }

        # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
        if(rec.num.trees < non.lin.mod.pv$m$getPars()[1]){
          new.rec_thresh = new.rec_thresh*0.1
        }
      }
    }, error=function(e){})
  }

  # If recommendation should be done with the selection stability, optimise numbers of trees based on estimated selection stability
  if(recommendation == "selection"){

    if(!exists('D_est.sv')){
      warning("A recommendation cannot be given because the relationship between selection stability and numbers of trees could not be modelled.")
    }

    # Try to perform a recommendation using a non-linear model
    tryCatch({

      # Calculate the increase of selection stability per increase of trees
      D_est.sv$diff = c(NA,diff(D_est.sv$estimated_selection_stability)/10)
      D_est.sv = D_est.sv[-1,]

      # Finally, make a recommendation
      new.rec_thresh = rec_thresh
      trust.rec = FALSE
      while(trust.rec == FALSE){

        rec.num.trees = round(D_est.sv[D_est.sv$diff<new.rec_thresh,]$num.trees[1], round_rec)

        # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
        if(rec.num.trees >= non.lin.mod.sv$m$getPars()[1]){
          if(exists('D_est.pv')){
            estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
          }
          estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
          estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
          trust.rec = TRUE
        }

        # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
        if(rec.num.trees < non.lin.mod.sv$m$getPars()[1]){
          new.rec_thresh = new.rec_thresh*0.1
        }
      }
    }, error=function(e){})
  }

  # Create the output based on the recommended number of trees
  if(!is.na(rec.num.trees)){

    # If the recommended number of trees is for some reason lower than 500 (default), set it to be 500
    if(rec.num.trees < 500){
      rec.num.trees = 500
    }

    if(verbose){
      message("\n Recommended number of trees: ", rec.num.trees)
    }

    # Create output

    # if prediction and selection stability could be modeled
    if(exists('D_est.pv') & exists('D_est.sv')){
      modelpara.matrix = matrix(c(non.lin.mod.pv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_selection_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Selection_stability", "Computation_time")
    }

    # if prediction stability could be modeled but selection stability could not be modeled
    if(exists('D_est.pv') & !exists('D_est.sv')){
      warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
      modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Prediction_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Computation_time")
    }

    # If selection stability could be modeled but prediction stability could not
    if(!exists('D_est.pv') & exists('D_est.sv')){
      warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")
      modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Selection_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_selection_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Selection_stability", "Computation_time")
    }
    colnames(modelpara.matrix) = c("Inflection_point", "Slope")
    colnames(RFstab.matrix) = c("Value")

    output = list(rec.num.trees, recommendation, RFstab.matrix, summary.result, modelpara.matrix)
    names(output) = c("recommendation", "recommendation_for", "expected_RF_stability", "result.table", "model.parameters")
    class(output) = "opt_prediction_object"
    return(output)
  }
  # If no recommendation could be made, create output without the recommendation
  if(is.na(rec.num.trees)){

    # if prediction and selection stability could be modeled
    if(exists('D_est.pv') & exists('D_est.sv')){
      modelpara.matrix = matrix(c(non.lin.mod.pv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")
    }

    # if prediction stability could be modeled but selection stability could not be modeled
    if(exists('D_est.pv') & !exists('D_est.sv')){
      modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Prediction_stability")
      warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
    }

    # if prediction stability could not be modeled but selection stability could be modeled
    if(!exists('D_est.pv') & exists('D_est.sv')){
      modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Selection_stability")
      warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")
    }

    # if neither prediction nor selection stability could be modeled
    if(!exists('D_est.pv') & !exists('D_est.sv')){
      warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values as well as between selection stability and num.trees values\n")
      output = list(summary.result)
      names(output) = c("result.table")
    }
    else{
      colnames(modelpara.matrix) = c("Inflection_point", "Slope")
      output = list(summary.result, modelpara.matrix)
      names(output) = c("result.table", "model.parameters")
    }

    class(output) = "opt_prediction_object"
    return(output)
  }
}
