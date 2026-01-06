#' @title Optimise random forest predictions
#'
#' @description Optimising random forest predictions by calculating the prediction stability with certain numbers of trees
#'
#' @param X_Test A data frame containing the explanatory variables of the test data set. If not entered, the out of bag data will be used.
#' @param alpha The number of best individuals to be selected in the test data set based on their predicted response values. If < 1, alpha will be considered to be the relative amount of individuals in the test data set.
#' @param visualisation Can be set to "prediction" to draw a plot of the prediction stability or "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param select_for What should be selected? In random forest classification, this must be set to a vector containing the values of the desired classes. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param recommendation If set to "prediction" (default) or "selection", a recommendation will be given based on optimised prediction or selection stability. If set to be "none", the function will analyse the stability of random forest with the inserted numbers of trees without giving a recommendation.
#' @param Krippendorf If the response is metric, should the prediction stability be calculated as the intraclass correlation coefficient (default) or as Krippendorf's alpha? If Krippendorf's alpha should be computed, the parameter "Krippendorf" must be set to either "interval" or "ratio" depending on the scale of the response variable. 
#' @param rank_based If the response is metric, should the prediction stability be defined by the similarity of the predicted values via the intraclass correlation coefficient (rank_based == FALSE, default) or by the rankings of the objects via Kendall's W (rank_based == TRUE)?
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
#' @importFrom irr icc kappam.fleiss kripp.alpha kendall
#' @importFrom stats predict
#' @importFrom graphics points
#' @importFrom ranger ranger
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom ordinalForest ordfor



opt_prediction = function(y, X, X_Test=NULL,
                          number_repetitions = 10, alpha = 0.15,
                          num.trees_values = c(250, 500, 750, 1000, 2000), 
                          visualisation = c("none","prediction","selection"), 
                          select_for = c("high", "low", "zero"),
                          recommendation = c("prediction","selection", "none"),
                          rec_thresh = 1e-6, round_recommendation = c("thousand","hundred","ten","none"), 
                          response_type = NULL, Krippendorf = NULL, rank_based = FALSE,
                          verbose = TRUE, ...){
  
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
  
  # Check the response variable
  if(is.numeric(y)){
    # Validate select_for for numeric y
    select_for = match.arg(select_for)
    # Define the response as metric if it has not been set by the user
    if(is.null(response_type)){
      response_type = "metric"
    }
  }
  else if(is.character(y) | is.factor(y)){
    # Validate select_for for categorical y
    if(missing(select_for) || !all(select_for %in% unique(y))){
      stop("For a categorical response variable, select_for must be a subset of its classes.")
    }
    select_for = unique(select_for)
    # Ensure select_for does not include all levels of y.
    if(length(select_for) == length(levels(y))){
      stop("select_for cannot include all classes of the categorical response variable.")
    }
    if(is.character(y)){
      y = as.factor(y)
    }
    # Define the response as categorical if it has not been set by the user
    if(is.null(response_type)){
      response_type = "categorical"
    }
  }
  else if(!is.character(y) & !is.numeric(y) & !is.factor(y)){
    stop("The response variable is neither categorical (character or factor) nor numeric.")
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
  
  # Check alpha
  if(response_type == "metric"){
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
  }
  if(response_type == "ordinal"){
    if(!is.numeric(alpha)){
      stop("For ordinal scaled response variables, alpha must be numeric.")
    }
    
    if(alpha < min(y) | alpha > max(y)){
      stop("alpha must be in the range of the response variable; min(y) <= alpha <= max(y).")
    }
  }
  
  # Check if the test data set consists of multipe objects
  if(is.null(X_Test)){
    MOPS.analysis = TRUE
  }
  else{
    if(nrow(X_Test) > 1){
      MOPS.analysis = TRUE
    }
    else{
      MOPS.analysis = FALSE
    }
  }
  
  # Check the value for Krippendorf
  if(!is.null(Krippendorf)){
    if(!(Krippendorf %in% c("ratio", "interval"))){
      stop("Invalid value for 'Krippendorf'; must be 'ratio', 'interval', or NULL")
    }
  }
  
  # Check the value for rank_based
  if(rank_based != TRUE & rank_based != FALSE){
    stop("Invalid value for 'rank_based'; must be TRUE or FALSE")
  }
  
  # Run the analysis
  
  summary.result = data.frame()
  for(i in 1:length(num.trees_values)){
    D_preds = data.frame(ID= sample.IDs)
    D_selection = data.frame(ID= sample.IDs)
    time.taken = 0
    
    if(MOPS.analysis == FALSE){
      prediction_vector = vector()
    }
    for(rep in 1:number_repetitions){
      
      if(verbose){
        message(paste0("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
      }
      
      start.time = Sys.time()
      if(response_type == "ordinal"){
        y = factor(y, levels = sort(unique(y)), ordered = TRUE)
        ordfor_data <- data.frame(y = y, X)
        
        myForest <- ordfor(depvar="y", data=ordfor_data,
                        nsets = num.trees_values[i], ...)
        
        if(is.null(X_Test)){
          predictions <- myForest$ypred_oob
        } else {
          predictions <- predict(myForest, newdata = X_Test)$ypred
        }
        
        if(MOPS.analysis == FALSE){
          prediction_vector = c(prediction_vector, predictions)
        }
      }
      if(response_type != "ordinal"){
        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees_values[i],
                           verbose = FALSE,
                           write.forest = TRUE,
                           keep.inbag = TRUE)
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
        
        if(MOPS.analysis == FALSE){
          prediction_vector = c(prediction_vector, predictions)
        }
      }
      time.taken = time.taken + as.numeric(difftime(Sys.time(), start.time, units = "secs"))
      
      # Analysis of prediction stability and selection stability for multiple test objects 
      if(MOPS.analysis == TRUE){
        
        # Creating the data frame to estimate the prediction stability (D_preds)
        tmp_D_preds = data.frame(predictions)
        names(tmp_D_preds) = paste0("Predictions_run_", rep)
        D_preds = cbind(D_preds, tmp_D_preds)
        
        # Creating the data frame to estimate the selection stability (D_selection)
        D_pred_test = data.frame(ID = sample.IDs, pred = predictions)
        
        if(response_type == "metric"){
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
        if(response_type == "ordinal"){
          if(select_for == "high"){
            selection = D_pred_test[D_pred_test$pred >= alpha,]$ID
          }
          else{
            selection = D_pred_test[D_pred_test$pred <= alpha,]$ID
          }
        }
        if(response_type == "categorical"){
          selection = D_pred_test[D_pred_test$pred %in% select_for,]$ID
        }
        tmp_D_selection = data.frame(ID = sample.IDs)
        tmp_D_selection$selection = "rejected"
        tmp_D_selection[tmp_D_selection$ID %in% selection,]$selection = "selected"
        names(tmp_D_selection) = c("ID", paste0("Selections_in_run_", rep))
        D_selection = merge(D_selection, tmp_D_selection, by="ID")
      }
    }
    
    # Create the data frame summary.result for multiple objects
    if(MOPS.analysis == TRUE){
      # Removing the column with the IDs so that D_preds is a data frame that contains only the predictions
      D_preds = D_preds[,-1]
      
      # Removing the column with the IDs so that D_selection is a data frame that contains only the levels "selected" and "not_selected"
      D_selection = D_selection[,-1]
      
      # Calculating the prediction stability
      if(response_type == "metric"){
        if(is.null(Krippendorf) & rank_based == FALSE){
          pred_stability = icc(D_preds)$value
          ps_definition = "ICC"
        }
        if(is.null(Krippendorf) & rank_based == TRUE){
          pred_stability = kendall(D_preds)$value
          ps_definition = "Kendalls_W"
        }
        if(!is.null(Krippendorf)){
          D_preds = as.matrix(D_preds)
          D_preds = t(D_preds)
          pred_stability = kripp.alpha(D_preds, method=Krippendorf)$value
          ps_definition = "Krippendorfs_alpha"
        }
      }
      if(response_type == "ordinal"){
        D_preds = as.matrix(D_preds)
        D_preds = t(D_preds)
        pred_stability = kripp.alpha(D_preds, method="ordinal")$value
        ps_definition = "Krippendorfs_alpha"
      }
      if(response_type == "categorical"){
        pred_stability = kappam.fleiss(D_preds)$value
        ps_definition = "Fleiss_Kappa"
      }
      tmp_res = data.frame(num.trees_values = num.trees_values[i],
                           pred_stability = pred_stability,
                           selection_stability = kappam.fleiss(D_selection)$value,
                           computation_time = time.taken/number_repetitions)
      summary.result = rbind(summary.result, tmp_res)
    }
    
    # Create the data frame summary.result for a single objects
    if(MOPS.analysis == FALSE){
      SOPS_value = 1/(1+(sd(prediction_vector)/mean(prediction_vector)))
      ps_definition = "Single_Object_Prediction_Stability"
      
      tmp_res = data.frame(num.trees_values = num.trees_values[i],
                           pred_stability = SOPS_value,
                           computation_time = time.taken/number_repetitions)
      summary.result = rbind(summary.result, tmp_res)
    }
  }
  
  if(visualisation == "prediction"){
    create_stability_plot(summary.result$pred_stability, summary.result$num.trees_values, "prediction stability")
  }
  
  if(visualisation == "selection"){
    create_stability_plot(summary.result$selection_stability, summary.result$num.trees_values, "selection stability")
  }
  
  # If there are more than four data points, model the relationship(s)
  if(nrow(summary.result) >= 4){
    
    # non linear modelling of the relationship between prediction stability and num.trees values
    predictionStab = fit_stability_model(summary.result, "pred_stability", test_seq, visualisation == "prediction")
    
    # non linear modelling of the relationship between selection stability and num.trees values
    selectionStab = fit_stability_model(summary.result, "selection_stability", test_seq, visualisation == "selection")
    
    # linear modelling of the relationship between run time and num.trees values
    runtime_model = lm(summary.result$computation_time ~ summary.result$num.trees_values)
  }
  if(is.null(predictionStab)){
    message("predictionStab doesn't exist")
  }
  if(is.null(selectionStab)){
    message("selectionStab doesn't exist")
  }
  # After all num.trees_values have been analysed, give a recommendation
  recommended_num.trees = NA
  # If recommendation should be done with the prediction stability, optimise numbers of trees based on estimated prediction stability
  if(recommendation == "prediction" && !is.null(predictionStab)){
    recommended_num.trees = find_recommendation(predictionStab$estimates, predictionStab$model, rec_thresh, round_rec)
  } else if(recommendation == "selection" && !is.null(selectionStab)){
    recommended_num.trees = find_recommendation(selectionStab$estimates, selectionStab$model, rec_thresh, round_rec)
  } else if(recommendation != "none"){
    warning("A recommendation cannot be given because the relationship between the requested stability and numbers of trees could not be modelled.")
  }
  
  # Create output
  # Base output
  output = list(prediction_stability_definition = ps_definition, result_table = summary.result)
  # Add model parameters if available
  model_params = list()
  if(!is.null(predictionStab)) model_params[["Prediction_stability"]] = predictionStab$model$m$getPars()
  if(!is.null(selectionStab)) model_params[["Selection_stability"]] = selectionStab$model$m$getPars()
  if(length(model_params) > 0){
    modelpara_matrix = do.call(rbind, model_params)
    colnames(modelpara_matrix) = c("Inflection_point", "Slope")
    output$model_parameters = modelpara_matrix
  }
  # Add recommendation if available
  if(!is.na(recommended_num.trees)){
    # If the recommended number of trees is for some reason lower than 500 (default), set it to be 500
    if(recommended_num.trees < 500) recommended_num.trees = 500
    if(verbose) message("\n Recommended number of trees: ", recommended_num.trees)
    output$recommendation = recommended_num.trees
    output$recommendation_for = recommendation
    # Calculate expected stability for recommended number of trees
    stab_values = c()
    if(!is.null(predictionStab)) stab_values["Prediction_stability"] = predictionStab$estimates[predictionStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    if(!is.null(selectionStab)) stab_values["Selection_stability"] = selectionStab$estimates[selectionStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    stab_values["Computation_time"] = estimate_runtime(recommended_num.trees, runtime_model$coefficients[1], runtime_model$coefficients[2])
    output$expected_RF_stability <- matrix(stab_values, ncol = 1, dimnames = list(names(stab_values), "Value"))
  }
  class(output) = "opt_prediction_object"
  return(output)
}
