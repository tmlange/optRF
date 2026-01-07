#' @title Optimise random forest variable importance estimations
#'
#' @description Optimising random forest for estimating the importance of variables by calculating the variable importance stability with certain numbers of trees
#'
#' @param y A vector containing the response variable.
#' @param X A data frame containing the explanatory variables. The number of rows must be equal to the number of elements in y.
#' @param alpha The amount of most important variables to be selected based on their estimated variable importance. If < 1, alpha will be considered the relative amount of variables in the data set.
#' @param importance Variable importance mode, one of "permutation" (default), "impurity" or "impurity_corrected". The "impurity" measure is the Gini index for classification and the variance of the responses for regression.
#' @param visualisation Can be set to "importance" to draw a plot of the variable importance stability or to "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param recommendation If set to "importance" (default) or "selection", a recommendation will be given based on optimised variable importance or selection stability.
#' @param rank_based Should the variable importance stability be defined by the similarity of the estimated variable importance values via the intraclass correlation coefficient (rank_based == FALSE, default) or by the rankings of the variables via Kendall's W (rank_based == TRUE)?
#' @inheritParams round_rec_helper
#' @inheritParams number_rep_helper
#' @inheritParams rec_thresh_helper
#' @inheritParams opt_shared_parameters
#'
#' @return An opt_importance_object containing the recommended number of trees, based on which measure the recommendation was given (importance or selection), a matrix summarising the estimated stability and computation time of a random forest with the recommended numbers of trees, a matrix containing the calculated stability and computation time for the analysed numbers of trees, and the parameters used to model the relationship between stability and numbers of trees.
#'
#' @examples
#' \dontrun{
#' data(SNPdata)
#' set.seed(123)
#' result_optimp = opt_importance(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' summary(result_optimp)
#' }
#'
#' @export
#' @importFrom irr icc kappam.fleiss kendall
#' @importFrom graphics points
#' @importFrom ranger ranger
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom ordinalForest ordfor



opt_importance = function(y, X, number_repetitions = 10, alpha = 0.05, 
                          num.trees_values = c(250, 500, 750, 1000, 2000),
                          importance = c("permutation", "impurity", "impurity_corrected"),
                          visualisation = c("none","importance","selection"), 
                          recommendation = c("importance","selection"),
                          rec_thresh = 1e-6, 
                          round_recommendation = c("thousand","hundred","ten","none"), 
                          rank_based = FALSE, response_type = NULL,
                          verbose = TRUE, ...){
  
  rec.num.trees = NA
  
  # Defining to what number the recommendation of number of trees should be rounded to
  round_rec = round_rec_helper(round_recommendation)
  
  # Check value of importance
  importance = match.arg(importance)
  
  # Check value of visualisation
  visualisation = match.arg(visualisation)
  
  # Check value of recommendation
  recommendation = match.arg(recommendation)
  
  # Check value of number_repetitions
  number_repetitions = number_rep_helper(number_repetitions)
  
  # Check value of rec_thresh
  rec_thresh = rec_thresh_helper(rec_thresh)
  
  # If y is neither numeric nor a factor, return an error message
  if(!is.numeric(y) & !is.factor(y)){
    stop("The response variable is neither numeric nor a factor")
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
  
  # Check if y and X have the same number of observations
  if(!all.equal(nrow(X), length(y))){
    stop("Length of y does not equal number of rows of X \n")
  }
  
  
  variable.number <- round(ncol(X), -2)
  if(variable.number < 100000){
    test_seq = seq(10, 1000000, 10)
  }
  if(variable.number > 100000){
    test_seq = seq(10, round((variable.number*100), -1), 10)
  }
  
  if(!is.numeric(num.trees_values) | any(num.trees_values < 1)){
    stop("The num.tree_values need to be a vector of positive numbers")
  }
  num.trees_values = ceiling(num.trees_values)
  
  # Check the value for rank_based
  if(rank_based != TRUE & rank_based != FALSE){
    stop("Invalid value for 'rank_based'; must be TRUE or FALSE")
  }
  
  
  # Run the analysis
  
  summary.result = data.frame()
  for(i in 1:length(num.trees_values)){
    
    D_VI = data.frame(variable.name = names(X))
    D_selection = data.frame(variable.name = names(X))
    time.taken = 0
    for(rep in 1:number_repetitions){
      
      # Perform random forest to estimate the importance per variable
      if(verbose){
        message(paste0("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number_repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
      }
      
      start.time = Sys.time()
      if(!is.null(response_type) && response_type == "ordinal"){
        y = factor(y, levels = sort(unique(y)), ordered = TRUE)
        ordfor_data <- data.frame(y = y, X)
        myForest <- ordfor(depvar="y", data=ordfor_data,
                        nsets = num.trees_values[i], ...)
        VI_result = data.frame(myForest$varimp)
      }
      else{
        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees_values[i],
                           importance = importance,
                           verbose = FALSE,
                           write.forest = TRUE,
                           ...)
        VI_result = data.frame(myForest$variable.importance)
      }
      time.taken = time.taken + as.numeric(difftime(Sys.time(), start.time, units = "secs"))
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
    
    if(rank_based){
      variable_importance_stability = kendall(D_VI)$value
      VI_definition = "Kendalls_W"
    } else {
      variable_importance_stability = icc(D_VI)$value
      VI_definition = "ICC"
    }
    
    tmp_res = data.frame(num.trees_values = num.trees_values[i],
                         VI_stability = variable_importance_stability,
                         selection_stability = kappam.fleiss(D_selection)$value,
                         computation_time = time.taken/number_repetitions)
    summary.result = rbind(summary.result, tmp_res)
    
    if(visualisation == "importance"){
      create_stability_plot(summary.result$VI_stability, summary.result$num.trees_values, "variable importance stability")
    }
    
    if(visualisation == "selection"){
      create_stability_plot(summary.result$selection_stability, summary.result$num.trees_values, "selection stability")
    }
    
    # If there are more than four data points, perform non linear modelling
    if(nrow(summary.result) >= 4){
      
      # non linear modelling of the relationship between variable importance stability and num.trees values
      importanceStab = fit_stability_model(summary.result, "VI_stability", test_seq, visualisation == "importance")
      
      # non linear modelling of the relationship between selection stability and num.trees values
      selectionStab = fit_stability_model(summary.result, "selection_stability", test_seq, visualisation == "selection")
      
      # linear modelling of the relationship between run time and num.trees values
      runtime_model = lm(computation_time ~ num.trees_values, data = summary.result)
    }
  }
  
  # After all num.trees_values have been analysed, give a recommendation
  recommended_num.trees = NA
  # If recommendation should be done with the variable importance stability, optimise numbers of trees based on estimated variable importance stability
  if(recommendation == "importance" && !is.null(importanceStab)){
    recommended_num.trees = find_recommendation(importanceStab$estimates, importanceStab$model, rec_thresh, round_rec)
  } else if(recommendation == "selection" && !is.null(selectionStab)){
    recommended_num.trees = find_recommendation(selectionStab$estimates, selectionStab$model, rec_thresh, round_rec)
  } else{
    warning("A recommendation cannot be given because the relationship between the requested stability and numbers of trees could not be modelled.")
  }
  
  # Create output
  # Base output
  output = list(variable_importance_stability_definition = VI_definition, result_table = summary.result)
  # Add model parameters if available
  model_params = list()
  if(!is.null(importanceStab)) model_params[["Variable_importance_stability"]] = importanceStab$model$m$getPars()
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
    if(!is.null(importanceStab)) stab_values["Variable_importance_stability"] = importanceStab$estimates[importanceStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    if(!is.null(selectionStab)) stab_values["Selection_stability"] = selectionStab$estimates[selectionStab$estimates$num.trees==recommended_num.trees,]$estimated_stability
    stab_values["Computation_time"] = predict(runtime_model, newdata = data.frame(num.trees_values = recommended_num.trees))
    output$expected_RF_stability <- matrix(stab_values, ncol = 1, dimnames = list(names(stab_values), "Value"))
  }
  class(output) = "opt_importance_object"
  return(output)
}
