#' @title Optimise random forest for estimation of variable importance
#'
#' @description Optimising random forest for estimating the importance of variables by calculating the variable importance stability with certain numbers of trees
#'
#' @param y A vector containing the response variable.
#' @param X A data frame containing the explanatory variables. The number of rows must be equal to the number of elements in y.
#' @param alpha The amount of most important variables to be selected based on their estimated variable importance. If < 1, alpha will be considered the relative amount of variables in the data set.
#' @param importance Variable importance mode, one of "permutation" (default), "impurity" or "impurity_corrected". The "impurity" measure is the Gini index for classification and the variance of the responses for regression.
#' @param visualisation Can be set to "importance" to draw a plot of the variable importance stability or to "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param recommendation If set to "importance" (default) or "selection", a recommendation will be given based on optimised variable importance or selection stability. If set to be "none", the function will analyse the stability of random forest with the inserted numbers of trees without giving a recommendation.
#' @inheritParams round_rec_helper
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
#' @importFrom irr icc kappam.fleiss
#' @importFrom graphics points
#' @importFrom ranger ranger
#' @importFrom minpack.lm nlsLM nls.lm.control




opt_importance = function(y, X, number.repetitions=10, alpha = 0.05, num.trees_values= c(250, 500, 750, 1000, 2000),
                          importance = c("permutation", "impurity", "impurity_corrected"),
                          visualisation= c("none","importance","selection"), recommendation = c("importance","selection","none"),
                          rec.thresh=1e-6, round.recommendation = c("thousand","hundred","ten","none"), verbose = TRUE, ...){

  rec.num.trees = NA

  # Defining to what number the recommendation of number of trees should be rounded to
  round.rec = round_rec_helper(round.recommendation)

  # Check value of importance
  importance = match.arg(importance)

  # Check value of visualisation
  visualisation = match.arg(visualisation)

  # Check value of recommendation
  recommendation = match.arg(recommendation)

  # If y is neither numeric nor a factor, return an error message
  if(!is.numeric(y) & !is.factor(y)){
    stop("The response variable is neither numeric nor a factor")
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

  # Run the analysis

  summary.result = data.frame()
  for(i in 1:length(num.trees_values)){

    D_VI = data.frame(variable.name = names(X))
    D_selection = data.frame(variable.name = names(X))
    time.taken = 0
    for(rep in 1:number.repetitions){

      # Perform random forest to estimate the importance per variable
      if(verbose){
        message(paste0("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number.repetitions)*100, 0), "%            \r", sep=""), appendLF = F)
      }

      start.time = Sys.time()
      myForest <- ranger(x=X,
                         y=y,
                         num.trees = num.trees_values[i],
                         importance = importance,
                         verbose = FALSE,
                         write.forest = TRUE,
                         ...)
      time.taken = time.taken + as.numeric(difftime(Sys.time(), start.time, units = "secs"))
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

    tmp_res = data.frame(num.trees_values = num.trees_values[i],
                         VI_stability = icc(D_VI)$value,
                         selection_stability = kappam.fleiss(D_selection)$value,
                         computation_time = time.taken/number.repetitions)
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
      tryCatch({
        non.lin.mod.VIv <- non_linear_modelling(summary.result, "VI_stability", test_seq, visualisation == "importance")
        D_est.VIv = data.frame(num.trees = test_seq,
                               estimated_VI_stability = TwoPLmodel(test_seq, non.lin.mod.VIv$m$getPars()[1], non.lin.mod.VIv$m$getPars()[2]))
      }, error=function(e){})

      # non linear modelling of the relationship between selection stability and num.trees values
      tryCatch({
        non.lin.mod.sv <- non_linear_modelling(summary.result, "selection_stability", test_seq, visualisation == "selection")
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

  # If recommendation should be done with the variable importance stability, optimise numbers of trees based on estimated variable importance stability
  if(recommendation == "importance"){

    if(!exists('D_est.VIv')){
      warning("A recommendation cannot be given because the relationship between variable importance stability and numbers of trees could not be modelled.")
    }

    # Try to perform a recommendation using a non-linear model
    tryCatch({

      # Calculate the increase of variable importance stability per increase of trees
      D_est.VIv$diff = c(NA,diff(D_est.VIv$estimated_VI_stability)/10)
      D_est.VIv = D_est.VIv[-1,]

      # Finally, make a recommendation
      new.rec.thresh = rec.thresh
      trust.rec = FALSE
      while(trust.rec == FALSE){

        rec.num.trees = round(D_est.VIv[D_est.VIv$diff<new.rec.thresh,]$num.trees[1], round.rec)

        # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
        if(rec.num.trees >= non.lin.mod.VIv$m$getPars()[1]){
          if(exists('D_est.sv')){
            estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
          }
          estimated_final_VI_stability = D_est.VIv[D_est.VIv$num.trees==rec.num.trees,]$estimated_VI_stability
          estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
          trust.rec = TRUE
        }

        # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
        if(rec.num.trees < non.lin.mod.VIv$m$getPars()[1]){
          new.rec.thresh = new.rec.thresh*0.1
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
      new.rec.thresh = rec.thresh
      trust.rec = FALSE
      while(trust.rec == FALSE){

        rec.num.trees = round(D_est.sv[D_est.sv$diff<new.rec.thresh,]$num.trees[1], round.rec)

        # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
        if(rec.num.trees >= non.lin.mod.sv$m$getPars()[1]){
          if(exists('D_est.VIv')){
            estimated_final_VI_stability = D_est.VIv[D_est.VIv$num.trees==rec.num.trees,]$estimated_VI_stability
          }
          estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
          estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
          trust.rec = TRUE
        }

        # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
        if(rec.num.trees < non.lin.mod.sv$m$getPars()[1]){
          new.rec.thresh = new.rec.thresh*0.1
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

    # If VI and selection stability could be modeled
    if(exists('D_est.VIv') & exists('D_est.sv')){
      modelpara.matrix = matrix(c(non.lin.mod.VIv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Variable_importance_stability", "Selection_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_VI_stability, estimated_final_selection_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Variable_importance_stability", "Selection_stability", "Computation_time")
    }

    # If variable importance stability could be modeled but selection stability could not
    if(exists('D_est.VIv') & !exists('D_est.sv')){
      warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
      modelpara.matrix = matrix(non.lin.mod.VIv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Variable_importance_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_VI_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Variable_importance_stability", "Computation_time")
    }

    # If selection stability could be modeled but variable importance stability could not
    if(!exists('D_est.VIv') & exists('D_est.sv')){
      warning("Could not produce a nonlinear model to describe the relationship between variable importance stability and num.trees values\n")
      modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
      rownames(modelpara.matrix) = c("Selection_stability")

      RFstab.matrix = matrix(c(rec.num.trees, estimated_final_selection_stability, estimated_final_run_time))
      rownames(RFstab.matrix) = c("num.trees", "Selection_stability", "Computation_time")
    }
    colnames(modelpara.matrix) = c("Inflection_point", "Slope")
    colnames(RFstab.matrix) = c("Value")

    output = list(rec.num.trees, recommendation, RFstab.matrix, summary.result, modelpara.matrix)
    names(output) = c("recommendation", "recommendation_for", "expected_RF_stability", "result.table", "model.parameters")
    class(output) = "opt_importance_object"
    return(output)
  }



  # If a recommendation should be made but could not be made
  if(recommendation == "importance" | recommendation == "selection"){
    warning("No recommendation could be made \n")
  }

  # If variable importance stability and selection stability could be modeled
  if(exists('D_est.VIv') & exists('D_est.sv')){
    modelpara.matrix = matrix(c(non.lin.mod.VIv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
    rownames(modelpara.matrix) = c("Variable_importance_stability", "Selection_stability")
  }

  # If variable importance stability could be modeled but selection stability could not
  if(exists('D_est.VIv') & !exists('D_est.sv')){
    modelpara.matrix = matrix(non.lin.mod.VIv$m$getPars(), ncol=2, byrow=T)
    rownames(modelpara.matrix) = c("Variable_importance_stability")
    warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
  }

  # If selection stability could be modeled but variable importance stability could not
  if(!exists('D_est.VIv') & exists('D_est.sv')){
    modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
    rownames(modelpara.matrix) = c("Selection_stability")
    warning("Could not produce a nonlinear model to describe the relationship between variable importance stability and num.trees values\n")
  }

  # If neither could be modeled
  if(!exists('D_est.VIv') & !exists('D_est.sv')){
    warning("Could not produce a nonlinear model to describe the relationship between variable importance stability and num.trees values as well as between selection stability and num.trees values\n")
    output = list(summary.result)
    names(output) = c("result.table")
  }
  else{
    colnames(modelpara.matrix) = c("Inflection_point", "Slope")
    output = list(summary.result, modelpara.matrix)
    names(output) = c("result.table", "model.parameters")
  }

  class(output) = "opt_importance_object"
  return(output)
}
