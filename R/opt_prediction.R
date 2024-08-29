#' @title Optimise random forest for prediction
#'
#' @description Optimising random forest predictions by calculating the prediction stability with certain numbers of trees
#'
#' @param y A vector containing the response variable in the training data set.
#' @param X A data frame containing the explanatory variables in the training data set. The number of rows must be equal to the number of elements in y.
#' @param X_Test A data frame containing the explanatory variables of the test data set. If not entered, a test data set will be randomly generated.
#' @param number.repetitions Number of repetitions of random forest to estimate the prediction stability.
#' @param alpha The number of best individuals to be selected in the test data set based on their predicted response values. If < 1, alpha will be considered to be the relative amount of individuals in the test data set.
#' @param num.trees_values A vector containing the numbers of trees to be analysed. If not specified, 250, 500, 750, 1000, and 2000 trees will be analysed.
#' @param visualisation Can be set to "prediction" to draw a plot of the prediction stability or "selection" to draw a plot of the selection stability for the numbers of trees to be analysed.
#' @param select_for What should be selected? In random forest classification, this must be set to the value of the desired class. In random forest regression, this can be set as "high" (default) to select the individuals with the highest predicted value, "low" to select the individuals with the lowest predicted value, or "zero" to select the individuals which predicted value is closest to zero.
#' @param recommendation If set to "prediction" (default) or "selection", a recommendation will be given based on optimised prediction or selection stability. If set to be "none", the function will analyse the stability of random forest with the inserted numbers of trees without giving a recommendation.
#' @param rec.thresh If the number of trees leads to an increase of stability smaller or equal to the value specified, this number of trees will be recommended. Default is 1e-6.
#' @param round.recommendation Setting to what number the recommended number of trees should be rounded to. Options: "none", "ten", "hundred", "thousand" (default).
#' @param ... Any other argument from the ranger function.
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
#' @importFrom ranger ranger
#' @importFrom minpack.lm nlsLM




opt_prediction = function(y=NULL, X=NULL, X_Test=NULL,
                          number.repetitions=10, alpha = 0.15,
                          num.trees_values=c(250, 500, 750, 1000, 2000), visualisation = c("none","prediction","selection"), select_for = c("high", "low", "zero"),
                          recommendation = c("prediction","selection", "none"),
                          rec.thresh = 1e-6, round.recommendation = c("thousand","hundred","ten","none"), ...){

  rec.num.trees = NA

  # Check if data was given by the user
  if(is.null(y) | is.null(X)){
    stop("Please insert data via y and X \n")
  }

  # Test if data format is correct
  if(!all.equal(nrow(X), length(y))){
    stop("Length of y does not equal number of rows of X \n")
  }


  # If no test data set was entered, a random test data set will be generated
  if(is.null(X_Test)){
    message("No test data were entered. A test data set will be simulated.")
    Test_size = round(ncol(X), -2)/10

    create.var = function(x){
      if(dim(table(x)) == 1){
        rep(x[1], Test_size)
      }
      else{
        sample(unique(x)[order(unique(x))], size = Test_size, replace=TRUE, prob=as.numeric(table(x)/length(x)))
      }
    }

    # For very small data sets, simulate test data with 100 observations
    if(Test_size<100){
      Test_size = 100
    }
    X_Test = apply(X, 2, create.var)
  }
  else{
    if(ncol(X) != ncol(X_Test) | !all(colnames(X) %in% colnames(X_Test))){
      stop("X_Test needs to contain the same variables as X.")
    }
  }

  # If y is neither numeric nor a factor, return an error message
  if(!is.numeric(y) & !is.factor(y)){
    stop("The response variable is neither numeric nor a factor.")
  }

  variable.number <- round(ncol(X), -2)


  if(!is.numeric(num.trees_values) | any(num.trees_values < 1)){
    stop("The num.tree_values need to be a vector of positive numbers.\n")
  }

  if(variable.number < 100000){
    test_seq = seq(10, 1000000, 10)
  }
  if(variable.number > 100000){
    test_seq = seq(10, round((variable.number*100), -1), 10)
  }

  # Defining the number of individuals to be selected from the test data set
  if(alpha < 1){
    selection.size = round(nrow(X_Test)*alpha)
  }
  else{
    selection.size = round(alpha)
  }
  row.names(X_Test) = paste0("ID_", c(1:nrow(X_Test)))

  # Defining to what number the recommendation of number of trees should be rounded to
  if(identical(round.recommendation, c("thousand","hundred","ten","none"))){
    round.recommendation = "thousand"
  }
  if(length(round.recommendation) != 1 || !(round.recommendation %in% c("thousand","hundred","ten","none"))){
    stop("Invalid input for round.recommendation The round.recommendation must be either \"thousand\", \"hundred\", \"ten\", or \"none\".")
  }
  if(round.recommendation == "none"){
    round.rec = 0
  }
  if(round.recommendation == "ten"){
    round.rec = -1
  }
  if(round.recommendation == "hundred"){
    round.rec = -2
  }
  if(round.recommendation == "thousand"){
    round.rec = -3
  }


  TwoPLmodel = function(test_seq, p1, p2){
    1 / (1+(p1/test_seq)^p2)
  }
  estimate_runtime = function(test_seq, p1, p2){
    p1 + test_seq*p2
  }

  # Check value of recommendation
  if(identical(recommendation, c("prediction","selection","none"))){
    recommendation = "prediction"
  }
  if(length(recommendation) != 1 || !(recommendation %in% c("prediction","selection","none"))){
    stop("Invalid input for recommendation. The recommendation must be either \"prediction\", \"selection\", or \"none\".")
  }

  # Check value of visualisation
  if(identical(visualisation, c("none","prediction","selection"))){
    visualisation = "none"
  }
  if(length(visualisation) != 1 || !(visualisation %in% c("none","prediction","selection"))){
    stop("Invalid input for visualisation The visualisation must be either \"prediction\", \"selection\", or \"none\".")
  }


  # Run the analysis

  # If y is numeric, perform regression analysis
  if(is.numeric(y)){

    # First, check what should be selected for
    # If there was a wrong word entered by the user, give an error message
    # if(!is.null(select_for)){
    #  if(!(select_for %in% c("high","zero","low"))){
    #    stop("Invalid input for select_for was set. Please enter either \"high\", \"low\", or \"zero\" when using opt_prediction for random forest regression.")
    #  }
    #}
    #else{
    #  # If there is nothing defined by the user, select for high phenotypes (like yield)
    #  select_for = "high"
    #  message("No value for select_for was set. It will be selected for highest values (default).")
    #}

    # Check value of select_for
    if(identical(select_for, c("high","low","zero"))){
      select_for = "high"
      message("No value for select_for was set. It will be selected for highest values (default).")
    }
    if(length(select_for) != 1 || !(select_for %in% c("high","low","zero"))){
      stop("Invalid input for select_for was set. Please enter either \"high\", \"low\", or \"zero\" when using opt_prediction for random forest regression.")
    }


    red.pred.stability = vector()
    summary.result = data.frame()
    for(i in 1:length(num.trees_values)){

      D_preds = data.frame(ID= row.names(X_Test))
      D_selection = data.frame(ID= row.names(X_Test))
      start.time = Sys.time()
      for(rep in 1:number.repetitions){

        cat("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number.repetitions)*100, 0), "%            \r", sep="")

        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees_values[i],
                           verbose = FALSE,
                           write.forest = TRUE,
                           ...)
        predictions <- predict(myForest, data=X_Test)$predictions


        D_pred_test = data.frame(ID = row.names(X_Test), pred = predictions)
        predictions = data.frame(predictions)
        names(predictions) = paste0("Predictions_run", rep)
        D_preds = cbind(D_preds, predictions)


        # Perform the selection
        if(select_for == "high"){
          D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=T),]
          selection = D_pred_test$ID[1:selection.size]
        }
        else{
          if(select_for == "low"){
            D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=F),]
            selection = D_pred_test$ID[1:selection.size]
          }
          else{
            # If it is neither "low" nor "high", it must be "zero"
            # To analyse which predictions are closest to zero, calculate absolute values
            D_pred_test$pred = abs(D_pred_test$pred)
            D_pred_test = D_pred_test[order(D_pred_test$pred, decreasing=F),]
            selection = D_pred_test$ID[1:selection.size]
          }
        }

        tmp_D_selection = data.frame(ID = row.names(X_Test))
        tmp_D_selection$selection = "rejected"
        tmp_D_selection[tmp_D_selection$ID %in% selection,]$selection = "selected"
        names(tmp_D_selection) = c("ID", paste0("Selections_in_run_", rep))
        D_selection = merge(D_selection, tmp_D_selection, by="ID")
      }
      end.time = Sys.time()

      # Removing the column with the IDs so that D_selection is a data frame that contains only the levels "selected" and "not_selected"
      D_selection = D_selection[,-1]

      # Removing the column with the IDs so that D_preds is a data frame that contains only the predictions
      D_preds = D_preds[,-1]

      # Summarising the results
      tmp_res = data.frame(num.trees_values = num.trees_values[i],
                           pred.stability = icc(D_preds)$value,
                           selection.stability = kappam.fleiss(D_selection)$value,
                           run.time = (as.numeric(difftime(end.time, start.time, units = "secs")))/number.repetitions)

      summary.result = rbind(summary.result, tmp_res)

      if(visualisation == "prediction"){
        plot(summary.result$pred.stability ~ summary.result$num.trees_values, main='Relationship between\n prediction stability and number of trees',
             ylab="Prediction stability", xlab="number of trees",
             col="black", cex=1.5, pch=20,
             ylim=c((min(summary.result$pred.stability)-0.001), (max(summary.result$pred.stability)+0.001)),
             xlim=c(min(summary.result$num.trees_values),max(summary.result$num.trees_values)),
             cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
      }

      if(visualisation == "selection"){
        plot(summary.result$selection.stability ~ summary.result$num.trees_values, main='Relationship between\n selection stability and number of trees',
             ylab="Selection stability", xlab="number of trees",
             col="black", cex=1.5, pch=20,
             ylim=c((min(summary.result$selection.stability)-0.001), (max(summary.result$selection.stability)+0.001)),
             xlim=c(min(summary.result$num.trees_values),max(summary.result$num.trees_values)),
             cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
      }


      # If there are more than four data points, model the relationship(s)
      if(nrow(summary.result) >= 4){

        # non linear modelling of the relationship between prediction stability and num.trees values
        tryCatch({

          start_val_p1 = summary.result$num.trees_values[round((nrow(summary.result)/2))]
          non.lin.mod.pv <- nlsLM(pred.stability ~ 1 / (1+(p1/num.trees_values)^p2), data=summary.result,
                                  start=c(p1=start_val_p1, p2=0.5),
                                  control = nls.lm.control(maxiter = 1024))

          if(visualisation == "prediction"){
            points(TwoPLmodel(test_seq, non.lin.mod.pv$m$getPars()[1], non.lin.mod.pv$m$getPars()[2]) ~ test_seq,
                   type="l", col="navyblue", lwd=3)
          }

          D_est.pv = data.frame(num.trees = test_seq,
                                estimated_prediction_stability = TwoPLmodel(test_seq, non.lin.mod.pv$m$getPars()[1], non.lin.mod.pv$m$getPars()[2]))
        }, error=function(e){})



        # non linear modelling of the relationship between selection stability and num.trees values
        tryCatch({

          start_val_p1 = summary.result$num.trees_values[round((nrow(summary.result)/2))]
          non.lin.mod.sv <- nlsLM(selection.stability ~ 1 / (1+(p1/num.trees_values)^p2), data=summary.result,
                                  start=c(p1=start_val_p1, p2=0.5),
                                  control = nls.lm.control(maxiter = 1024))

          if(visualisation == "selection"){
            points(TwoPLmodel(test_seq, non.lin.mod.sv$m$getPars()[1], non.lin.mod.sv$m$getPars()[2]) ~ test_seq,
                   type="l", col="navyblue", lwd=3)
          }

          D_est.sv = data.frame(num.trees = test_seq,
                                estimated_selection_stability = TwoPLmodel(test_seq, non.lin.mod.sv$m$getPars()[1], non.lin.mod.sv$m$getPars()[2]))
        }, error=function(e){})



        # linear modelling of the relationship between run time and num.trees values
        tryCatch({

          runtime_model = lm(summary.result$run.time ~ summary.result$num.trees_values)

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

        # Calculate the decrease of prediction stability per increase of trees
        v1 = D_est.pv$estimated_prediction_stability[-nrow(D_est.pv)]
        v2 = D_est.pv$estimated_prediction_stability[-1]
        D_est.pv = D_est.pv[-1,]
        D_est.pv$diff = v2 - v1
        D_est.pv$diff = D_est.pv$diff/10

        # Finally, make a recommendation
        new.rec.thresh = rec.thresh
        trust.rec = FALSE
        while(trust.rec == FALSE){

          rec.num.trees = round(D_est.pv[D_est.pv$diff<new.rec.thresh,]$num.trees[1], round.rec)

          # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
          if(rec.num.trees >= non.lin.mod.pv$m$getPars()[1]){
            if(exists('D_est.sv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
            if(!exists('D_est.sv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
          }

          # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
          if(rec.num.trees < non.lin.mod.pv$m$getPars()[1]){
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

        # Calculate the decrease of selection stability per increase of trees
        v1 = D_est.sv$estimated_selection_stability[-nrow(D_est.sv)]
        v2 = D_est.sv$estimated_selection_stability[-1]
        D_est.sv = D_est.sv[-1,]
        D_est.sv$diff = v2 - v1
        D_est.sv$diff = D_est.sv$diff/10

        # Finally, make a recommendation
        new.rec.thresh = rec.thresh
        trust.rec = FALSE
        while(trust.rec == FALSE){

          rec.num.trees = round(D_est.sv[D_est.sv$diff<new.rec.thresh,]$num.trees[1], round.rec)

          # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
          if(rec.num.trees >= non.lin.mod.sv$m$getPars()[1]){
            if(exists('D_est.pv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
            if(!exists('D_est.pv')){
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
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

      message("\n Recommended num.trees value: ", rec.num.trees)

      # Create output

      # if prediction and selection stability could be modeled
      if(exists('D_est.pv') & exists('D_est.sv')){
        modelpara.matrix = matrix(c(non.lin.mod.pv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_selection_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Selection_stability", "Run_time")
      }

      # if prediction stability could be modeled but selection stability could not be modeled
      if(exists('D_est.pv') & !exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")

        modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Run_time")
      }

      # If selection stability could be modeled but prediction stability could not
      if(!exists('D_est.pv') & exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")

        modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Selection_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_selection_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Selection_stability", "Run_time")
      }

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
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")

        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if prediction stability could be modeled but selection stability could not be modeled
      if(exists('D_est.pv') & !exists('D_est.sv')){
        modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability")

        warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if prediction stability could not be modeled but selection stability could be modeled
      if(!exists('D_est.pv') & exists('D_est.sv')){
        modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Selection_stability")

        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")
        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if neither prediction nor selection stability could be modeled
      if(!exists('D_est.pv') & !exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values as well as between selection stability and num.trees values\n")
        output = list(summary.result)
        names(output) = c("result.table")
        class(output) = "opt_prediction_object"
      }

      return(output)
    }


  } # Finished opt_prediction if y is numeric




  # If y is a factor, perform classification
  if(is.factor(y)){

    # Check the value of selected_for and check if this value occurs in the response of the training data set
    if(identical(select_for, c("high","low","zero"))){
      stop("No value for select_for was set. Please select what class should be selected for.")
    }
    if(length(select_for) != 1 || !(select_for %in% names(summary(y)))){
      stop("The value for select_for does not occur in the response variable of the training data set. Please select what class should be selected for.")
    }




    red.pred.stability = vector()
    summary.result = data.frame()
    for(i in 1:length(num.trees_values)){

      D_selection = data.frame(ID = row.names(X_Test))
      D_preds = data.frame(ID = row.names(X_Test))
      start.time = Sys.time()
      for(rep in 1:number.repetitions){

        cat("Analysing random forest with ", num.trees_values[i], " trees, progress: ", round((rep/number.repetitions)*100, 0), "%            \r", sep="")

        myForest <- ranger(x=X,
                           y=y,
                           num.trees = num.trees_values[i],
                           verbose = FALSE,
                           write.forest = TRUE)

        predictions <- predict(myForest, data=X_Test)$predictions

        # Creating the data frame to estimate the prediction stability (D_preds)
        tmp_D_preds = data.frame(predict(myForest, data=X_Test)$predictions)
        names(tmp_D_preds) = paste0("Predictions_run_", rep)
        D_preds = cbind(D_preds, tmp_D_preds)

        # Creating the data frame to estimate the selection stability (D_selection)
        D_pred_test = data.frame(ID = row.names(X_Test), pred = predictions)


        selection = D_pred_test[D_pred_test$pred == select_for,]$ID
        tmp_D_selection = data.frame(ID = row.names(X_Test))
        tmp_D_selection$selection = "rejected"
        tmp_D_selection[tmp_D_selection$ID %in% selection,]$selection = "selected"
        names(tmp_D_selection) = c("ID", paste0("Selections_in_run_", rep))
        D_selection = merge(D_selection, tmp_D_selection, by="ID")
      }
      end.time = Sys.time()

      # Removing the column with the IDs so that D_preds is a data frame that contains only the predicted classes
      D_preds = D_preds[,-1]

      # Removing the column with the IDs so that D_selection is a data frame that contains only the levels "selected" and "not_selected"
      D_selection = D_selection[,-1]

      # Summarising results
      tmp_res = data.frame(num.trees_values = num.trees_values[i],
                           pred.stability = kappam.fleiss(D_preds)$value,
                           selection.stability = kappam.fleiss(D_selection)$value,
                           run.time = (as.numeric(difftime(end.time, start.time, units = "secs")))/number.repetitions)

      summary.result = rbind(summary.result, tmp_res)

      if(visualisation == "prediction"){
        plot(summary.result$pred.stability ~ summary.result$num.trees_values, main='Relationship between\n prediction stability and number of trees',
             ylab="Prediction stability", xlab="number of trees",
             col="black", cex=1.5, pch=20,
             ylim=c((min(summary.result$pred.stability)-0.001), (max(summary.result$pred.stability)+0.001)),
             xlim=c(min(summary.result$num.trees_values),max(summary.result$num.trees_values)),
             cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
      }

      if(visualisation == "selection"){
        plot(summary.result$selection.stability ~ summary.result$num.trees_values, main='Relationship between\n selection stability and number of trees',
             ylab="Selection stability", xlab="number of trees",
             col="black", cex=1.5, pch=20,
             ylim=c((min(summary.result$selection.stability)-0.001), (max(summary.result$selection.stability)+0.001)),
             xlim=c(min(summary.result$num.trees_values),max(summary.result$num.trees_values)),
             cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
      }


      # If there are more than four data points, perform non linear modelling
      if(nrow(summary.result) >= 4){

        # non linear modelling of the relationship between prediction stability and num.trees values
        tryCatch({

          non.lin.mod.pv <- nlsLM(pred.stability ~ 1 / (1+(p1/num.trees_values)^p2), data=summary.result,
                                  start=c(p1=100, p2=0.5),
                                  control = nls.lm.control(maxiter = 1024))

          if(visualisation == "prediction"){
            points(TwoPLmodel(test_seq, non.lin.mod.pv$m$getPars()[1], non.lin.mod.pv$m$getPars()[2]) ~ test_seq,
                   type="l", col="navyblue", lwd=3)
          }

          D_est.pv = data.frame(num.trees = test_seq,
                                estimated_prediction_stability = TwoPLmodel(test_seq, non.lin.mod.pv$m$getPars()[1], non.lin.mod.pv$m$getPars()[2]))
        }, error=function(e){})



        # non linear modelling of the relationship between selection stability and num.trees values
        tryCatch({

          non.lin.mod.sv <- nlsLM(selection.stability ~ 1 / (1+(p1/num.trees_values)^p2), data=summary.result[1:i,],
                                  start=c(p1=100, p2=0.5),
                                  control = nls.lm.control(maxiter = 1024))

          if(visualisation == "selection"){
            points(TwoPLmodel(test_seq, non.lin.mod.sv$m$getPars()[1], non.lin.mod.sv$m$getPars()[2]) ~ test_seq,
                   type="l", col="navyblue", lwd=3)
          }

          D_est.sv = data.frame(num.trees = test_seq,
                                estimated_selection_stability = TwoPLmodel(test_seq, non.lin.mod.sv$m$getPars()[1], non.lin.mod.sv$m$getPars()[2]))
        }, error=function(e){})


        # linear modelling of the relationship between run time and num.trees values
        tryCatch({
          runtime_model = lm(summary.result$run.time ~ summary.result$num.trees_values)

          D_est.rt = data.frame(num.trees = test_seq,
                                estimated_run_time = estimate_runtime(test_seq, runtime_model$coefficients[1], runtime_model$coefficients[2]))
        }, error=function(e){})

      }



    # After all num.trees_values have been analysed, give a recommendation


    # If recommendation should be done with the prediction stability, optimise numbers of trees based on estimated prediction stability
    if(recommendation == "prediction"){

      if(!exists('D_est.pv')){
        warning("A recommendation cannot be given because the relationship between prediction stability and numbers of trees could not be modelled.")
      }


      # Try to perform a recommendation using a non-linear model
      tryCatch({

        # Calculate the decrease of prediction stability per increase of trees
        v1 = D_est.pv$estimated_prediction_stability[-nrow(D_est.pv)]
        v2 = D_est.pv$estimated_prediction_stability[-1]
        D_est.pv = D_est.pv[-1,]
        D_est.pv$diff = v2 - v1
        D_est.pv$diff = D_est.pv$diff/10

        # Finally, make a recommendation
        new.rec.thresh = rec.thresh
        trust.rec = FALSE
        while(trust.rec == FALSE){

          rec.num.trees = round(D_est.pv[D_est.pv$diff<new.rec.thresh,]$num.trees[1], round.rec)

          # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
          if(rec.num.trees >= non.lin.mod.pv$m$getPars()[1]){
            if(exists('D_est.sv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
            if(!exists('D_est.sv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
          }

          # If the recommendation is smaller than the inflection point, reduce the recommendation threshold by the factor 10
          if(rec.num.trees < non.lin.mod.pv$m$getPars()[1]){
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

        # Calculate the decrease of prediction stability per increase of trees
        v1 = D_est.sv$estimated_selection_stability[-nrow(D_est.sv)]
        v2 = D_est.sv$estimated_selection_stability[-1]
        D_est.sv = D_est.sv[-1,]
        D_est.sv$diff = v2 - v1
        D_est.sv$diff = D_est.sv$diff/10

        # Finally, make a recommendation
        new.rec.thresh = rec.thresh
        trust.rec = FALSE
        while(trust.rec == FALSE){

          rec.num.trees = round(D_est.sv[D_est.sv$diff<new.rec.thresh,]$num.trees[1], round.rec)

          # Only trust the recommended number of trees, if the recommendation is greater than the inflection point
          if(rec.num.trees >= non.lin.mod.sv$m$getPars()[1]){
            if(exists('D_est.pv')){
              estimated_final_prediction_stability = D_est.pv[D_est.pv$num.trees==rec.num.trees,]$estimated_prediction_stability
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
            if(!exists('D_est.pv')){
              estimated_final_selection_stability = D_est.sv[D_est.sv$num.trees==rec.num.trees,]$estimated_selection_stability
              estimated_final_run_time = D_est.rt[D_est.rt$num.trees==rec.num.trees,]$estimated_run_time
              trust.rec = TRUE
            }
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

      if(rec.num.trees < 500){
        rec.num.trees = 500
      }

      message("\n Recommended num.trees value: ", rec.num.trees)

      # Create output

      # if prediction and selection stability could be modeled
      if(exists('D_est.pv') & exists('D_est.sv')){
        modelpara.matrix = matrix(c(non.lin.mod.pv$m$getPars(), non.lin.mod.sv$m$getPars()), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_selection_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Selection_stability", "Run_time")

      }

      # if prediction stability could be modeled but selection stability could not be modeled
      if(exists('D_est.pv') & !exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
        modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_prediction_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Prediction_stability", "Run_time")

      }

      # If selection stability could be modeled but prediction stability could not
      if(!exists('D_est.pv') & exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")

        modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Selection_stability")

        RFstab.matrix = matrix(c(rec.num.trees, estimated_final_selection_stability, estimated_final_run_time))
        colnames(RFstab.matrix) = c("Value")
        rownames(RFstab.matrix) = c("num.trees", "Selection_stability", "Run_time")

      }

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
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability", "Selection_stability")

        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if prediction stability could be modeled but selection stability could not be modeled
      if(exists('D_est.pv') & !exists('D_est.sv')){
        modelpara.matrix = matrix(non.lin.mod.pv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Prediction_stability")

        warning("Could not produce a nonlinear model to describe the relationship between selection stability and num.trees values\n")
        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if prediction stability could not be modeled but selection stability could be modeled
      if(!exists('D_est.pv') & exists('D_est.sv')){
        modelpara.matrix = matrix(non.lin.mod.sv$m$getPars(), ncol=2, byrow=T)
        colnames(modelpara.matrix) = c("Inflection_point", "Slope")
        rownames(modelpara.matrix) = c("Selection_stability")

        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values\n")
        output = list(summary.result, modelpara.matrix)
        names(output) = c("result.table", "model.parameters")
        class(output) = "opt_prediction_object"
      }

      # if neither prediction nor selection stability could be modeled
      if(!exists('D_est.pv') & !exists('D_est.sv')){
        warning("Could not produce a nonlinear model to describe the relationship between prediction stability and num.trees values as well as between selection stability and num.trees values\n")
        output = list(summary.result)
        names(output) = c("result.table")
        class(output) = "opt_prediction_object"
      }

      return(output)
    }
  } # Finished opt_prediction if y is a factor
}
}
