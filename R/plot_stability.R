#' @title Plot random forest stability
#'
#' @description Plot the estimated stability of random forest against certain numbers of trees
#'
#' @param optRF_object An optRF_object, either the result from the \link{opt_importance} or the \link{opt_prediction} function.
#' @param measure A character string indicating which stability measure is to be plotted. One of "selection" (default, visualises selection stability), "prediction" (visualises prediction stability) or "importance" (visualises variable importance stability).
#' @param from Smallest num.trees value to be plotted.
#' @param to Greatest num.trees value to be plotted.
#' @param add.recommendation When set as TRUE, if a recommendation was stated within the opt_prediction or opt_importance function, the recommended num.trees value as well as the expected random forest stability will be highlighted in the graph
#' @param add If FALSE, a new plot will be created, if TRUE, the graph will be added to an existing plot.
#' @param ... Any other arguments from the plot function.
#'
#' @return A plot showing the estimated stability of random forest for the given num.trees values.
#'
#' @examples
#' data(SNPdata)
#' set.seed(123)
#' result_optpred = opt_prediction(y = SNPdata[,1], X=SNPdata[,-1]) # optimise random forest
#' plot_stability(result_optpred, measure = "prediction", add.recommendation = TRUE, add=FALSE)
#' plot_stability(result_optpred, measure = "selection",  add.recommendation = FALSE, add=TRUE)
#'
#' @export

plot_stability = function(optRF_object, measure = c("selection","importance","prediction"),
                          from = 0, to = 100000, add.recommendation = TRUE,
                          add = FALSE, ...){

  # Check if the correct object was inserted
  if(!(is(optRF_object, "opt_prediction_object")) & !(is(optRF_object, "opt_importance_object"))){
    stop("Invalid object was inserted. The inserted object must be the result from the opt_prediction or opt_importance function.")
  }

  # If the measure argument is invalid, give an error message
  if(identical(measure, c("selection", "importance", "prediction"))){
    measure = "selection"
  }
  if(length(measure) != 1 || !(measure %in% c("selection","importance","prediction"))){
    stop("Invalid input for measure. The measure must be either \"selection\", \"importance\", or \"prediction\".")
  }

  TwoPLmodel = function(at, p1, p2){
    1 / (1+(p1/at)^p2)
  }

  plot_seq = seq(from, to, 1)

  # If the object is an opt_prediction_object
  if(is(optRF_object, "opt_prediction_object")){

    # If the measure was set to be importance, this will not work
    if(measure == "importance"){
      stop("The variable importance stability cannot be plotted with an object created with the function opt_prediction.\nPlease set the measure argument to either \"prediction\" or \"selection\". \n")
    }

    # If the measure was set to be prediction, plot the prediction stability
    if(measure == "prediction"){

      if(add == FALSE){
        if(nrow(optRF_object$model.parameters) == 2 || # If a model for prediction and selection could be produced, plotting is not a problem
           (nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Prediction_stability")){ # If only one model could be produced, it must be the prediction model
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_prediction could not model the relationship between the number of trees and prediction or selection stability.\n")
        }
        else{
          stop("The function opt_prediction could not model the relationship between the number of trees and the prediction stability.\n")
        }
      }

      if(add == TRUE){
        if(nrow(optRF_object$model.parameters) == 2 || # If a model for prediction and selection could be produced, plotting is not a problem
           (nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Prediction_stability")){ # If only one model could be produced, it must be the prediction model
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_prediction could not model the relationship between the number of trees and prediction or selection stability.\n")
        }
        else{
          stop("The function opt_prediction could not model the relationship between the number of trees and the prediction stability.\n")
        }
      }
    }

    # If the measure was set to be selection, plot the selection stability
    if(measure == "selection"){
      if(add == FALSE){
        if(nrow(optRF_object$model.parameters) == 2){ # If a model for prediction and selection could be produced, plotting is not a problem
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if((nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Selection_stability")){ # If only one model could be produced, it must be the selection model
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_prediction could not model the relationship between number of trees and prediction or selection stability.\n")
        }
        else{
          stop("The function opt_prediction could not model the relationship between number of trees and the selection stability.\n")
        }
      }
      if(add == TRUE){

        if(nrow(optRF_object$model.parameters) == 2 ){ # If a model for prediction and selection could be produced, plotting is not a problem
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if((nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Selection_stability")){ # If only one model could be produced, it must be the selection model
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_prediction could not model the relationship between number of trees and prediction or selection stability.\n")
        }
        else{
          stop("The function opt_prediction could not model the relationship between number of trees and the selection stability.\n")
        }
      }
    }
  }


  # If the object is an opt_importance_object
  if(is(optRF_object, "opt_importance_object")){

    # If the measure was set to be prediction, this will not work
    if(measure == "prediction"){
      stop("The prediction stability cannot be plotted with an object created with the function opt_importance.\nPlease set the measure argument to either \"importance\" or \"selection\". \n")
    }

    # If the measure was set to be importance, plot the importance stability
    if(measure == "importance"){
      if(add == FALSE){
        if(nrow(optRF_object$model.parameters) == 2 || # If a model for VI and selection could be produced, plotting is not a problem
           (nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "VI_stability")){ # If only one model could be produced, it must be the VI model
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_importance could not model the relationship between number of trees and variable importance or selection stability.\n")
        }
        else{
          stop("The function opt_importance could not model the relationship between number of trees and the variable importance stability.\n")
        }
      }

      if(add == TRUE){
        if(nrow(optRF_object$model.parameters) == 2 || # If a model for VI and selection could be produced, plotting is not a problem
           (nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "VI_stability")){ # If only one model could be produced, it must be the VI model
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_importance could not model the relationship between number of trees and variable importance or selection stability.\n")
        }
        else{
          stop("The function opt_importance could not model the relationship between number of trees and the variable importance stability.\n")
        }
      }
    }

    # If the measure was set to be selection, plot the selection stability
    if(measure == "selection"){
      if(add == FALSE){
        if(nrow(optRF_object$model.parameters) == 2 ){ # If a model for VI and selection could be produced, plotting is not a problem
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Selection_stability"){ # If only one model could be produced, it must be the selection model
          plot(TwoPLmodel(plot_seq, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]) ~ plot_seq,
               type="l", ylab="Random forest stability", xlab="number of trees", main="Relationship between\n random forest stability and number of trees",
               cex.axis=1.2, cex.lab=1.2, cex.main=1.2, lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_importance could not model the relationship between number of trees and variable importance or selection stability.\n")
        }
        else{
          stop("The function opt_importance could not model the relationship between number of trees and the selection stability.\n")
        }
      }

      if(add == TRUE){
        if(nrow(optRF_object$model.parameters) == 2){ # If a model for VI and selection stability could be produced, plotting is not a problem
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[2,1], optRF_object$model.parameters[2,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 1 && row.names(optRF_object$model.parameters) == "Selection_stability"){ # If only one model could be produced, it must be the selection model
          points(TwoPLmodel(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
                 type="l", lwd=1.2, ...)
          if(add.recommendation == TRUE & !is.null(optRF_object$recommendation)){
            abline(v = optRF_object$recommendation, col=rgb(1, 0, 0, 0.5))
            abline(h = TwoPLmodel(optRF_object$recommendation, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]),
                   col=rgb(1, 0, 0, 0.5))
          }
          return()
        }
        if(nrow(optRF_object$model.parameters) == 0){ # If no model could be produced, give an error message
          stop("The function opt_importance could not model the relationship between number of trees and variable importance or selection stability.\n")
        }
        else{
          stop("The function opt_importance could not model the relationship between number of trees and the selection stability.\n")
        }
      }
    }
  }
}
