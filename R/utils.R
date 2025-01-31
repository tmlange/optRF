#' @param number.repetitions Number of repetitions of random forest to estimate the stability.
#' @param num.trees_values A vector containing the numbers of trees to be analysed. If not specified, 250, 500, 750, 1000, and 2000 trees will be analysed.
#' @param rec.thresh If the number of trees leads to an increase of stability smaller or equal to the value specified, this number of trees will be recommended. Default is 1e-6.
#' @param verbose Show computation status
#' @param ... Any other argument from the ranger function.
#' @name opt_shared_parameters
NULL

#' @param optRF_object An optRF_object, either the result from the \link{opt_importance} or the \link{opt_prediction} function.
#' @name estimate_plot_shared_parameters
NULL

#' @param round.recommendation Setting to what number the recommended number of trees should be rounded to. Options: "none", "ten", "hundred", "thousand" (default).
round_rec_helper = function(round.recommendation = c("thousand","hundred","ten","none")){

  round.recommendation = match.arg(round.recommendation)

  switch(
    round.recommendation,
    none = 0,
    ten = -1,
    hundred = -2,
    thousand = -3
  )
}

TwoPLmodel = function(vec, p1, p2){
  1 / (1+(p1/vec)^p2)
}

TwoPLmodel.inv = function(vec, p1, p2){
  p1 / (((1/vec)-1)^(1/p2))
}

estimate_runtime = function(vec, p1, p2){
  p1 + vec*p2
}

#' Creates a plot of stability dependent on the number of trees, which is used by the \link{opt_importance} and the \link{opt_prediction} functions.
#'
#' @param stability_values A numeric vector containing the values for the y axis
#' @param num.tree_values A numeric vector containing the values for the x axis
#' @param label A character string indicating which stability measure is depicted and should be used for title and axis label
#' @noRd
create_stability_plot = function(stability_values, num.tree_values, label){
  plot(stability_values ~ num.tree_values, main=paste0('Relationship between\n', label, ' and number of trees'),
       ylab=label, xlab="number of trees",
       col="black", cex=1.5, pch=20,
       ylim=c((min(stability_values)-0.001), (max(stability_values)+0.001)),
       xlim=c(min(num.tree_values),max(num.tree_values)),
       cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
}

#' Performs non linear modelling between stability values and the number of trees
#'
#' @param summary.result A data.frame containing the number of trees and the stability as columns
#' @param variable A character string indicating the name of the column containing the stability values
#' @param test_seq A numeric vector containing the values for the x axis that should visualized
#' @param visualisation A boolean value indicating whether the model should be visualized in the current plot
#'
#' @return The non linear model as the output of the nlsLM function
#' @noRd
non_linear_modelling = function(summary.result, variable, test_seq, visualisation){
  start_val_p1 = summary.result$num.trees_values[round((nrow(summary.result)/2))]
  non.lin.mod <- nlsLM(summary.result[,variable] ~ 1 / (1+(p1/num.trees_values)^p2), data=summary.result,
                          start=c(p1=start_val_p1, p2=0.5),
                          control = nls.lm.control(maxiter = 1024))

  if(visualisation){
    points(TwoPLmodel(test_seq, non.lin.mod$m$getPars()[1], non.lin.mod$m$getPars()[2]) ~ test_seq,
           type="l", col="navyblue", lwd=3)
  }
  return(non.lin.mod)
}
