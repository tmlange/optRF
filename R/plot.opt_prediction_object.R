#' @export

plot.opt_prediction_object = function(optRF_object, ...){
  estimate_stability = function(at, p1, p2){
    1 / (1+(p1/at)^p2)
  }

  max.plot = round(max(optRF_object$result.table$num.trees_values)*1.1)
  plot_seq = seq(0, max.plot, 1)

  plot(optRF_object$result.table$pred.stability ~ optRF_object$result.table$num.trees_values,
       main='Relationship between\n prediction stability and number of trees',
       ylab="Prediction stability", xlab="number of trees", ...)
  points(estimate_stability(plot_seq, optRF_object$model.parameters[1,1], optRF_object$model.parameters[1,2]) ~ plot_seq,
         type="l", col="navyblue", lwd=3)
}
