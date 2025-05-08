#' @export
#' @importFrom graphics points

plot.opt_prediction_object = function(x, ...){

  max.plot = round(max(x$result.table$num.trees_values)*1.1)
  plot_seq = seq(0, max.plot, 1)

  plot(x$result.table$pred_stability ~ x$result.table$num.trees_values,
       main='Relationship between\n prediction stability and number of trees',
       ylab="Prediction stability", xlab="number of trees", ...)
  points(TwoPLmodel(plot_seq, x$model.parameters[1,1], x$model.parameters[1,2]) ~ plot_seq,
         type="l", col="navyblue", lwd=3)
}
