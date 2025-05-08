#' @export
#' @importFrom graphics points

plot.opt_importance_object = function(x, ...){

  max.plot = round(max(x$result.table$num.trees_values)*1.1)
  plot_seq = seq(0, max.plot, 1)

  plot(x$result.table$VI_stability ~ x$result.table$num.trees_values,
       main='Relationship between\n variable importance stability and number of trees',
       ylab="Variable importance stability", xlab="number of trees", ...)
  points(TwoPLmodel(plot_seq, x$model.parameters[1,1], x$model.parameters[1,2]) ~ plot_seq,
         type="l", col="navyblue", lwd=3)
}
