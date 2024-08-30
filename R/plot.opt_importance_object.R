#' @export
#' @importFrom graphics points

plot.opt_importance_object = function(x, ...){
  estimate_stability = function(at, p1, p2){
    1 / (1+(p1/at)^p2)
  }

  max.plot = round(max(x$result.table$num.trees_values)*1.1)
  plot_seq = seq(0, max.plot, 1)

  plot(x$result.table$VI.stability ~ x$result.table$num.trees_values,
       main='Relationship between\n variable importance stability and number of trees',
       ylab="Variable importance stability", xlab="number of trees", ...)
  points(estimate_stability(plot_seq, x$model.parameters[1,1], x$model.parameters[1,2]) ~ plot_seq,
         type="l", col="navyblue", lwd=3)
}
