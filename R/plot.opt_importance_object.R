#' @export

plot.opt_importance_object = function(x, ...){

  max_plot = round(max(x$result_table$num.trees_values)*1.1)
  plot_seq = seq(0, max_plot, 1)

  plot(x$result_table$VI_stability ~ x$result_table$num.trees_values,
       main='Relationship between\n variable importance stability and number of trees',
       ylab="Variable importance stability", xlab="number of trees", ...)
  graphics::lines(TwoPLmodel(plot_seq, x$model_parameters[1,1], x$model_parameters[1,2]) ~ plot_seq,
         col="navyblue", lwd=3)
}
