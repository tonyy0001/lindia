

#' Plot all given plots in a square matrix form.
#'
#' @param plots a list of plots
#' @param ncol numeric; the number of column that the arranged grid need to be.
#' defaults to fitting all plots in square matrix
#' @param max.per.page numeric; maximum number of plots allowed in one page.
#' @return plots in a given list arrangeed using gridExtra
#' @examples 
#' library(MASS)
#' data(Cars93)
#' # a regression with categorical variable
#' cars_lm <- lm(Price ~ Passengers + Length + RPM + Origin, Cars93)
#' p <- gg_diagnose(cars_lm, plot.all = FALSE)
#' names(p)
#' selected.plots <- plots[-c(2, 5)]
#' plot_all(selected.plots)
#' @export
gg_scalelocation <- function(fitted.lm, method = 'loess', scale.factor = 1, se = FALSE) {

   handle_exception(fitted.lm, "gg_scalelocation")

   # obtain stardardized residual and fitted values from fitted.lm
   fitted_values = fitted(fitted.lm)
   std_res = rstandard(fitted.lm)

   df = data.frame(std_res, fitted_values)
   names(df) = c("sqrt(std_res)", "fitted_values")
   return (ggplot(data = df, aes(y = std_res, x = fitted_values)) +
              geom_point(size = scale.factor) +
              geom_smooth(method = method, se = se, size = scale.factor, color = "indianred3") +
              labs(x="Sqrt(Standardized Residuals)", y = "Fitted Values"), title = "Scale-Location Plot")
}
