#' Generate histogram of residuals in ggplot.
#'
#' @param fitted.lm a fitted linear model (i.e. lm, glm) that contains fitted regression
#' @param bins bin size for histogram
#' @return A ggplot object
#' @examples
#' library(MASS)
#' data(Cars93)
#' cars_lm <- lm(Price~ Passengers + Length + RPM, Cars93)
#' gg_reshist(cars_lm)
#' # specify number of bins
#' gg_reshist(cars_lm, bins = 20)
#' @export
gg_reshist <- function(fitted.lm, bins = NULL) {
  handle_exception(fitted.lm, "gg_reshist")
  
  #obtain residual and fitted values from fitted.lm
  res <- tibble(residuals = fitted.lm$residuals)
  
  if (is.null(bins)) {
    return(
      ggplot(res, aes(residuals)) + 
             geom_histogram(color = "white") +
             labs(x = "Residuals", title = "Histogram of Residuals")
    )
  }
  else {
    return(
      ggplot(res, aes(residuals)) + 
        geom_histogram(color = "white", bins = bins) +
        labs(x = "Residuals", y = "Count", title = "Histogram of Residuals"))
  }
}
