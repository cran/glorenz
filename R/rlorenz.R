## Relative Lorenz Curve function


#' Compute relevant probabilities and estimates for selecting performance criteria
#'
#' @param p percentile based on data from group with lower mean value
#' @param data dataset of group with higher mean value.
#' @param group variable of interest. Entered in quotes.Must be present in data and edata.
#' @param newwts sampling weights. "newwts" by default. Must be present in data and edata.
#' @param edata dataset of group with lower mean value.Must have defined newwts column. Sum of newwts for edata must be equal to sum of newwts for data.
#'
#' @return Relative Lorenz function value for p
#' @export
#'
#' @examples
#'
#'df_samp <- data.frame(x1 = rnorm(500, mean = 5, sd = 2),newwts = rep(1, 500))
#'df_samp2 <- data.frame(x1 = rnorm(500, mean = 4.5, sd = 2),newwts = rep(1, 500))
#'p_vals <- seq(0, 1, length.out = 100)
#'lc_vals <- rlorenz(p_vals, data = df_samp, group = "x1", edata = df_samp2)
#'
#' #Creates relative Lorenz curve values for two sets of simulated data


rlorenz <- function(p, data , group , edata, newwts="newwts" ) {

  if (sum(data[[newwts]]) != sum(edata[[newwts]])) {
    warning("Warning: sum(data$newwts) is different from sum(edata$newwts); Datasets not comparable")
  }

  mu <- sum(data[[group]] * data[[newwts]]) / sum(data[[newwts]])

  Epsres <- sapply(p, function(prob) Eps(prob, data = edata, group))
  out <- sapply(Epsres, function(ep) {
    sum(data[[group]][data[[group]] <= ep] * data[[newwts]][data[[group]] <= ep]) / sum(data[[newwts]])
  })

  return(out / mu)
}
