## Inverse CDF helper function


#' Inverse cumulative distribution function for a subset of the dataset
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @param p cumulative distribution percentile
#' @param data dataset with defined group and newwts columns
#' @param group variable of interest
#'
#'
#' @return Cumulative distribution percentile for quantile y
#' @export
#'
#' @examples
#'
#' df_samp <- data.frame(qtyvar = rnorm(1000, mean = 5, sd = 2),newwts = rep(1, 1000))
#' Eps(0.25,df_samp)
#' # Finds the quantile for 25th percentile in simulated data.


Eps <- function(p, data, group = "qtyvar") {
  ys <- sort(unique(data[[group]]), decreasing = TRUE)

  if (p == 0) return(min(ys))

  low <- 1
  high <- length(ys)

  while (low < high) {
    mid <- floor((low + high) / 2)
    check <- Fd(ys[mid], data, group)

    if (check >= p) {
      low <- mid + 1
    } else {
      high <- mid
    }
  }

  return(ys[max(low - 1, 1)])
}
