## CDF helper function


#' Cumulative distribution function for a subset of the dataset
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param y cumulative distribution quantile
#' @param data dataset with defined group and newwts columns
#' @param group variable of interest
#' @param newwts sampling weights
#'
#'
#' @return Cumulative distribution percentile for quantile y
#' @export
#'
#' @examples
#'
#' df_samp <- data.frame(qtyvar = rnorm(1000, mean = 5, sd = 2),newwts = rep(1, 1000))
#' Fd(3.5,df_samp)
#' # Finds the percentile for 3.5 in simulated data.


Fd <- function(y, data, group="qtyvar", newwts="newwts") {
  selected_rows <- data %>% filter(.data[[group]] <= y)
  out <- sum(selected_rows[[newwts]]) / sum(data[[newwts]])
  return(out)
}

