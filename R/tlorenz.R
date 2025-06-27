## Main Function


#' Compute relevant probabilities and estimates for selecting performance criteria
#' @importFrom stats uniroot
#' @importFrom LorenzRegression Lorenz.curve
#' @param q 1-percentile of data on variable of interest in group with higher mean value
#' @param d1 dataset of group with higher mean value.
#' @param d2 dataset of group with lower mean value.
#' @param group variable of interest. Entered in quotes.Must be present in d1 and d2
#' @param newwts sampling weights. "newwts" by default. Must be present in data and edata.
#'
#'
#'
#' @return Transformed Lorenz function value for q
#' @export
#'
#' @examples
#'df_samp <- data.frame(x1 = rnorm(500, mean = 5, sd = 2),newwts = rep(1, 500))
#'df_samp2 <- data.frame(x1 = rnorm(500, mean = 4.5, sd = 2),newwts = rep(1, 500))
#'p_vals <- seq(0, 1, length.out = 100)
#'lc_vals <- tlorenz(p_vals, d1 = df_samp, group = "x1", d2 = df_samp2)
#'
#' #Creates transformed Lorenz curve values for two sets of simulated data

tlorenz <- function(q, d1 , group , d2 , newwts="newwts") {

  if (sum(d1[[newwts]]) != sum(d2[[newwts]])) {
    warning("Warning: sum(d1$newwts) is different from sum(d2$newwts); Datasets not comparable")
  }

  inverse <- function(f, lower = -100, upper = 100) {
    function(y) sapply(y, function(yy) {
      tryCatch(
        uniroot((function(x) f(x) - yy), lower = lower, upper = upper)$root,
        error = function(e) NA
      )
    })
  }

  x1 <- Lorenz.curve(
    y = d1[[group]],
    na.rm = TRUE,
    ties.method = c("mean", "random"),
    seed = NULL,
    weights = d1[[newwts]]
  )

  x2 <- Lorenz.curve(
    y = d2[[group]],
    na.rm = TRUE,
    ties.method = c("mean", "random"),
    seed = NULL,
    weights = d2[[newwts]]
  )

  x2I <- inverse(x2, lower = -100, upper = 100)

  mu1 <- sum(d1[[group]] * d1[[newwts]]) / sum(d1[[newwts]])
  mu2 <- sum(d2[[group]] * d2[[newwts]]) / sum(d2[[newwts]])


  out <- sapply(q, function(qq) {
    tryCatch(
      x2I(mu1 / mu2 * (1 - x1(1 - qq))),
      error = function(e) NA
    )
  })

  out<-as.numeric(out)
  out[out<0]<-0
  return(as.numeric(out))
}
