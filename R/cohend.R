#' A function that will give the confidence interval around Cohen's d.
#' It requires an estimate of d, n1, n2
#' @param d estimate of Cohen's d
#' @param n1 sample size of cell 1
#' @param n2 sample size of cell 2
#' @param alpha alpha level
#' @keywords effect size
#' @export
#' @examples
#' cohend(d.est, n1, n2)

cohend <- function(d, n1, n2, alpha = 0.95) {

  library(compute.es)

  t <- d * sqrt((n1 * n2)/(n1 + n2))

  capture.output(fit <- compute.es::tes(t = t, n.1 = n1, n.2 = n2, level = 100 * alpha), file = "NUL") # capture output, otherwise you'll get a lot of output you don't need

  return(c(fit$l.d, fit$u.d))
}
