#' A function that will give the confidence interval around Cohen's d.
#'
#' This function will give you a confidence interval around Cohen's d.
#' It requires an estimate of d, n1, n2, and whether or not you want the upper/lower bound
#' @param d estimate of Cohen's d
#' @param n1 sample size of cell 1
#' @param n2 sample size of cell 2
#' @param lu lower or upper bound
#' @param ci alpha level
#' @keywords effect size
#' @export
#' @examples
#' cohend(d.est, n1, n2, "lwr")

cohend <- function(d, n1, n2,lu,ci = 0.95) {
  library(compute.es)
  t <- d * sqrt((n1 * n2)/(n1 + n2))
  capture.output(
    fit <- compute.es::tes(t = t, n.1 = n1, n.2 = n2, level = 100 * ci),
    file = "NUL"
  )
  if(lu=="lwr"){ return(fit$l.d) }
  if(lu=="upr"){ return(fit$u.d) }
}
