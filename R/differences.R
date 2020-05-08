#' A function that takes two means, sd's, n's and gives lower and upper bounds for estimated difference and cohen d
#'
#' This function will give you a confidence interval around a difference and its associated cohen's d
#'
#' formula for d = (mu1 - mu2) / sigma
#' where sigma = sqrt ( a / b ) = pooled standard deviation
#' where a = (n-1) x s1 x s1 + (n-2) x s2 x s2
#' where b = n1 + n2 - 2
#'
#' also: t = ( mean(x1) - mean(x2) ) / (sigma  * sqrt(1/n1 + 1/n2))
#' so t = d * 1 / sqrt(1/n1 + 1/n2)
#' so d = t * sqrt(1/n1 + 1/n2)
#'
#' @param mean1 mean 1
#' @param mean2 mean 2
#' @param sd1 sd 1
#' @param sd2 sd 2
#' @param n1 sample size 1
#' @param n2 sample size 2
#' @keywords CI
#' @export
#' @examples
#' differences(mean1,mean2,sd1,sd2,n1,n2)

differences <- function(mean1,mean2,sd1,sd2,n1,n2,ci = 0.95) {

  difference <- mean1 - mean2
  df <- n1+n2-2
  sd.pooled <- sqrt(( sd1*sd1 * (n1-1) + sd2*sd2 * (n2-1))/(df))

  lwr <- difference + qt((1-ci)/2, df = df) * sd.pooled * sqrt(1/n1 + 1/n2)
  upr <- difference + qt((1+ci)/2, df = df) * sd.pooled * sqrt(1/n1 + 1/n2)

  d.est <- (difference) / sd.pooled
  d.lwr <- cohend(d.est,n1,n2,"lwr")
  d.upr <- cohend(d.est,n1,n2,"upr")

  pvalue <- round(2 * pt( -abs( d.est / sqrt(1/n1+1/n2)  ), df=df ),4)

  # return:
  as_tibble(cbind(difference,lwr,upr,df,pvalue,d.est,d.lwr,d.upr)) %>% magrittr::set_names(c("difference","lwr","upr","df","p","d.est","d.lwr","d.upr"))
}
