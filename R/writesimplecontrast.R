#' A function for comparing two cell means
#'
#' This function will calculate a contrast and summarize it in publication-ready output.
#' This is for simple contrasts (simply comaring two cell means). For more complicated contrasts, use writecontrast.
#' You only need to pass on two numbers, x and y. These numbers refer to the experimental cells. They denote the subscripts of the Ns, the means, and the sds table.
#' Ns should contain the sample sizes, means should contain the means, sds should contain the standard deviations
#' Set rmd to true to get Rmarkdown output.
#' Set list to true to return a list object.
#' RMD trumps list.
#' @param x which experimental cell 1
#' @param y which experimental cell 2
#' @param aovmodel An anova model
#' @param K contrast
#' @keywords contrast
#' @export
#' @examples
#' writecontrast()

writesimplecontrast <- function(x, y, aovmodel,K,rmd=FALSE,list=FALSE) {
  t <- glht(aovmodel,linfct=K)
  d <- summary(t)$test$tstat[1] * sqrt(1/Ns[x] +1/Ns[y])
  p <- summary(t)$test$pvalues[1]
  est <- round(confint(t)$confint[1],2)
  lwr <- round(confint(t)$confint[2],2)
  upr <- round(confint(t)$confint[3],2)
  if (p < .001) {
    pvalue <- paste(", p < .001")
    if(rmd) { pvalue <- paste(", *p* < .001") }
  } else {
    if(p < .06) {
      pvalue <- paste(", p = ",round(p,3),sep="")
      if(rmd) { pvalue <- paste(", *p* = ",round(p,3),sep="") }
    } else {
      pvalue <- paste(", p = ",round(p,2),sep="")
      if(rmd) { pvalue <- paste(", *p* = ",round(p,2),sep="") }
    }
  }
  df <- Ns[x]+Ns[y]-2
  contrastx <- paste(": ",round(means[x],2)," (SD = ",round(sds[x],2),") vs. ",round(means[y],2)," (SD = ",round(sds[y],2),"), est = ",round(summary(t)$test$coefficients[1],2),", 95% CI [",lwr,"; ",upr,"], t(",df,") = ",round(abs(summary(t)$test$tstat),2),pvalue,", d = ",round(abs(d),2), sep="")
  if (rmd & list) {list=F}
  if(rmd) { contrastx <- paste(": ",round(means[x],2)," (*SD* = ",round(sds[x],2),") vs. ",round(means[y],2)," (*SD* = ",round(sds[y],2),"), *est* = ",round(summary(t)$test$coefficients[1],2),", 95% CI [",lwr,"; ",upr,"], *t*(",df,") = ",round(abs(summary(t)$test$tstat),2),pvalue,", *d* = ",round(abs(d),2), sep="") }
  if(list){
    contrastx <- list(round(means[x],2),round(sds[x],2),round(means[y],2),round(sds[y],2),round(summary(t)$test$coefficients[1],2),lwr,upr,df,round(abs(summary(t)$test$tstat),2),pvalue,round(abs(d),2),Ns[x],Ns[y])
    names(contrastx) <- c("m1","sd1","m2","sd2","est","lwr","upr","df","t",p,d,n1,n2)
    }
  return(contrastx)
}
