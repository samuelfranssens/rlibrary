#' A function for calculating (complicated) contrasts
#'
#' This function will calculate a contrast and summarize it in publication-ready output.
#' This is for complicated contrasts. For simple contrasts (simply comparing two cell means), use writesimplecontrast
#' Set rmd to true to get Rmarkdown output.
#' Set list to true to return a list object.
#' RMD trumps list.
#' @param n1 number of observations in cell 1
#' @param n2 number of observations in cell 2
#' @param nrofterms how many terms are involved?
#' @param Y dependent variable
#' @param aovmodel An anova model
#' @param K contrast
#' @keywords contrast
#' @export
#' @examples
#' writecontrast()

writecontrast <- function(n1,n2,nrofterms,aovmodel,K,rmd=FALSE,list=FALSE) {
  t <- glht(aovmodel,linfct=K)
  d <- summary(t)$test$tstat[1] * sqrt(1/n1 +1/n2) 
  df <- n1+n2-nrofterms
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
  contrastx <- paste(": est = ",round(summary(t)$test$coefficients[1],2),", 95% CI [",lwr,"; ",upr,"], t(",df,") = ",round(abs(summary(t)$test$tstat),2),pvalue,", d = ",round(abs(d),2), sep="")
  if (rmd & list) {list=F}
  if(rmd) { contrastx <- paste(": *est* = ",round(summary(t)$test$coefficients[1],2),", 95% CI [",lwr,"; ",upr,"], *t*(",df,") = ",round(abs(summary(t)$test$tstat),2),pvalue,", *d* = ",round(abs(d),2), sep="") }
  if(list) { 
    contrastx <- list(round(summary(t)$test$coefficients[1],2),lwr,upr,df,round(abs(summary(t)$test$tstat),2),p,round(abs(d),2))
    names(contrastx) <- c("est","lwr","upr","df","t","p","d") 
    }
  return(contrastx)
}