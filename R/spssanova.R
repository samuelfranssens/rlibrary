#' A function that generates the same ANOVA output as SPSS does, i.e., Type III Anova
#' @param formula A formula.
#' @param DATA data
#' @keywords spss anova
#' @export
#' @examples
#' spssanova(formula = "p1 ~ conditie", DATA = md)

spssanova <- function(formula, DATA) {

  # http://www.statscanbefun.com/rblog/2015/8/27/ensuring-r-generates-the-same-anova-f-values-as-spss

  options(contrasts = c("contr.helmert", "contr.poly"))   # Set contrast coding to contr.helmert
  linear.model <- lm(as.formula(formula), data=DATA)
  type3_anova <- car::Anova(linear.model, type = 3)
  options(contrasts=c("contr.treatment","contr.poly"))    # Set contrast coding to contr.treatment

  return(type3_anova)
}
