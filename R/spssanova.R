#' A function that generates the same ANOVA output as SPSS does.
#' This is a BETA version.
#' This will set your contrast coding to treatment
#' This will give a different output when first assigning the linear model to an object and then applying the function (e.g., z <- lm(y~x) ; spssanova(z))
#' vs when not assigning the linear model to an object (e.g., spssanova(lm(y~x)))
#' I don't know why this happens, but the second method gives the same output as SPSS
#' @param linearmodelfit A linear model
#' @keywords spss anova
#' @export
#' @examples
#' spssanova(lm(y ~ x, data))

spssanova <- function(linearmodelfit) {
  # initial_contrastcoding <- getOption("contrasts")      # Get the initial contrast coding
  options(contrasts=c("contr.sum","contr.poly"))          # Set contrast coding to contr.sum
  type3anova <- drop1(linearmodelfit,~.,test="F")         # Type III ANOVA
  options(contrasts=c("contr.treatment","contr.poly"))    # Set contrast coding to contr.treatment
  # options(contrasts=initial_contrastcoding)             # Reset to initial contrast coding

  return(type3anova)
}
