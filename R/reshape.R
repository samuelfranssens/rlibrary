#' A function to convert datasets from wide to long format
#'
#' You need to pass on the dataset and a vector with 'measure.vars', i.e., the wide variables that will become a long variable
#' @param x dataset
#' @param measure.vars wide variables that will become a long variable
#' @keywords wide long format
#' @export
#' @examples
#' wide2long(data, c("measure1","measure2"))

wide2long <- function(x,measure.vars){
  id.vars <- names(x)[names(x) %nin% measure.vars]
  data <- melt(data = x, id.vars = id.vars, measure.vars = measure.vars)
  return(data)
}
