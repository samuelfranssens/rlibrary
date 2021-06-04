#' Summarize data set
#'
#' @param data data
#' @param yvar variable
#' @keywords summarize
#' @export
#' @examples
#' data %>% summarize_for_graph(measure)

summarize_for_graph <- function(data, yvar){
  yvar <- enquo(yvar)

  data %>%
    drop_na(!!yvar) %>%
    summarize(mean = mean(!!yvar), sd = sd(!!yvar), n = n(), min = min(!!yvar), max = max(!!yvar),
              ci = 1.96 * sd/sqrt(n), lwr = mean - ci,  upr = mean + ci, median = median(!!yvar))
}
