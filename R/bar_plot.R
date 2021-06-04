#' bar plot ggplot2
#'
#' @param plot plot where mean is dependent variable
#' @param theme add theme?
#' @keywords make a bar plot
#' @export
#' @examples
#' plot %>% bar_plot

bar_plot <- function(plot, theme = TRUE){
  plot <- plot +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(mean,2)), fontface = "bold", position = position_dodge(0.9), vjust = 3, color="black") +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.3, size = 1, position = position_dodge(0.9), color = "black", alpha = .95)

  if(theme){plot %>% add_theme()} else {plot}
}
