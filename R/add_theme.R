#' Add theme to a ggplot2 plot
#'
#' @param plot plot
#' @param SIZE size
#' @keywords add theme to a ggplot2 plot
#' @export
#' @examples
#' plot %>% add_theme()

add_theme <- function(plot, SIZE = 13){
  plot +
    theme(axis.title.y = element_text(colour = "black", size = SIZE, face = "bold"),
          axis.title.x = element_text(colour = "black", size = SIZE, face = "bold"),
          axis.text.y =  element_text(colour = "black", size = SIZE, face = "bold"),
          axis.text.x =  element_text(colour = "black", size = SIZE, face = "bold"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.1, "lines"),
          strip.text = element_text(size = SIZE, face = "bold"),
          strip.background = element_rect(fill = "gray"),
          legend.title = element_blank())
}
