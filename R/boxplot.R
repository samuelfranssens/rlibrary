#' A helper function for onefactor and twofactor
#'
#' Use onefactor and twofactor instead of this
#' @param box.arg box.arg
#' @param y dependent variable
#' @keywords boxplot
#' @export
#' @examples
#' boxplot(graph, limits, breaks)

boxplot <- function(box.arg, y){

  y.min <- floor(min(y))
  y.max <- ceiling(max(y))
  limits <- c(y.min-0.5,y.max+0.5)
  breaks  <-  c(y.min:y.max)

  box.arg <- box.arg +
    scale_y_continuous(limits=limits, breaks = breaks) +
    geom_boxplot(fill = colors$fill.boxplot, outlier.fill = colors$fill.outlier, outlier.shape = 25, outlier.size = 3) +
    geom_jitter (size = 3, height = 0.1, width = 0.2) +
    theme(axis.title.y = element_text(colour="black", size=13, face="bold"),
          axis.title.x = element_text(colour="black", size=13, face="bold"),
          axis.text.y =element_text(colour="black", size=13,  face="bold"),
          axis.text.x =element_text(colour="black", size=13,  face="bold"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.1,"lines"),
          legend.position="none",
          strip.text = element_text(size = 13, face = "bold"),
          strip.background = element_rect(fill = colors$fill.boxplot))

  return(box.arg)
}
