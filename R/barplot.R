#' A helper function for onefactor and twofactor
#'
#' Use onefactor and twofactor instead of this
#' @param bar.arg box.arg
#' @param limits limits
#' @param breaks breaks
#' @keywords boxplot
#' @export
#' @examples
#' barplot(graph, limits, breaks)

barplot <- function(bar.arg, lwr, upr){

  y.min <- floor(min(lwr))
  y.max <- ceiling(max(upr))
  limits <- c(min(y.min,0),y.max+0.5)
  breaks <- c(min(y.min,0):y.max)

  bar.arg <- bar.arg +
    scale_y_continuous(limits=limits, breaks = breaks) +
    geom_bar(stat="identity", position="dodge", colour="black") +
    geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.3, size = 1, position=position_dodge(.9), colour=colors$errorbar) +
    geom_text(aes(label=round(means,2)),colour=colors$text.mean, fontface="bold", position=position_dodge(.9), size=5, vjust = 3) +
    geom_hline(yintercept=0)+
    theme(axis.title.y = element_text(colour="black", size=13, face="bold"),
          axis.title.x = element_text(colour="black", size=13, face="bold"),
          axis.text.y =element_text(colour="black", size=13,  face="bold"),
          axis.text.x =element_text(colour="black", size=13,  face="bold"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.1,"lines"),
          strip.text = element_text(size = 13, face = "bold"),
          strip.background = element_rect(fill = colors$fill.boxplot),
          legend.title = element_blank())

  return(bar.arg)
}
