#' Boxplots and barplots for two X variables
#'
#' @param y dependent variable
#' @param x vector with names of independent variables
#' @param graph.type boxplot or barplot
#' @keywords barplot boxplot
#' @export
#' @examples
#' graph.experiment("control", c("info","imagination"), study3, graph.type = "box")

graph.interaction <- function(y, x, z, dataset, SIZE = 5){

  ivcount <- length(x) # how many independent variables
  if (ivcount>2){ stop("too many x variables")}

  # select data -------------------------------------------------------------
  data <- dplyr::select(dataset,y,x,z)
  if (ivcount == 1){
    names(data) <- c("dv","iv1","z")
  }
  if (ivcount == 2){
    names(data) <- c("dv","iv1","iv2","z")
  }

  # summarize -------------------------------------------------------------
  y.min <- floor(  min(data$dv))
  y.max <- ceiling(max(data$dv))
  levels1 <- length(levels(factor(data$iv1))) # number of levels of X1
  levels2 <- ifelse(ivcount == 2,length(levels(factor(data$iv2))),1) # number of levels of X2

  # colors -------------------------------------------------------------
  colors <- c("gray","tomato3","orange","red","tomato3")
  names(colors) <- c("fill","outlier","point","text","error")

  # boxplot -------------------------------------------------------------
  limits  <- c(y.min-0.5,y.max+0.5) # boxplot
  breaks  <-  c(y.min:y.max)        # boxplot

  graph <- ggplot(aes(x = z, y = dv, colour = iv1), data=data) +
      geom_jitter (size = SIZE*0.6, height = 0.1, width = 0.2) +
      stat_smooth(method="lm", size=SIZE*0.4)

  if (ivcount == 2){ # if there are more than one independent variables, split up the graph
    graph <- graph + facet_wrap(~ iv2)
  }

  # boxplot & barplot -------------------------------------------------------------
  graph <- graph +
    scale_y_continuous(limits=limits, breaks = breaks) +
    theme(axis.title.y = element_text(colour="black", size=SIZE*2.6, face="bold"),
          axis.title.x = element_text(colour="black", size=SIZE*2.6, face="bold"),
          axis.text.y =element_text(colour="black", size=SIZE*2.6,  face="bold"),
          axis.text.x =element_text(colour="black", size=SIZE*2.6,  face="bold"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.1,"lines"),
          strip.text = element_text(size = SIZE*2.6, face = "bold"),
          strip.background = element_rect(fill = "gray")) +
    xlab(z)+
    ylab(y) +
    labs(color=x[1])

  return(graph)
}
