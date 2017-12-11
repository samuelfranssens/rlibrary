#' Boxplots and barplots for one X variable
#'
#' @param y dependent variable
#' @param x1 independent variable
#' @param graph.type boxplot or barplot
#' @keywords barplot boxplot
#' @export
#' @examples
#' onefactor(data$y, data$x, graph.type = "box")

onefactor <- function(y, x1, graph.type = "bar"){
  levels <- length(levels(factor(x1)))

  means <- tapply(y,x1,mean)
  sds   <- tapply(y,x1,sd)
  n     <- tapply(y,x1,length)

  text.df <- tibble(x1 = levels(as.factor(x1)),means = as.vector(means),sd = as.vector(sds),ns = as.vector(n),ci = 1.96 * sd / sqrt(ns),lwr = means - ci,upr = means + ci)
  dataset <- as_tibble(cbind(as.factor(x1),as.numeric(y)))

  # make the graph: boxplot
  if (graph.type == "box"){
    graph <- ggplot(aes(y = y, x = x1, group = x1), data=dataset)
    graph <- boxplot(graph, y = dataset$y)+
      geom_point(data = text.df, aes(x = x1, y = means), colour=colors$fill.mean, shape=18, size=7) +
      geom_text (data = text.df, aes(x = x1, y = means , label=round(means,2)), colour=colors$text.mean, hjust = -0.8, size = 5, fontface="bold", inherit.aes=FALSE)
  } # end of boxplot

  # make the graph: barplot
  if (graph.type == "bar"){
    graph <- ggplot(aes(y = means, x = x1, ymax=(round(means,0)+1)),data = text.df)
    graph <- barplot(graph, lwr = text.df$lwr, upr = text.df$upr) +
      guides(fill = FALSE)
  } # end of barplot

  # only if the first factor has two levels and a simple contrast can be computed
  if (levels == 2) {
    t <- glht(aov(y ~ x1), linfct=matrix(c(0, 1),1))
    d <- summary(t)$test$tstat[1] * sqrt(1/n[1] +1/n[2])
    p <- summary(t)$test$pvalues[1]
    graph <- graph + geom_text(label = paste("d = ",round(d,2),", p = ",round(p,3),sep=""), x = 1.5, y = min(y)+0.5, color = colors$text.d, fontface="bold",size = 5)
  }

  return(graph)
}
