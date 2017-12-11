#' Boxplots and barplots for two X variables
#'
#' @param y dependent variable
#' @param x1 independent variable
#' @param x2 another independent variable
#' @param graph.type boxplot or barplot
#' @keywords barplot boxplot
#' @export
#' @examples
#' onefactor(data$y, data$x, graph.type = "box")

twofactors <- function(y,x1,x2, graph.type = "bar"){
  means <- tapply(y,list(x1,x2),mean)
  sds   <- tapply(y,list(x1,x2),sd)
  n     <- tapply(y,list(x1,x2),length)

  levels1 <- length(levels(factor(x1)))
  levels2 <- length(levels(factor(x2)))

  text.df <- tibble(x1 = rep(levels(as.factor(x1)),levels2),
                    x2 = rep(levels(as.factor(x2)),each = levels1),
                    means = as.vector(means),
                    sd = as.vector(sds),
                    ns = as.vector(n),
                    ci = 1.96 * sd / sqrt(ns),
                    lwr = means - ci,
                    upr = means + ci,
                    d = 0,
                    p = 0)

  dataset <- as_tibble(cbind(as.factor(x1),as.factor(x2),as.numeric(y)))

  # make the graph: boxplot
  if (graph.type == "box"){
    graph <- ggplot(aes(y = y, x = x1, group = x1), data=dataset) + facet_wrap(~x2)
    graph <- boxplot(graph, y = dataset$y) +
      geom_point(data = text.df, aes(x = x1, y = means), colour=colors$fill.mean, shape=18, size=7) +
      geom_text (data = text.df, aes(x = x1, y = means, label=round(means,2)), colour = colors$text.mean, hjust = -0.8, fontface="bold", size = 5, inherit.aes=FALSE)
  }

  # make the graph: barplot
  if (graph.type == "bar"){
    graph <- ggplot(aes(y = means, x = x2, ymax=(round(means,0)+1), fill = x1), data = text.df)
    graph <- barplot(graph, lwr = text.df$lwr, upr = text.df$upr)
    if (levels1 == 3){
      graph <- graph + scale_fill_manual(values = c(colors$bar1,colors$bar2,colors$bar3))
    }
  }

  # only if the first factor has two levels and a simple contrast can be computed
  if (levels1 == 2) {
    for (i in 1:levels2){ # for each level of 2: calculate d & p
      # 0 1 = main effect factor 1
      # levels2-1 times zero for main effects factor 2
      # levels2-1 times zero for interactions
      k <- matrix(c(0,1,rep(0,levels2-1),rep(0,levels2-1)),1)

      if (i>1){ # if not first level
        x <- 2 + levels2-1 + i-1
        k[x] <- 1
        #print(k)
      }

      t <- glht(aov(y ~ x1*x2), linfct=k)
      b <- (i-1)*2 + 1
      e <- b+1
      text.df$d[b:e] <- summary(t)$test$tstat[1] * sqrt(1/n[b] +1/n[e])
      text.df$p[b:e] <- summary(t)$test$pvalues[1]
    }

    if (graph.type == "bar"){
      graph <- graph +
        geom_text(data=text.df, aes(x = x2,  y = min(y)+0.2, label=paste("d = ",round(d,2),", p = ",round(p,3),sep="")), color = colors$text.d, fontface="bold", size = 5, inherit.aes=FALSE) +
        scale_fill_manual(values = c(colors$bar1, colors$bar2))
    }
    if (graph.type == "box"){
      graph <- graph + geom_text(data=text.df, aes(x = 1.5, y = min(y)+0.2, label=paste("d = ",round(d,2),", p = ",round(p,3),sep="")), color = colors$text.d, fontface="bold", size = 5, inherit.aes=FALSE)
    }
  }

  return(graph)
}
