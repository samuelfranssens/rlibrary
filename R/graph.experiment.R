#' Boxplots and barplots for two X variables
#'
#' @param y dependent variable
#' @param x vector with names of independent variables
#' @param graph.type boxplot or barplot
#' @keywords barplot boxplot
#' @export
#' @examples
#' graph.experiment("control", c("info","imagination"), study3, graph.type = "box")

graph.experiment <- function(y, x, dataset, graph.type = "bar"){

  ivcount <- length(x)

  # select data -------------------------------------------------------------
  data <- dplyr::select(dataset,y,x)
  if (ivcount == 1){
    names(data) <- c("dv","iv1")
    data.grouped <- group_by(data,iv1)
  }
  if (ivcount == 2){
    names(data) <- c("dv","iv1","iv2")
    data.grouped <- group_by(data,iv2,iv1)
  }
  if (ivcount == 3){
    names(data) <- c("dv","iv1","iv2","iv3")
    data.grouped <- group_by(data,iv3,iv2,iv1)
  }
  if (ivcount >3){
    stop("too many independent variables")
  }

  # summarize -------------------------------------------------------------
  summary <- summarise(data.grouped, mean = mean(dv), sd = sd(dv), n = n(), ci = 1.96 * sd/sqrt(n), lwr = mean-ci, upr = mean+ci, d = 0, p = 0, est = 0)
  y.min <- floor(min(data$dv))
  y.max <- ceiling(max(data$dv))
  levels1 <- length(levels(factor(data$iv1))) # number of levels factor 1
  levels2 <- ifelse(ivcount == 2,length(levels(factor(data$iv2))),1) # number of levels factor 2

  # colors -------------------------------------------------------------
  colors <- c("gray","tomato3","orange","red","tomato3")
  names(colors) <- c("fill","outlier","point","text","error")

  # boxplot -------------------------------------------------------------
  if (graph.type == "box"){
    limits <- c(y.min-0.5,y.max+0.5) # boxplot
    breaks  <-  c(y.min:y.max)       # boxplot

    graph <- ggplot(aes(y = dv, x = iv1, group = iv1), data=data)
    if(ivcount == 2){ graph <- graph + facet_wrap(~ iv2) }
    graph <- graph +
      geom_boxplot(fill = "gray", outlier.fill = "tomato3", outlier.shape = 25, outlier.size = 3) +
      geom_jitter (size = 3, height = 0.1, width = 0.2) +
      geom_point(data = summary, aes(x = iv1, y = mean), colour=colors["point"], shape=18, size=7) +
      geom_text (data = summary, aes(x = iv1, y = mean , label=round(mean,2)), colour=colors["text"], hjust = -0.8, size = 5, fontface="bold", inherit.aes=FALSE)
  } # end of boxplot

  # barplot -------------------------------------------------------------
  if (graph.type == "bar"){
    limits <- c(min(y.min,0),y.max+0.5) # barplot
    breaks <- c(min(y.min,0):y.max)     # barplot

    graph <- ggplot(aes(y = mean, x = iv1, ymax=(round(mean,0)+1)), data = summary)
    if(ivcount == 2){ graph <- graph + facet_wrap(~ iv2) }
    graph <- graph +
      geom_bar(stat="identity", position="dodge", colour="black") +
      geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.3, size = 1, position=position_dodge(.9), colour=colors["error"]) +
      geom_text(aes(label=round(mean,2)),colour=colors["text"], fontface="bold", position=position_dodge(.9), size=5, vjust = 3) +
      geom_hline(yintercept=0) +
      theme(legend.title = element_blank())
  } # end of barplot

  # boxplot & barplot -------------------------------------------------------------
  graph <- graph +
    scale_y_continuous(limits=limits, breaks = breaks) +
    theme(axis.title.y = element_text(colour="black", size=13, face="bold"),
          axis.title.x = element_text(colour="black", size=13, face="bold"),
          axis.text.y =element_text(colour="black", size=13,  face="bold"),
          axis.text.x =element_text(colour="black", size=13,  face="bold"),
          panel.grid.major = element_line(colour = "gray85"),
          panel.grid.minor = element_line(colour = "gray"),
          panel.background = element_rect(fill = "white"),
          panel.spacing = unit(0.1,"lines"),
          strip.text = element_text(size = 13, face = "bold"),
          strip.background = element_rect(fill = "gray"),
          legend.position = "none") +
    xlab("")+
    ylab(y)

  # contrasts -------------------------------------------------------------
  # iv2 = facet, so we're testing the effect of iv1 on dv.
  # iv1 needs two levels for contrasts to make intuitive sense
  if (levels1 == 2) {
    for (i in seq(levels2)) { # for each level of iv2: calculate d & p
      # 0 1 = main effect factor 1
      # levels2-1 times zero for main effects factor 2
      # levels2-1 times zero for interactions
      k <- matrix(c(0,1,rep(0,levels2-1),rep(0,levels2-1)),1)

      if (i>1){ # if not first level, update k
        position <- 2 + levels2-1 + i-1
        k[position] <- 1
        #print(k)
      }

      if(ivcount == 1){ t <- glht(aov(dv ~ iv1,data=data), linfct=k) }
      if(ivcount == 2){ t <- glht(aov(dv ~ iv1*iv2,data=data), linfct=k) }

      b <- (i-1)*2 + 1
      e <- b+1
      summary$d[b:e] <- summary(t)$test$tstat[1] * sqrt(1/summary$n[b] +1/summary$n[e])
      summary$p[b:e] <- summary(t)$test$pvalues[1]
      summary$est[b:e] <- summary(t)$test$coefficients
    }
  }

  # update graph with contrasts ---------------------------------------------
  if (levels1 == 2) {

   graph <- graph + geom_text(data=summary, aes(x = 1.5, y = y.min+0.2, label=paste0("d = ",round(d,2),", p = ",round(p,3),", est = ",round(est,2))), color = colors["text"], fontface="bold", size = 5)
  }

  if (ivcount == 2) { # adds the p value of the interaction term
    interaction <- as_tibble(expand.grid(levels(factor(data$iv1)),levels(factor(data$iv2))))
    names(interaction) <- c("iv1","iv2")
    interaction$dv <- y.max - .02
    interaction$label <- ""
    interaction$label[nrow(interaction)] <- paste0("interaction: p = ",round(type3anova(lm(dv ~ iv1 * iv2, data=data))["iv1:iv2",4],3))

    graph + geom_text(data=interaction, inherit.aes = FALSE, aes(x = iv1, y = dv, label=label), color = colors["text"], fontface="bold", size = 5)
  }

  return(graph)
}
