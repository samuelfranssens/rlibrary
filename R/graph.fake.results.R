#' Generate graph based on means (for class)
#'
#' @param a mean
#' @param b mean
#' @param c mean
#' @param d mean
#' @param scale scale, start from 0
#' @param contrast first factor
#' @param context second factor
#' @param dv label
#' @param addlabels add labels
#' @keywords barplot
#' @export
#' @examples
#' graph.fake.results(a = 2, b = 5, c = 7, d = 2, scale = c(0,7), contrast = c("gender","male","female"), context = c("social_influence","absent","present"), "choice")

graph.fake.results <- function(a, b, c=0, d=0, scale, contrast, context = "none", dv, SIZE = 5, addlabels = 0){

  if(context[1] == "none"){ # one factor with two levels
    data <- tibble(contrst = c(contrast[2],contrast[3])) %>%
      mutate(contrst = factor(contrst, levels = contrast[2:3])) %>%
      arrange(contrst) %>%
      mutate(label = c("A","B"),
             dv = c(a,b))
  }

  if(context[1] != "none"){
    data <- tibble(contrst = c(rep(contrast[2],2),rep(contrast[3],2)),
                   contxt = rep(c(context[2],context[3]),2)) %>%
      mutate(contrst = factor(contrst, levels = contrast[2:3]),
             contxt = factor(contxt, levels = context[2:3])) %>%
      arrange(contxt, contrst) %>%
      mutate(label = c("A","B","C","D"),
             dv = c(a,b,c,d))
  }

  if(context[1] == "none"){
    graph <- ggplot(aes(y = dv, x = contrst), data = data) +
      geom_bar(stat="identity", position="dodge", colour="black") +
      geom_text(aes(label=dv   ), fontface="bold", position=position_dodge(.9), size = SIZE, vjust = 1.5) +
      geom_hline(yintercept=0) +
      scale_y_continuous(limits=scale, breaks = seq(scale[2])) +
      theme(axis.title.y = element_text(colour="black", size=SIZE*2.6, face="bold"),
            axis.title.x = element_text(colour="black", size=SIZE*2.6, face="bold"),
            axis.text.y =element_text(colour="black", size=SIZE*2.6,  face="bold"),
            axis.text.x =element_text(colour="black", size=SIZE*2.6,  face="bold"),
            panel.grid.major = element_line(colour = "gray85"),
            panel.grid.minor = element_line(colour = "gray"),
            panel.background = element_rect(fill = "white"),
            panel.spacing = unit(0.1,"lines"),
            strip.text = element_text(size = SIZE*2.6, face = "bold"),
            strip.background = element_rect(fill = "gray"),
            legend.key.size = unit(SIZE/5, "cm"),
            legend.text = element_text(size = SIZE*2.6, face = "bold"),
            legend.title = element_text(size = SIZE*2.6, face = "bold")) +
      xlab("")+
      ylab(dv) +
      scale_fill_brewer(palette = "Set1")
  }

  if(context[1] != "none"){
    graph <- ggplot(aes(y = dv, x = contxt, fill = contrst), data = data) +
      geom_bar(stat="identity", position="dodge", colour="black") +
      geom_text(aes(label=dv   ), fontface="bold", position=position_dodge(.9), size = SIZE, vjust = 1.5) +
      geom_hline(yintercept=0) +
      scale_y_continuous(limits=scale, breaks = seq(scale[2])) +
      theme(axis.title.y = element_text(colour="black", size=SIZE*2.6, face="bold"),
            axis.title.x = element_text(colour="black", size=SIZE*2.6, face="bold"),
            axis.text.y =element_text(colour="black", size=SIZE*2.6,  face="bold"),
            axis.text.x =element_text(colour="black", size=SIZE*2.6,  face="bold"),
            panel.grid.major = element_line(colour = "gray85"),
            panel.grid.minor = element_line(colour = "gray"),
            panel.background = element_rect(fill = "white"),
            panel.spacing = unit(0.1,"lines"),
            strip.text = element_text(size = SIZE*2.6, face = "bold"),
            strip.background = element_rect(fill = "gray"),
            legend.key.size = unit(SIZE/5, "cm"),
            legend.text = element_text(size = SIZE*2.6, face = "bold"),
            legend.title = element_text(size = SIZE*2.6, face = "bold")) +
      xlab(context[1])+
      labs(fill = contrast[1])+
      ylab(dv) +
      scale_fill_brewer(palette = "Set1")
  }


  if(addlabels == 1){
    graph <- graph + geom_text(aes(label=label, y = 0.5), fontface="bold", position=position_dodge(.9), size = SIZE, vjust = -2)
  }

  return(graph)
}
