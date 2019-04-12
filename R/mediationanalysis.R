#' Simple mediation analysis
#'
#' @param y dependent variable
#' @param mediator mediator
#' @param x independent variable(s)
#' @param effects which effect are we testing
#' @param data dataset
#' @param simulate calculate CIs via bootstrapping?
#' @param numberofsimulations number of bootstraps
#' @param seed set seed to get the same results
#' @param alpha significance level
#' @keywords mediation
#' @export
#' @examples
#' graph.experiment("regard", "impression", "cc", "ccgucci")

mediationanalysis <- function(y, mediator, x, effects, data, simulate = 0, numberofsimulations = 5000, seed = 123, alpha = .95){

  tdi <- c("total","direct","indirect")

  # observed
  eq1 <- lm(paste0(mediator," ~ ",paste(x, collapse = "*")),        data=data) # M ~ X
  eq2 <- lm(paste0(y," ~ ",paste(x, collapse = "*")),               data=data) # Y ~ X
  eq3 <- lm(paste0(y," ~ ",paste(x, collapse = "*")," + ",mediator),data=data) # Y ~ X + M

  B <- coef(eq3)[mediator]

  observed <- tibble(effect = tdi,
                     observed = c(sum(coef(eq2)[ unlist(effects[1]) ]),
                                  sum(coef(eq3)[ unlist(effects[1]) ]),
                                  sum(coef(eq1)[ unlist(effects[1]) ]) * B))

  # simulate
  if(simulate){
    set.seed(seed)
    rownumber <- 1
    output <- tibble(simulation = seq(numberofsimulations), total = NA, direct = NA, indirect = NA)

    for (j in seq(numberofsimulations)) {

      # generate simulated data
      simulation <- data
      for (i in seq(nrow(data))) {
        simulation[i,] <- sample_n(data,1)
      }

      # calculate results for simulated data
      eq1.sim <- lm(paste0(mediator," ~ ",paste(x, collapse = "*")),        data=simulation) # M ~ X
      eq2.sim <- lm(paste0(y," ~ ",paste(x, collapse = "*")),               data=simulation) # Y ~ X
      eq3.sim <- lm(paste0(y," ~ ",paste(x, collapse = "*")," + ",mediator),data=simulation) # Y ~ X + M

      B <- coef(eq3.sim)[mediator]

      output[rownumber,tdi] <- c(sum(coef(eq2.sim)[ unlist(effects[1]) ]),
                                 sum(coef(eq3.sim)[ unlist(effects[1]) ]),
                                 sum(coef(eq1.sim)[ unlist(effects[1]) ]) * B)
      # re-iterate
      rownumber <- rownumber + 1
      if(rownumber %% (numberofsimulations/10) == 0){print(rownumber/numberofsimulations*100)}
    }

    # summarize results of simulation
    UB <- 1-(1-alpha)/2
    LB <- (1-alpha)/2
    summary <- tibble(estimate = c(mean(output$total), mean(output$direct), mean(output$indirect)),
                      lb = c(quantile(output$total,LB), quantile(output$direct,LB), quantile(output$indirect,LB)),
                      ub = c(quantile(output$total,UB), quantile(output$direct,UB), quantile(output$indirect,UB)),
                      belowzero = 0, abovezero = 0,  significant = "", p = 0)
    for (i in seq(nrow(summary))){
      summary$belowzero[i] <- length(which(output[,i+1]<0)) / nrow(output)
      summary$abovezero[i] <- 1-summary$belowzero[i]
      summary$significant[i] <- case_when(between(0, summary$lb[i], summary$ub[i]) ~ "not significant", TRUE ~ "significant")
      summary$p[i] <- case_when(sign(summary$estimate[i])<0 ~ summary$abovezero[i],
                                sign(summary$estimate[i])>0 ~ summary$belowzero[i])
    }

    return(cbind(observed,summary))
  } else {
    return(observed)
  }
}
