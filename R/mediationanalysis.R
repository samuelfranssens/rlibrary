#' Simple mediation analysis
#'
#' @param y dependent variable
#' @param mediator mediator
#' @param x_variables independent variable(s)
#' @param data dataset
#' @param simulate calculate CIs via bootstrapping?
#' @param numberofsimulations number of bootstraps
#' @param seed set seed to get the same results
#' @param alpha significance level
#' @keywords mediation
#' @export
#' @examples
#' mediationanalysis(y = "tour", mediator = "control", x_variables = c("imagery_appeal","x2"), simulate = TRUE, data = data)

mediationanalysis <- function(y, mediator, x_variables, data, simulate = FALSE, numberofsimulations = 5000, seed = 123, alpha = .95){ #

  # constant
  tdi <- c("total","direct","indirect")

  # x variable names
  x_variables_names <- paste0("x",seq(length(x_variables)))

  # select variables and rename
  data <- data %>% select(!!y, !!mediator, !!x_variables) %>%
    magrittr::set_names(c("y","mediator",x_variables_names))

  # equations
  eq1 <- paste0("mediator ~ ", paste(x_variables_names, collapse = "*"))
  eq2 <- paste0("y ~ ",paste(x_variables_names, collapse = "*"))
  eq3 <- paste0("y ~ ",paste(x_variables_names, collapse = "*")," + mediator")

  # OBSERVED
  observed_eq1 <- lm(eq1, data = data) %>% broom::tidy()
  observed_eq2 <- lm(eq2, data = data) %>% broom::tidy()
  observed_eq3 <- lm(eq3, data = data) %>% broom::tidy()

  B <- observed_eq3 %>% filter(term == "mediator") %>% pull(estimate)

  observed <- tibble(effect = tdi,
                     observed = c(observed_eq2 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate),
                                  observed_eq3 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate),
                                  (observed_eq1 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate)) * B))

  # simulate
  if(simulate){
    set.seed(seed)
    output <- tibble(simulation = seq(numberofsimulations), total = NA, direct = NA, indirect = NA)

    for (j in seq(numberofsimulations)) {

      # generate simulated data
      simulation <- sample_n(tbl = data, size = nrow(data), replace = TRUE)

      # calculate results for simulated data
      simulated_eq1 <- lm(eq1, data = simulation) %>% broom::tidy()
      simulated_eq2 <- lm(eq2, data = simulation) %>% broom::tidy()
      simulated_eq3 <- lm(eq3, data = simulation) %>% broom::tidy()

      B <- simulated_eq3 %>% filter(term == "mediator") %>% pull(estimate)

      output[j, tdi] <- c(simulated_eq2 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate),
                          simulated_eq3 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate),
                          (simulated_eq1 %>% filter(grepl("x1", term)) %>% slice(1) %>% pull(estimate)) * B)
      # re-iterate
      if(j %% (numberofsimulations/10) == 0){print(j/numberofsimulations*100)}
    }

    # summarize results of simulation
    LB <- (1-alpha)/2
    UB <- 1-LB

    summary <- output %>%
      pivot_longer(cols = c(total,direct,indirect),
                   names_to = "effect",
                   values_to = "value") %>%
      group_by(effect) %>%
      summarize(estimate = mean(value),
                lb = quantile(value, LB), ub = quantile(value, UB),
                count = n(),
                belowzero = sum(value<0)/count, abovezero = 1-belowzero) %>%
      ungroup() %>%
      mutate(significant = case_when(lb<0 & ub>0 ~ "not significant",
                                     TRUE ~ "significant"),
             p = case_when(sign(estimate)<0 ~ abovezero*2,
                           sign(estimate)>0 ~ belowzero*2))


    observed <- left_join(observed,summary)
  }
  return(observed)
}
