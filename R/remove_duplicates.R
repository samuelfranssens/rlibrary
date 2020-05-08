#' A function that removes duplicates from a dataset
#'
#'
#' It requires the following columns: dataset$experiment (optional), dataset$subject, dataset$MID
#'
#' data$experiment =1 for the experiment that happened first chronologically, = 2 for the experiment that happened second, ...
#'
#' dataset$subject = 1 for the subject that participated first chronologically, = 2 for the subject that participated second, ...
#'
#' dataset$MID refers to mturk (or other) IDs
#' @param dataset A dataframe with experiment (optional), subject, MID as variables.
#' @keywords duplicates
#' @export
#' @examples
#' remove_duplicates(dataset)

remove_duplicates <- function(dataset){

  library(tidyverse)

  if ("experiment" %in% names(dataset)){
    # dataset <- dataset[order(dataset$experiment,dataset$subject),]
    dataset <- dataset %>% arrange(experiment, subject) %>% mutate(duplicate = 0)
  } else {
    dataset <- dataset %>% arrange(subject) %>% mutate(duplicate = 0)
  }

  for (i in 2:nrow(dataset)){
    j <- i-1
    before <- dataset$MID[c(1:j)]
    if (dataset$MID[i] %in% before){
      dataset$duplicate[i] <- 1
    }
  }
  return(dataset)
}
