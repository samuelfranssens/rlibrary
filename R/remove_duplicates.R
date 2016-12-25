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
#' dataset <- remove_duplicates(dataset)

remove_duplicates <- function(dataset){
  if ("experiment" %in% names(dataset)){
    dataset <- dataset[order(dataset$experiment,dataset$subject),]
  } else {
    dataset <- dataset[order(dataset$subject),]
  }
  
  dataset$duplicate <- 0
  for (i in 2:length(dataset$subject)){
    j <- i-1
    before <- dataset$MID[c(1:j)]
    if (dataset$MID[i] %in% before){
      dataset$duplicate[i] <- 1
    } 
  }  
  return(dataset)
}