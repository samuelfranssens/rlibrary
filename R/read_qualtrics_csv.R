#' Read Qualtrics data CSV files
#'
#' @param filename filename (include .csv extension)
#' @keywords taskmaster
#' @export
#' @examples
#' read_qualtrics_csv("dataset.csv")

read_qualtrics_csv <- function(filename){
  variable_names <- read_csv(filename) %>% names()

  read_csv(filename, skip = 3, col_names = FALSE) %>%
    magrittr::set_names(variable_names) %>%
    select(-c("IPAddress", "Progress", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "DistributionChannel", "UserLanguage")) %>%
    rename(duration = `Duration (in seconds)`)
}
