#' A function to read qualtrics CSV files
#'
#' You need to pass on the name of the csv file
#' @param x csv filename
#' @keywords read Qualtrics
#' @export
#' @examples
#' read_qualtrics("data.csv")

read_qualtrics <- function(x){
  csv1 <- read.csv(x, header = TRUE)
  csv2 <- read.csv(x, header = TRUE, skip = 1)
  csv2 <- csv2[-1,]
  names(csv2) <- names(csv1)

  require(dplyr)
  csv <- dplyr::select(csv2 , -IPAddress,-Progress,-RecordedDate,-ResponseId,-RecipientLastName,-RecipientFirstName,-RecipientEmail,-ExternalReference,-DistributionChannel,-UserLanguage)
  setnames(csv, old = c("StartDate","EndDate","Status","Duration..in.seconds.","Finished","LocationLatitude","LocationLongitude"),
           new = c("start","end","status","duration","finished","latitude","longitude"))
  csv <- as_tibble(csv)
  return(csv)
}
