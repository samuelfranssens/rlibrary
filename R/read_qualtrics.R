#' A function to read qualtrics CSV files
#'
#' You need to pass on the name of the csv file
#' @param x csv filename
#' @keywords read Qualtrics
#' @export
#' @examples
#' read_qualtrics("data.csv")

read_qualtrics <- function(x){
  require(dplyr)
  csv <- dplyr::select(x , -IPAddress,-Progress,-RecordedDate,-ResponseId,-RecipientLastName,-RecipientFirstName,-RecipientEmail,-ExternalReference,-DistributionChannel,-UserLanguage)
  setnames(csv, old = c("StartDate","EndDate","Status","Duration..in.seconds.","Finished","LocationLatitude","LocationLongitude"),
           new = c("start","end","status","duration","finished","latitude","longitude"))
  csv <- as_tibble(csv)
  return(csv)
}
