#' A function to read qualtrics CSV files
#'
#' You need to pass on the name of the csv file
#' @param x csv filename
#' @keywords read Qualtrics
#' @export
#' @examples
#' read_qualtrics("data.csv")

read_qualtrics <- function(x){
  headers <- as_tibble(read.csv(x)[1,])
  csv     <- as_tibble(read.csv(x, skip = 3, header = F))
  names(csv) <- names(headers)

  csv <- select(csv , -IPAddress,-Progress,-RecordedDate,-ResponseId,-RecipientLastName,-RecipientFirstName,-RecipientEmail,-ExternalReference,-DistributionChannel,-UserLanguage)
  setnames(csv, old = c("StartDate","EndDate","Status","Duration..in.seconds.","Finished","LocationLatitude","LocationLongitude"),
           new = c("start","end","status","duration","finished","latitude","longitude"))
  subject <- 1:length(csv$start)
  csv <- as_tibble(cbind(subject,csv))
  return(csv)
}
