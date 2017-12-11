#' A function get Qualtrics data
#'
#' needs survey id, api token, root url, and two optional parameters allow to force to re-download data & to save the data
#' @param surveyid survey id
#' @param apitoken api token
#' @param rooturl root url
#' @param force logical: re-download?
#' @param save logical: save?
#' @keywords get Qualtrics
#' @export
#' @examples
#' get_qualtrics("qmlskdfj","mkjqmdlsfkjsd","url",force = FALSE, save = TRUE)

get_qualtrics <- function(surveyid, apitoken, rooturl, force = FALSE, save = FALSE){
  library(qualtRics)
  qualtrics.variables <- c("ResponseID", "ResponseSet", "IPAddress", "StartDate","EndDate","RecipientLastName","RecipientFirstName","RecipientEmail","ExternalDataReference","Finished","Status","LocationLatitude","LocationLongitude","LocationAccuracy")
  registerOptions(api_token=apitoken, root_url=rooturl, useLabels= F)
  rd <- getSurvey(surveyid, force_request = force)

  non.qualtrics.variables <- names(rd)[names(rd) %nin% qualtrics.variables]
  rd1 <- rd[, qualtrics.variables[c(1,4,5,10:13)]]
  names(rd1) <- c("id","start","end","finished","status","latitude","longitude")
  rd2 <- rd[, non.qualtrics.variables]
  subject <- c(1:length(rd1$id))

  rawdata <- as_tibble(cbind(subject,rd1,rd2))

  if (save){
    save(rd, file="rawdata.rdata")
    wd <- getwd()
    print(paste("saved in: ",wd, sep=""))
  }

  return(rawdata)


}





