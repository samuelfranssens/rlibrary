#' Taskmaster
#'
#' @param df data frame
#' @keywords taskmaster
#' @export
#' @examples
#' taskmaster(df)

taskmaster <- function(df){

  df <- df %>% drop_na(worktimeArray) # otherwise you get errors

  taskmaster_columns <- c("worktimeArray", "tasktime", "offTask", "onTask", "totalOffTask", "totalOnTask", "perPagePT", "pageNo")

  my_data <-  df %>%
    select(all_of(taskmaster_columns))

  totalPages <- max(str_count(my_data$perPagePT, "PAGE BREAK"))+1 # this calculates the number of pages

  ## separate arguments:
  # 1. column to split
  # 2. labels of column (finds total columns based on number of pages in survey)
  # 3. Delimiter

  arrayColumnNames <- sprintf("Page_%d", 1:(totalPages)) # into

  timingArrays <- my_data %>% # this splits perPagePT into different pages
    separate(perPagePT, into = arrayColumnNames, sep = "PAGE BREAK", extra = "drop", fill = "right") %>%
    select(all_of(arrayColumnNames))

  ## Loop through the column for each page and extract additional columns of data (clickAways, timeOffPage, timeOnPage )
  for(i in seq(totalPages)) {

    columnName <- names(timingArrays)[i]
    ## Separate so one there is one time per cell
    boundaryCrossings <- timingArrays %>%
      separate_(columnName, sprintf("Crossing_%d", 0:max(str_count(timingArrays[,i], "\\."), na.rm = T)), sep = ",", fill = "right") %>%
      select(-starts_with("Page"), -"Crossing_0") %>%  ## Drop all other columns, ## Drop Crossing_0 column (since each vector starts with a ",")
      mutate_at(.vars = vars(matches("Crossing")), .funs = as.numeric) ## Convert to numeric

    ## Calculate total time on the page
    timeOnPage <-  apply(boundaryCrossings, 1, function(x) if(all(is.na(x))) {""} else {sum(x[x>0],na.rm = T)}) # if all is NA, then nothing, else, sum of all that is above zero
    timeOffPage <- apply(boundaryCrossings, 1, function(x) if(all(is.na(x))) {""} else {abs(sum(x[x<0],na.rm = T))})
    clickAways <-  apply(boundaryCrossings, 1, function(x) if(all(is.na(x))) {""} else {sum(!is.na(x))})

    # Add to df
    my_data <- my_data %>%
      bind_cols(tibble(timeOnPage, timeOffPage, clickAways) %>%
                  set_names(paste0("Page_",i,"_",c("timeOnPage","timeOffPage","ClickAways"))))

  }

  # return
  df %>%
    select(-all_of(taskmaster_columns)) %>%
    bind_cols(my_data) %>%
    select(-all_of(taskmaster_columns)) %>%
    mutate_at(.vars = vars(matches("OffPage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("OnPage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("OffPage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("ClickAways")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("OffPage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("OnPage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("OffPage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("ClickAways")), .funs = as.numeric)
}
