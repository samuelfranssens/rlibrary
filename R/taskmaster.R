#' Taskmaster
#'
#' @param df data frame
#' @keywords taskmaster
#' @export
#' @examples
#' taskmaster(df)

taskmaster <- function(df){

  taskmaster_columns <- c("worktimearray", "tasktime", "offtask", "ontask", "totalofftask", "totalontask", "perpagept", "pageno")

  my_data <-  df %>%
    select(all_of(taskmaster_columns))

  totalpages <- max(str_count(my_data$perpagept, "page break"))+1 # this calculates the number of pages

  ## separate arguments:
  # 1. column to split
  # 2. labels of column (finds total columns based on number of pages in survey)
  # 3. Delimiter

  arrayColumnNames <- sprintf("page_%d", 1:(totalpages)) # into

  timingArrays <- my_data %>% # this splits perpagept into different pages
    separate(perpagept, into = arrayColumnNames, sep = "page break", extra = "drop", fill = "right") %>%
    select(all_of(arrayColumnNames))

  ## Loop through the column for each page and extract additional columns of data (clickaways, timeoffpage, timeonpage )
  for(i in seq(totalpages)) {

    columnName <- names(timingArrays)[i]
    ## Separate so one there is one time per cell
    boundarycrossings <- timingArrays %>%
      separate_(columnName, sprintf("crossing_%d", 0:max(str_count(timingArrays[,i], "\\."), na.rm = T)), sep = ",", fill = "right") %>%
      select(-starts_with("page"), -"crossing_0") %>%  ## Drop all other columns, ## Drop crossing_0 column (since each vector starts with a ",")
      mutate_at(.vars = vars(matches("crossing")), .funs = as.numeric) ## Convert to numeric

    ## Calculate total time on the page
    timeonpage <-  apply(boundarycrossings, 1, function(x) if(all(is.na(x))) {""} else {sum(x[x>0],na.rm = T)}) # if all is NA, then nothing, else, sum of all that is above zero
    timeoffpage <- apply(boundarycrossings, 1, function(x) if(all(is.na(x))) {""} else {abs(sum(x[x<0],na.rm = T))})
    clickaways <-  apply(boundarycrossings, 1, function(x) if(all(is.na(x))) {""} else {sum(!is.na(x))})

    # Add to df
    my_data <- my_data %>%
      bind_cols(tibble(timeonpage, timeoffpage, clickaways) %>%
                  set_names(paste0("page_",i,"_",c("timeonpage","timeoffpage","clickaways"))))

  }

  # return
  df %>%
    select(-all_of(taskmaster_columns)) %>%
    bind_cols(my_data) %>%
    select(-all_of(taskmaster_columns)) %>%
    mutate_at(.vars = vars(matches("offpage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("onpage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("offpage")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("clickaways")), .funs = as.character) %>%
    mutate_at(.vars = vars(matches("offpage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("onpage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("offpage")), .funs = as.numeric) %>%
    mutate_at(.vars = vars(matches("clickaways")), .funs = as.numeric)
}
