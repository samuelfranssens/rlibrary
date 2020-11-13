#' Taskmaster
#'
#' @param df data frame
#' @keywords taskmaster
#' @export
#' @examples
#' taskmaster(df)

taskmaster <- function(df){

  taskmaster_columns <- c("worktimearray", "tasktime", "offtask", "ontask", "totalofftask", "totalontask", "perpagept", "pageno")
  df <- df %>% drop_na(worktimearray)  # otherwise you get errors

  # extract the data we will use
  my_data <- df %>% select(all_of(taskmaster_columns))
  totalPages <- max(str_count(my_data$perpagept, "PAGE BREAK")) # what is the maximum number of pages?

  ## separate arguments:
  # 1. column to split
  # 2. labels of column (finds total columns based on number of pages in survey)
  # 3. Delimiter

  timingArrays <- my_data %>% # this splits perPagePT into different pages
    separate(perpagept,
             into = paste0("Page_", seq(totalPages)),
             sep = "PAGE BREAK",
             extra = "drop",
             fill = "right") %>%
    select(all_of(paste0("Page_", seq(totalPages))))

  ## Loop through the column for each page and extract additional columns of data (clickAways, timeOffPage, timeOnPage )
  for(i in seq(totalPages)) {

    split_variable <- timingArrays %>% select(all_of(paste0("Page_",i))) %>% set_names("v") # which variable do we need to split

    max_number_vars <- max(str_count(split_variable$v, "\\."))+1

    results <- split_variable %>%
      separate(v, into = paste0("Crossing_",0:max_number_vars), sep = ",", fill = "right") %>%
      select(-Crossing_0) %>%
      mutate_all(.funs = as.numeric)

    # Add to df
    my_data <- my_data %>%
      bind_cols(tibble(timeOnPage = apply(results,  1, function(x) if(all(is.na(x))) {0} else {sum(x[x>0],na.rm = T)}), # sum of positive values
                       timeOffPage = apply(results, 1, function(x) if(all(is.na(x))) {0} else {abs(sum(x[x<0],na.rm = T))}), # sum of negative values
                       clickAways = apply(results,  1, function(x) if(all(is.na(x))) {0} else {sum(!is.na(x))})) %>%  # sum of clicks
                  set_names(paste0("p",i,"_",c("timeonpage","timeoffpage","clickaways"))))

  }

  # return
  df %>%
    select(-all_of(taskmaster_columns)) %>%
    bind_cols(my_data) %>%
    select(-all_of(taskmaster_columns))
}
