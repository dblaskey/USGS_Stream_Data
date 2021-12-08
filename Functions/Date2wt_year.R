# This is a function to turn a date column into a Year, Month, Day, 
# and Water Year columns. 
# df is the dataframe
# date_col is the name of the date column as a string
# requires tidyverse to be loaded

Date2wt_year = function(df, date_col){
  temp <- df %>%
    mutate(Date = as.POSIXct(.data[[date_col]])) %>%
    mutate(Year = year(.data[[date_col]]), Month = month(.data[[date_col]]), Day = day(.data[[date_col]])) %>%
    mutate(wt_year = ifelse(as.numeric(Month)>=10, as.numeric(Year) + 1, as.numeric(Year)))
  
  sites <<- temp
}
