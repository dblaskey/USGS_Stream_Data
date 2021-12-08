Summerize_USGS_Data = function (df_path, site_col_name, date_col, Q_col){
  
  load(df_path)
  
  Summerized_data <<- final_sites %>%
    group_by(.data[[site_col_name]]) %>%
    summarise(start_date = min(.data[[date_col]]), end_date=max(.data[[date_col]]), 
              Max = max(.data[[Q_col]]), Min = min(.data[[Q_col]]), 
              Std = sd(.data[[Q_col]]), Percentile_25th = quantile(.data[[Q_col]], 0.25),
              Percentile_50th = quantile(.data[[Q_col]], 0.50),
              Percentile_75th = quantile(.data[[Q_col]], 0.75), Obs = n()) %>%
    mutate(Record_length = ceiling((end_date - start_date)/365.25))
}