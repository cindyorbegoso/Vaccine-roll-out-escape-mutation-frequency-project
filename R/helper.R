

#define a function to convert the json file (covariant) to a df.

json_to_df <- function(json_path){
  
  data <- fromJSON(json_path)
  
  df_cov <- bind_rows(data, .id = 'location')
  
  list_df <- list()
  i = 1
  for (country in names(data)) {
    df <- as.data.frame(data[[country]])
    df$country <- country
    list_df[[i]] <- df
    i <- i + 1
  }
  
  merged_df <- do.call("rbind", list_df) 
  
  return(merged_df)
}









