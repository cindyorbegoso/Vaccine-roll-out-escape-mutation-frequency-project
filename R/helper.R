

#define a function to convert the json file (covariant) to a df.

json_to_df <- function(json_path){
  
  data <- fromJSON(json_path)
  
  df_cov <- bind_rows(data, .id = 'location')

  return(df_cov)
  
}


#define a function to reorganize the df_final

df_reorganise <- function(df_cov,df_vac){
  
  df_cov[df_cov$location=="USA",]$location <- "United States"
  df_cov[df_cov$location=="Czech Republic",]$location <- "Czechia"
  df_cov[df_cov$location=="Sint Maarten",]$location <- "Sint Maarten (Dutch part)"
  df_cov_location <- unique(df_cov$location)
  df_vac_location <- unique(df_vac$location)
  final_list <- intersect(df_cov_location, df_vac_location)
  df_cov_final <- df_cov[is.element(df_cov$location, final_list),]
  df_vac_final <- df_vac[is.element(df_vac$location, final_list),]
  df_final <- inner_join(x = df_cov_final, y = df_vac_final, by= c("location","week"))
  frequency <- df_final$cluster_sequences/df$total_sequences
  df_final <- cbind(df_final, frequency) 
  
  return(df_final)
  
}




filtrado_df <- function(df1, country) {
  df1 <- df1 %>% filter(location == country) 
  return(df1)
}








  
  


















