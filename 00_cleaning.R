library(tidyverse)

replace_values <- function(df, target_column, reference_column) {
  # Define the values to be replaced
  values_to_replace <- c("?", "??")
  
  # Use mutate and case_when to replace values
  df <- df |> 
    mutate(!!sym(target_column) := case_when(
      !!sym(target_column) == "–" ~ NA,
      !!sym(target_column) %in% values_to_replace ~ !!sym(reference_column),
      TRUE ~ !!sym(target_column)
    ))
  
  return(df)
}

# Example usage:
df <- read_csv("stacked_data.csv - stacked_data_final.csv.csv")
df <- replace_values(df, "same", "CFR Part")

write_csv(df, file = "stacked_data_cleaned.csv")
