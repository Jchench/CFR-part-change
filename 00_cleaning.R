library(tidyverse)

df <- read_csv("stacked_data.csv - stacked_data_final.csv.csv")

replace_values <- function(df, target_column, reference_column) {
  # Define the values to be replaced
  values_to_replace <- c("?", "??")
  
  # Use mutate and case_when to replace values
  df <- df %>% 
    mutate(!!sym(target_column) := case_when(
      !!sym(target_column) == "–" ~ NA_character_,
      !!sym(target_column) %in% values_to_replace ~ !!sym(reference_column),
      TRUE ~ !!sym(target_column)
    ))
  
  return(df)
}

df <- replace_values(df, "same", "CFR Part")

# categorizing
df <- 
  df %>% 
  mutate(category = case_when(
    str_detect(`Name of the Part`, regex("discounts|advances|open market|purchases|reserves|federal|interest|monetary", ignore_case = TRUE)) ~ "monetary policy",
    str_detect(`Name of the Part`, regex("prudential|capital|risk|liquidity|supervision|leverage|safety", ignore_case = TRUE)) ~ "prudential regulation",
    str_detect(`Name of the Part`, regex("consumer|credit|fair|truth|disclosure|protection|debt|complaints", ignore_case = TRUE)) ~ "consumer regulation",
    TRUE ~ "other"
  ))

write_csv(df, file = "stacked_data_cleaned.csv")
