library(tidyverse)

df <- read_csv("stacked_data.csv - stacked_data_final.csv.csv")

replace_values <- function(df, target_column, reference_column) {
  # Define the values to be replaced
  values_to_replace <- c("?", "??")
  
  # Use mutate and case_when to replace values
  df <- df |> 
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
  df |> 
  mutate(category = case_when(
    str_detect(`Name of the Part`, regex("discounts|advances|open market|purchases|reserves|federal|interest|monetary", ignore_case = TRUE)) ~ "monetary policy",
    str_detect(`Name of the Part`, regex("prudential|regulation|capital|risk|liquidity|supervision|leverage|safety", ignore_case = TRUE)) ~ "prudential regulation",
    str_detect(`Name of the Part`, regex("consumer|credit|fair|truth|disclosure|protection|debt|complaints", ignore_case = TRUE)) ~ "consumer regulation",
    TRUE ~ "other"
  ))

# add activity type
activity <- 
  read_csv("actions.csv") |> 
  janitor::clean_names() |> 
  mutate(cfr_part = as.character(cfr_part)) |> 
  distinct(cfr_part, year, next_edition, .keep_all = TRUE) |> 
  rename("CFR Part" = cfr_part, Year = year, Next.Edition = next_edition, Action = action)

df2 <- 
  left_join(df, activity, by = c("CFR Part", "Year", "Next.Edition"))

write_csv(df2, file = "stacked_data_cleaned_activity.csv")

