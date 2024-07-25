# Load necessary libraries
library(dplyr)
library(forcats)

# Read the CSV file
df <- read.csv("cleaning_matched_titles - cleaned.csv")

# Process the data to create the initial labels
df <- df %>%
  mutate(First_Label = paste("Task", CFR.Part, "(Year", Year, ")", sep = " "),
         Second_Label = ifelse(!is.na(same_next_edition),
                               paste("Task", same_next_edition, "(Year", Next.Edition, ")", sep = " "),
                               NA))

# Extract unique years
unique_years <- unique(df$Year)

# Function to recode tasks
recode_tasks <- function(df) {
  tasks <- factor(df$First_Label)  # Initial tasks as factors
  
  for (year in unique_years) {
    year_updates <- df %>% filter(Year == year)
    to_recode <- year_updates$Second_Label
    recode_as <- year_updates$First_Label
    names(to_recode) <- recode_as
    tasks <- fct_recode(tasks, !!!setNames(as.character(to_recode), as.character(recode_as)))
  }
  
  df$Recode_Tasks <- tasks
  return(df)
}

# Apply the function to recode tasks
result <- recode_tasks(df)
print(result)
