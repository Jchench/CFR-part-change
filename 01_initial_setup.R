library(tidyverse)

# Load the data
data <- read_csv("stacked_data_cleaned.csv")

# Rename columns to match the original example
colnames(data) <- c("CFR.Part", "Name.of.the.Part", "Year", 
                    "Next.Edition", "Next.Name.of.the.Part", "Next.Year", 
                    "lv_distances", "presumed_same", "same_next_edition", "category")

# Setup the data frame similar to the original example
map <- data |>
  select(Year, CFR.Part, Next.Edition, same_next_edition, category) |>
  rename(Task = CFR.Part, Next.Year = Next.Edition, Next.Task = same_next_edition, Category = category)

# Combine the columns into a factor label, handle NA values
labels <- map |>
  mutate(
    label1 = paste0(Task, " (Yr ", Year, ")"),
    label2 = ifelse(is.na(Next.Task), NA, paste0(Next.Task, " (Yr ", Next.Year, ")"))
  )

# The tasks are defined by the left column
tasks <- factor(labels$label1)

# Recode the tasks for each year, handle missing values
for (year in max(labels$Year):min(labels$Year)) {
  year_updates <- labels |> filter(Year == year)
  to_recode <- year_updates$label2[!is.na(year_updates$label2)]
  recode_as <- year_updates$label1[!is.na(year_updates$label2)]
  names(to_recode) <- recode_as
  tasks <- fct_recode(tasks, !!!to_recode)
}

# Prepare the relabeling function
labels$label <- tasks

# Select the final columns to display
final_labels <- labels |> select(Year, Task, label, Category)
final_labels

write_csv(final_labels, file = "labels.csv")
