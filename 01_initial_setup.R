library(dplyr)
library(forcats)

# Load the dataset
data <- read.csv("cleaning_matched_titles - cleaned.csv")

# Rename columns for consistency with previous example
colnames(data) <- c("CFR_Part", "Name_of_Part", "Year", "Next_Edition", "Name_of_Part_Next_Edition", "lv_distances", "presumed_same", "same_next_edition")

# Produce a data frame similar to what we want to do eventually
map <- data.frame(data)

# Combine the columns into a factor label
labels <- map %>%
  mutate(label1 = paste0("12 CFR ", CFR_Part, " (", Year, ")"),
         label2 = paste0("12 CFR ", same_next_edition, " (", Next_Edition, ")"))

# The tasks are defined by the left column
tasks <- factor(labels$label1)

# Recoding process
for (year in unique(map$Year)){
  year_updates <- labels %>% filter(Year == year)
  to_recode <- year_updates$label2
  recode_as <- year_updates$label1
  names(to_recode) <- recode_as
  tasks <- fct_recode(tasks, !!!to_recode)
}

# Display the final tasks factor
print(tasks)
