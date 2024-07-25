library(dplyr)
library(forcats)

# Load the dataset
data <- read.csv("edition_third_edition.csv")

# Rename columns for consistency with previous example
colnames(data) <- c("CFR_Part", "Name_of_Part", "Year", "Next_Edition", "Name_of_Part_Next_Edition", "lv_distances", "presumed_same", "same_next_edition")

# Convert Year and Next_Edition to numeric if they are not already
data$Year <- as.numeric(data$Year)
data$Next_Edition <- as.numeric(data$Next_Edition)

# Produce a data frame similar to what we want to do eventually
map <- data.frame(data)

# Combine the columns into a factor label
labels <- map %>% 
  mutate(label1 = paste0("12 CFR ", CFR_Part, " (", Year, ")"),
         label2 = paste0("12 CFR ", same_next_edition, " (", Next_Edition, ")"))

# The tasks are defined by the left column
tasks <- factor(labels$label1)
tasks_2 <- factor(labels$label2)

# Recoding process with debugging information
for (year in unique(map$Year)){
  year_updates <- labels %>% filter(Year == year)
  to_recode <- year_updates$label2
  recode_as <- year_updates$label1
  valid_recode <- !is.na(to_recode) & to_recode %in% levels(tasks)
  to_recode <- to_recode[valid_recode]
  recode_as <- recode_as[valid_recode]
  names(to_recode) <- recode_as
  tasks <- fct_recode(tasks, !!!to_recode)
}

write_csv(year_updates, file = "edition_3_updates.csv")
