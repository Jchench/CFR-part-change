library(tidyverse)

# Load the data
data <- read.csv("labels.csv")

# Process the data to identify the start, end, and reassignment years
task_info <- data |>
  group_by(label, Category) |>
  summarise(start = min(Year), end = max(Year), Task = first(Task)) |>
  ungroup() |>
  mutate(Task = as.factor(Task))

# Identify reassigned tasks (task number and first few numbers of the label don't match)
data <- data |> 
  mutate(label_prefix = as.numeric(sub("\\(.*", "", label)),
         Task = as.factor(Task))

reassigned_tasks <- data |>
  filter(Task != label_prefix)

# Creating better y-axis labels
task_info <- task_info |>
  mutate(TaskLabel = paste("CFR 12 ", sub(" .*", "", label), 
                           " (", sub(".*Yr (\\d{4}).*", "\\1", label), ")", sep = ""))

# Visualization
ggplot() +
  geom_segment(data = task_info, aes(x = start, xend = end, y = TaskLabel, yend = TaskLabel),
               size = 1, alpha = 0.50) +
  geom_point(data = task_info, aes(x = start, y = TaskLabel), size = 3, alpha = 0.50) +
  geom_point(data = task_info, aes(x = end, y = TaskLabel), shape = 1, size = 3) +
  geom_point(data = reassigned_tasks, 
             aes(x = Year, y = paste("CFR 12 ", sub(" .*", "", label), 
                                     " (", sub(".*Yr (\\d{4}).*", "\\1", label), ")", sep = "")), 
             shape = 4, size = 3) +
  labs(x = "Year", y = "Task", title = "Task Changes and Continuities Over Time") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 3)) +
  scale_y_discrete(limits = task_info$TaskLabel) +
  facet_wrap(~ Category, scales = "free_y")
