library(ggplot2)
library(dplyr)

# Load the data
data <- read.csv("test_labels_final.csv")

# Process the data to identify the start, end, and reassignment years
task_info <- data %>%
  group_by(label) %>%
  summarise(start = min(Year), end = max(Year), Task = first(Task)) %>%
  mutate(Task = as.factor(Task))

# Identify reassigned tasks (task number and first few numbers of the label don't match)
data$label_prefix <- as.numeric(sub("\\(.*", "", data$label))
reassigned_tasks <- data %>%
  filter(Task != label_prefix) %>%
  mutate(Task = as.factor(Task))

# Prepare the plot
ggplot() +
  geom_segment(data=task_info, aes(x=start, xend=end, y=label, yend=label), color="blue") +
  geom_point(data=task_info, aes(x=start, y=label), color="blue", shape=1, size=3) +
  geom_point(data=task_info, aes(x=end, y=label), color="blue", size=3) +
  geom_point(data=reassigned_tasks, 
             aes(x=Year, y=label), color="red", shape=4, size=3) +
  scale_y_discrete(limits = task_info$label) +
  labs(x="Year", y="Task", title="Task Changes and Continuities Over Time") +
  theme_minimal()
