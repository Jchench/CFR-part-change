# Load necessary libraries
library(ggplot2)

# Load the CSV file
data <- read.csv("test_labels.csv")

# Prepare data for plotting
tasks <- sort(unique(data$Task))
years <- unique(data$Year)

# Create a data frame to store task positions
task_positions <- data.frame(Task = tasks, Position = seq_along(tasks))

# Merge positions with the original data
data <- merge(data, task_positions, by.x = "Task", by.y = "Task")
names(task_positions)[1] <- "Next.Task"
data <- merge(data, task_positions, by.x = "Next.Task", by.y = "Next.Task", all.x = TRUE, suffixes = c("", ".Next"))

# Plotting
ggplot() +
  # Plot open circles for starting tasks
  geom_point(data = data, aes(x = Year, y = Position), size = 3, shape = 1) +
  # Plot lines for task continuations
  geom_segment(data = data, aes(x = Year, y = Position, xend = Next.Year, yend = Position.Next), color = "blue") +
  # Plot red X's for task changes
  geom_point(data = data[!is.na(data$Next.Task) & data$Task != data$Next.Task, ], 
             aes(x = Next.Year, y = Position.Next), color = "red", shape = 4, size = 3) +
  # Plot closed circles only at the end of the task
  geom_point(data = data[is.na(data$Next.Task) | (!is.na(data$Next.Task) & data$Task != data$Next.Task), ], 
             aes(x = Next.Year, y = Position.Next), size = 3, shape = 16) +
  # Set plot labels and theme
  labs(x = "Year", y = "Task", title = "Task Continuations and Changes Over Years") +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(breaks = seq_along(tasks), labels = tasks) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
