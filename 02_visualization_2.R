library(tidyverse)
library(cowplot)

# Load the data
data <- read.csv("labels.csv")

# Ensure each label has a consistent category
data <- data |> 
  group_by(label) |> 
  mutate(Category = first(Category)) |> 
  ungroup()

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

################ plotting ####################### --------------------------------------

# plot by category
generate_plot <- function(task_info, reassigned_tasks, category, title) {
  filtered_task_info <- task_info |> filter(Category == category)
  filtered_task_info$TaskLabel <- droplevels(factor(filtered_task_info$TaskLabel))
  
  filtered_reassigned_tasks <- reassigned_tasks |> filter(Category == category)
  filtered_reassigned_tasks$label <- droplevels(factor(paste("CFR 12 ", sub(" .*", "", filtered_reassigned_tasks$label), 
                                                             " (", sub(".*Yr (\\d{4}).*", "\\1", filtered_reassigned_tasks$label), ")", sep = "")))
  
  if (nrow(filtered_task_info) == 0 & nrow(filtered_reassigned_tasks) == 0) {
    warning(paste("No data for category:", category))
    return(NULL)
  }
  
  p <- ggplot() +
    geom_segment(data = filtered_task_info, aes(x = start, xend = end, y = TaskLabel, yend = TaskLabel),
                 size = 0.75, alpha = 0.50) +
    geom_point(data = filtered_task_info, aes(x = start, y = TaskLabel), size = 1.5, alpha = 0.50) +
    geom_point(data = filtered_task_info, aes(x = end, y = TaskLabel), shape = 1, size = 1.5) +
    geom_point(data = filtered_reassigned_tasks, 
               aes(x = Year, y = label), 
               shape = 4, size = 3) +
    scale_y_discrete(limits = levels(filtered_task_info$TaskLabel)) +
    xlim(1938, 1997) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(plot.title.position = "panel",
          plot.title = element_text(size = 8, hjust = 0.5),
          axis.text.y = element_text(size = 6),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  return(p)
}

# creating plots
consumer_reg_plot <- 
  generate_plot(task_info, reassigned_tasks, "consumer regulation", "Consumer Regulation") +
  theme(axis.text.x = element_blank())

monetary_pol_plot <- 
  generate_plot(task_info, reassigned_tasks, "monetary policy", "Monetary Policy") +
  theme(axis.text.x = element_blank())

prudential_reg_plot <- 
  generate_plot(task_info, reassigned_tasks, "prudential regulation", "Prudential Regulation") +
  labs(y = "Task") +
  theme(axis.text.x = element_blank())

other_plot <- 
  generate_plot(task_info, reassigned_tasks, "other", "Other") +
  labs(x = "Year")

title <- 
  ggplot() +
  labs(title = "Task Changes and Continuities Over Time") +
  theme_minimal() +
  theme(plot.title.position = "plot")

combined_plot <- 
  plot_grid(title, consumer_reg_plot, monetary_pol_plot, prudential_reg_plot, other_plot,
            ncol = 1, align = 'v', rel_heights = c(0.065, 0.38, 0.38, 0.12, 1.2))

ggsave("combined_plot.jpg", plot = combined_plot, width = 6.5, height = 9)