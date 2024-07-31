library(tidyverse)

activity <- 
  read_csv("actions.csv")

labels <- 
  read_csv("labels.csv")

labels_activity <- 
  left_join(labels, activity, by = c("Task", "Year"))

activity_type <- 
  labels_activity |>
  filter(!is.na(Action)) |> 
  mutate(TaskLabel = paste("CFR 12 ", sub(" .*", "", label), 
                           " (", sub(".*Yr (\\d{4}).*", "\\1", label), ")", sep = "")) |> 
  select(-Category)

write_csv(activity_type, file = "activity_type.csv")
