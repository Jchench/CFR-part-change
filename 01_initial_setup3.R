library(dplyr)

# Load the CSV file
matched_titles_df <- read.csv('cleaning_matched_titles - cleaned.csv')

generate_task_flows <- function(df) {
  task_flows <- list()
  
  # Find all unique starting points
  start_points <- df %>% filter(Year == min(Year))
  
  for (i in 1:nrow(start_points)) {
    row <- start_points[i, ]
    current_year <- row$Year
    current_task <- row$CFR.Part
    flow <- list(c(current_year, current_task))
    
    while (TRUE) {
      next_row <- df %>% filter(Year == current_year, CFR.Part == current_task)
      if (nrow(next_row) == 0 || is.na(next_row$same_next_edition)) {
        break
      }
      next_year <- next_row$Next.Edition
      next_task <- next_row$same_next_edition
      flow <- append(flow, list(c(next_year, next_task)))
      current_year <- next_year
      current_task <- next_task
    }
    
    # Fill the flow to the fixed length (in this case 2 steps as per the example)
    if (length(flow) == 1) {
      flow <- append(flow, list(c(NA, NA)))
    } else if (length(flow) == 2) {
      flow <- append(flow, list(c(NA, NA)))
    }
    
    task_flows <- append(task_flows, list(flow))
  }
  
  return(task_flows)
}

# Generate task flows
task_flows <- generate_task_flows(matched_titles_df)

# Create a dataframe similar to the expected output
flow_data <- data.frame(
  Year = numeric(),
  CFR.Part = numeric(),
  `Next.Edition` = numeric(),
  `same_next_edition` = numeric(),
  label1 = character(),
  label2 = character(),
  stringsAsFactors = FALSE
)

for (flow in task_flows) {
  flow_data <- rbind(flow_data, data.frame(
    Year = flow[[1]][1],
    CFR.Part = flow[[1]][2],
    `Next.Edition` = flow[[2]][1],
    `same_next_edition` = flow[[2]][2],
    label1 = paste(flow[[1]][2], "(Yr", flow[[1]][1], ")"),
    label2 = ifelse(is.na(flow[[2]][1]), "NA (Yr 2)", paste(flow[[2]][2], "(Yr", flow[[2]][1], ")"))
  ))
}

# Display the flow_data dataframe
print(flow_data)
