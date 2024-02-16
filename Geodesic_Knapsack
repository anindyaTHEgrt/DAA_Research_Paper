# Function to read data from file
read_knapsack_data <- function(file_path) {
  data <- read.table(file_path)
  weights <- data[, 1]
  values <- data[, 2]
  return(list(weights = weights, values = values))
}

# Knapsack algorithm using Geodesic distance method
knapsack_geodesic <- function(weights, values, capacity) {
  n <- length(weights)
  ratio <- values / weights
  order <- order(ratio, decreasing = TRUE)
  
  total_value <- 0
  total_weight <- 0
  selected_items <- logical(n)
  
  for (i in order) {
    if (total_weight + weights[i] <= capacity) {
      selected_items[i] <- TRUE
      total_weight <- total_weight + weights[i]
      total_value <- total_value + values[i]
    }
  }
  
  return(list(total_value = total_value, selected_items = selected_items))
}

# Main function to calculate time and space complexity
main <- function() {
  # Read knapsack data from file
  file_path <- "C:\\Users\\anind\\Downloads\\DAA PAPER\\RLab\\dantzig42_d.txt"
  knapsack_data <- read_knapsack_data(file_path)
  
  # Initialize capacity for knapsack
  capacity <- 200
  
  # Measure time complexity
  start_time <- Sys.time()
  result <- knapsack_geodesic(knapsack_data$weights, knapsack_data$values, capacity)
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  # Measure space complexity
  space_complexity <- object.size(result) / 1024  # in KB
  
  # Output results
  cat("Total value:", result$total_value, "\n")
  cat("Time taken:", time_taken, "seconds\n")
  cat("Space complexity:", space_complexity, "KB\n")
}

# Call the main function
main()
