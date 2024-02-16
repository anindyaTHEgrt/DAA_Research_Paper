# Install the proxy package if not already installed
if (!require("proxy")) install.packages("proxy")

# Load the proxy package
library(proxy)

# Load the data file
file_path <- "C:/Users/Admin/Documents/paper/dantzig42_d.txt"
distances <- as.matrix(read.table(file_path))

# Define a function to calculate the Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}

# Define Floyd's algorithm function
floyds_algorithm <- function(adjacency_matrix) {
  # Initialize the distance matrix
  n <- nrow(adjacency_matrix)
  distance_matrix <- adjacency_matrix
  for (k in 1:n) {
    for (i in 1:n) {
      for (j in 1:n) {
        distance_matrix[i, j] <- min(distance_matrix[i, j], distance_matrix[i, k] + distance_matrix[k, j])
      }
    }
  }
  return(distance_matrix)
}

# Run Floyd's algorithm and calculate the time complexity
start_time <- Sys.time()
distance_matrix <- floyds_algorithm(distances)
end_time <- Sys.time()
time_complexity <- end_time - start_time

# Calculate the space complexity
space_complexity <- object.size(distances) + object.size(distance_matrix)

# Print the results
cat("Time complexity:", time_complexity, "\n")
cat("Space complexity:", space_complexity, "\n")
