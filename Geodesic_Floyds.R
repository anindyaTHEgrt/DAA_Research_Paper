# Function to read data from file
read_distance_matrix <- function(file_path) {
  distance_matrix <- as.matrix(read.table(file_path))
  return(distance_matrix)
}

# Floyd's algorithm using Geodesic distance method
floyd_geodesic <- function(distance_matrix) {
  num_nodes <- nrow(distance_matrix)
  
  # Initialize distance matrix
  distances <- distance_matrix
  
  # Apply Floyd's algorithm
  for (k in 1:num_nodes) {
    for (i in 1:num_nodes) {
      for (j in 1:num_nodes) {
        if (distances[i, k] + distances[k, j] < distances[i, j]) {
          distances[i, j] <- distances[i, k] + distances[k, j]
        }
      }
    }
  }
  
  return(distances)
}

# Main function to calculate time and space complexity
main <- function() {
  # Read distance matrix from file
  file_path <- "C:\\Users\\anind\\Downloads\\DAA PAPER\\RLab\\dantzig42_d.txt"
  distance_matrix <- read_distance_matrix(file_path)
  
  # Measure time complexity
  start_time <- Sys.time()
  distances <- floyd_geodesic(distance_matrix)
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  # Measure space complexity
  space_complexity <- object.size(distances) / 1024  # in KB
  
  # Output results
  cat("Distances matrix:", "\n")
  print(distances)
  cat("Time taken:", time_taken, "seconds\n")
  cat("Space complexity:", space_complexity, "KB\n")
}

# Call the main function
main()
