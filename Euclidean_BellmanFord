# Function to read data from file
read_distance_matrix <- function(file_path) {
  distance_matrix <- as.matrix(read.table(file_path))
  return(distance_matrix)
}

# Bellman-Ford algorithm
bellman_ford <- function(distance_matrix, start_node) {
  num_nodes <- nrow(distance_matrix)
  num_edges <- num_nodes * (num_nodes - 1)
  
  # Initialize distances
  distances <- rep(Inf, num_nodes)
  distances[start_node] <- 0
  
  # Relax edges repeatedly
  for (i in 1:(num_nodes - 1)) {
    for (u in 1:num_nodes) {
      for (v in 1:num_nodes) {
        if (distance_matrix[u, v] != 0) {
          if (distances[u] + distance_matrix[u, v] < distances[v]) {
            distances[v] <- distances[u] + distance_matrix[u, v]
          }
        }
      }
    }
  }
  
  # Check for negative cycles
  for (u in 1:num_nodes) {
    for (v in 1:num_nodes) {
      if (distance_matrix[u, v] != 0) {
        if (distances[u] + distance_matrix[u, v] < distances[v]) {
          stop("Graph contains negative cycle")
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
  
  # Choose start node (could be randomized for more comprehensive analysis)
  start_node <- 1
  
  # Measure time complexity
  start_time <- Sys.time()
  distances <- bellman_ford(distance_matrix, start_node)
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  
  # Measure space complexity
  space_complexity <- object.size(distances) / 1024  # in KB
  
  # Output results
  cat("Distances from start node:", distances, "\n")
  cat("Time taken:", time_taken, "seconds\n")
  cat("Space complexity:", space_complexity, "KB\n")
}

# Call the main function
main()
