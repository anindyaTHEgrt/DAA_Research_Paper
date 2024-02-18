# Load the data file
file_path <- "C:\\Users\\anind\\Downloads\\DAA PAPER\\RLab\\att48_xy.txt"
points <- as.matrix(read.table(file_path))
options(max.print = 100)

# Define a function to calculate the Euclidean distance between two points
euclidean_distance <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
}

# Define a function to create an adjacency matrix using Euclidean distances
create_adjacency_matrix <- function(points) {
  n <- nrow(points)
  adjacency_matrix <- matrix(0, nrow=n, ncol=n)
  
  # Time complexity: O(n^2)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      # Euclidean distance calculation
      adjacency_matrix[i, j] <- euclidean_distance(points[i,], points[j,])
      adjacency_matrix[j, i] <- adjacency_matrix[i, j]
    }
  }
  
  return(adjacency_matrix)
}

# Create the adjacency matrix
adjacency_matrix <- create_adjacency_matrix(points)

# Define Dijkstra's algorithm function
dijkstras_algorithm <- function(adjacency_matrix, start_node) {
  # Initialize the distance matrix and visited nodes list
  n <- nrow(adjacency_matrix)
  distance_matrix <- matrix(Inf, nrow=n, ncol=n)
  visited <- rep(FALSE, n)
  
  # Set the distance to the start node to 0
  distance_matrix[start_node, start_node] <- 0
  
  # Time complexity: O(n^2)
  for (i in 1:n) {
    # Find the node with the minimum distance that has not been visited
    min_dist <- Inf
    min_index <- -1
    for (j in 1:n) {
      if (!visited[j] && distance_matrix[start_node, j] < min_dist) {
        min_dist <- distance_matrix[start_node, j]
        min_index <- j
      }
    }
    
    # Update the visited list and distance matrix
    if (min_index != -1) {
      visited[min_index] <- TRUE
      for (j in 1:n) {
        distance_matrix[start_node, j] <- min(distance_matrix[start_node, j], distance_matrix[start_node, min_index] + adjacency_matrix[min_index, j])
      }
    }
  }
  
  return(distance_matrix)
}

# Track time
start_time <- Sys.time()

# Run Dijkstra's algorithm
start_node <- 1
distance_matrix <- dijkstras_algorithm(adjacency_matrix, start_node)

# Track elapsed time
end_time <- Sys.time()
elapsed_time <- end_time - start_time

# Track memory usage
memory_usage <- object.size(distance_matrix)

# Print the distance matrix
print(distance_matrix)


# Print the elapsed time and memory usage
cat("Elapsed Time (ms):", elapsed_time * 1000, "\n")
cat("Memory Usage (bytes):", memory_usage, "\n")
