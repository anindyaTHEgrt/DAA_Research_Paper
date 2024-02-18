# Load the data file
file_path <- "C:\\Users\\anind\\Downloads\\DAA PAPER\\RLab\\att48_xy.txt"
points <- as.matrix(read.table(file_path))

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

# Define Prim's algorithm function
prims_algorithm <- function(adjacency_matrix) {
  # Initialize variables
  n <- nrow(adjacency_matrix)
  selected <- rep(FALSE, n)
  selected[1] <- TRUE
  result <- matrix(0, nrow=n, ncol=n)
  num_selected <- 1
  
  # Time complexity: O(n^2)
  while (num_selected < n) {
    min_dist <- Inf
    min_row <- NULL
    min_col <- NULL
    for (i in 1:n) {
      if (selected[i]) {
        for (j in 1:n) {
          if (!selected[j] && adjacency_matrix[i, j] < min_dist) {
            min_dist <- adjacency_matrix[i, j]
            min_row <- i
            min_col <- j
          }
        }
      }
    }
    result[min_row, min_col] <- min_dist
    result[min_col, min_row] <- min_dist
    selected[min_col] <- TRUE
    num_selected <- num_selected + 1
  }
  
  return(result)
}

# Track time
start_time <- Sys.time()

# Run Prim's algorithm
min_spanning_tree <- prims_algorithm(adjacency_matrix)

# Track elapsed time
end_time <- Sys.time()
elapsed_time <- end_time - start_time

# Track memory usage
memory_usage <- object.size(min_spanning_tree)

# Print the minimum spanning tree
options(max.print = 1000000) # Set option to display all rows and columns
print(min_spanning_tree)

# Print the elapsed time and memory usage
cat("Elapsed Time (ms):", elapsed_time * 1000, "\n")
cat("Memory Usage (bytes):", memory_usage, "\n")
