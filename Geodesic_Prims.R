# Install the proxy package if not already installed
if (!require("proxy")) install.packages("proxy")

# Load the proxy package
library(proxy)

# Load the data file
file_path <- "C:/Users/Admin/Documents/paper/dantzig42_d.txt"
distances <- as.matrix(read.table(file_path))

# Define a function to calculate the Geodesic distance between two points
geodesic_distance <- function(p1, p2) {
  distGeo(p1, p2)
}

# Define Prim's algorithm function
prims_algorithm <- function(adjacency_matrix) {
  # Initialize variables
  n <- nrow(adjacency_matrix)
  parent <- rep(0, n)
  key <- rep(Inf, n)
  mst_set <- rep(FALSE, n)
  
  # Set the key for the first vertex to 0
  key[1] <- 0
  
  # Main loop
  for (i in 1:(n - 1)) {
    # Find the vertex with the minimum key value
    u <- which.min(key)
    
    # Add the vertex to the MST set
    mst_set[u] <- TRUE
    
    # Update the key values for the adjacent vertices
    for (v in 1:n) {
      if (!mst_set[v] && adjacency_matrix[u, v] < key[v]) {
        key[v] <- adjacency_matrix[u, v]
        parent[v] <- u
      }
    }
  }
  
  # Return the parent array
  return(parent)
}

# Run Prim's algorithm and calculate the time complexity
start_time <- Sys.time()
mst <- prims_algorithm(distances)
end_time <- Sys.time()
time_complexity <- end_time - start_time

# Calculate the space complexity
space_complexity <- object.size(distances) + object.size(mst)

# Print the results
cat("Time complexity:", time_complexity, "\n")
cat("Space complexity:", space_complexity, "\n")
