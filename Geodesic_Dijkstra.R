# Install the proxy and igraph packages if not already installed
if (!require("proxy")) install.packages("proxy")
if (!require("igraph")) install.packages("igraph")

# Load the proxy and igraph packages
library(proxy)
library(igraph)

# Load the data file
file_path <- "C:/Users/Admin/Documents/paper/dantzig42_d.txt"
distances <- as.matrix(read.table(file_path))

# Define a function to calculate the Geodesic distance between two points
geodesic_distance <- function(p1, p2) {
  distGeo(p1, p2)
}

# Define Dijkstra's algorithm function
dijkstras_algorithm <- function(adjacency_matrix, source) {
  # Initialize variables
  n <- nrow(adjacency_matrix)
  dist <- rep(Inf, n)
  dist[source] <- 0
  visited <- rep(FALSE, n)
  
  # Main loop
  for (i in 1:n) {
    # Find the vertex with the minimum distance from the source that hasn't been visited
    u <- which.min(dist[!visited])
    
    # Mark the vertex as visited
    visited[u] <- TRUE
    
    # Update the distances for the adjacent vertices
    for (v in 1:n) {
      if (!visited[v] && adjacency_matrix[u, v] + dist[u] < dist[v]) {
        dist[v] <- adjacency_matrix[u, v] + dist[u]
      }
    }
  }
  
  # Return the distances array
  return(dist)
}

# Run Dijkstra's algorithm and calculate the time complexity
start_time <- Sys.time()
distances_from_source <- dijkstras_algorithm(distances, 1)
end_time <- Sys.time()
time_complexity <- end_time - start_time

# Calculate the space complexity
space_complexity <- object.size(distances) + object.size(distances_from_source)

# Print the results
cat("Time complexity:", time_complexity, "\n")
cat("Space complexity:", space_complexity, "\n")
