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

# Define Kruskal's algorithm function
kruskals_algorithm <- function(adjacency_matrix) {
  # Create an empty list to store the edges of the MST
  mst <- list()
  
  # Get the number of vertices in the graph
  n <- nrow(adjacency_matrix)
  
  # Initialize a vector to keep track of the parent of each vertex
  parent <- 1:n
  
  # Sort the edges of the graph by weight
  edges <- cbind(as.vector(upper.tri(adjacency_matrix, diag = TRUE)),
                 as.vector(adjacency_matrix))
  edges <- edges[order(edges[, 2]),]
  
  # Main loop
  for (i in 1:nrow(edges)) {
    # Get the indices of the two vertices connected by this edge
    u <- as.numeric(edges[i, 1])
    if(u %in% parent) {
      v <- as.numeric(which(parent == u))
      
      # If the two vertices are in different connected components, add this edge to the MST
      if (u != v) {
        mst <- c(mst, list(c(u, v)))
        # Update the parent of the second vertex to the parent of the first vertex
        parent[v] <- parent[u]
      }
    }
  }
  
  # Return the MST
  return(mst)
}

# Run Kruskal's algorithm and calculate the time complexity
start_time <- Sys.time()
mst <- kruskals_algorithm(distances)
end_time <- Sys.time()
time_complexity <- end_time - start_time

# Calculate the space complexity
space_complexity <- object.size(distances) + object.size(mst)

# Print the results
cat("Time complexity:", time_complexity, "\n")
cat("Space complexity:", space_complexity, "\n")
