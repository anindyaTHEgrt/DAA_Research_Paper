# Load the data
distances <- as.matrix(read.table("C:/Users/Admin/Documents/paper/dantzig42_d.txt"))

# Function to implement Dijkstra's algorithm
dijkstra <- function(graph, start) {
  n <- nrow(graph)
  visited <- rep(FALSE, n)
  dist <- rep(Inf, n)
  dist[start] <- 0
  
  for (i in 1:(n-1)) {
    # Find the vertex with the minimum distance
    minDist <- min(dist[!visited])
    u <- which.min(dist)
    
    # Mark the vertex as visited
    visited[u] <- TRUE
    
    # Update distances for adjacent vertices
    for (v in 1:n) {
      if (!visited[v] && graph[u, v] > 0) {
        dist[v] <- min(dist[v], dist[u] + graph[u, v])
      }
    }
  }
  
  return(dist)
}

# Execute Dijkstra's algorithm
start_time <- Sys.time()
result <- dijkstra(distances, 1)
end_time <- Sys.time()

# Print the results
print(result)
cat("Time taken:", end_time - start_time, "\n")
cat("Space complexity:", object.size(distances) + object.size(result), "\n")
