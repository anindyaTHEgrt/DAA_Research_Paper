# Function to read data from file
read_coordinate_data <- function(file_path) {
  data <- read.table(file_path)
  return(data)
}

# Function to convert Euclidean coordinates to latitude and longitude
euclidean_to_geographic <- function(euclidean_coords) {
  # Example conversion function
  # You may need to replace this with a proper conversion method based on your data
  latitude <- euclidean_coords[, 1] * 0.01  # Example conversion
  longitude <- euclidean_coords[, 2] * 0.01  # Example conversion
  return(data.frame(latitude = latitude, longitude = longitude))
}

# Function to compute Haversine distance between two points
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 6371  # Radius of the Earth in kilometers
  
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}

# Bellman-Ford algorithm using Haversine distance method
bellman_ford_haversine <- function(coords) {
  num_nodes <- nrow(coords)
  
  # Initialize distances
  distances <- rep(Inf, num_nodes)
  distances[1] <- 0  # Assuming the starting node is the first node
  
  # Relax edges repeatedly
  for (i in 1:(num_nodes - 1)) {
    for (u in 1:num_nodes) {
      for (v in 1:num_nodes) {
        if (u != v) {
          lat1 <- coords[u, "latitude"]
          lon1 <- coords[u, "longitude"]
          lat2 <- coords[v, "latitude"]
          lon2 <- coords[v, "longitude"]
          edge_weight <- haversine_distance(lat1, lon1, lat2, lon2)
          
          if (distances[u] + edge_weight < distances[v]) {
            distances[v] <- distances[u] + edge_weight
          }
        }
      }
    }
  }
  
  # Check for negative cycles
  for (u in 1:num_nodes) {
    for (v in 1:num_nodes) {
      if (u != v) {
        lat1 <- coords[u, "latitude"]
        lon1 <- coords[u, "longitude"]
        lat2 <- coords[v, "latitude"]
        lon2 <- coords[v, "longitude"]
        edge_weight <- haversine_distance(lat1, lon1, lat2, lon2)
        
        if (distances[u] + edge_weight < distances[v]) {
          stop("Graph contains negative cycle")
        }
      }
    }
  }
  
  return(distances)
}

# Main function to calculate time and space complexity
main <- function() {
  # Read coordinate data from file
  file_path <- "C:\\Users\\anind\\Downloads\\DAA PAPER\\RLab\\dantzig42_d.txt"
  coordinate_data <- read_coordinate_data(file_path)
  
  # Convert Euclidean coordinates to latitude and longitude
  geographic_coords <- euclidean_to_geographic(coordinate_data)
  
  # Measure time complexity
  start_time <- Sys.time()
  distances <- bellman_ford_haversine(geographic_coords)
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
