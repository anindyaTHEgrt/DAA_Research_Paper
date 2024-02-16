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

# Floyd's algorithm using Haversine distance method
floyd_haversine <- function(coords) {
  num_nodes <- nrow(coords)
  
  # Initialize distance matrix
  distances <- matrix(Inf, nrow = num_nodes, ncol = num_nodes)
  
  # Compute distances between nodes using Haversine distance
  for (i in 1:num_nodes) {
    for (j in 1:num_nodes) {
      if (i != j) {
        lat1 <- coords[i, "latitude"]
        lon1 <- coords[i, "longitude"]
        lat2 <- coords[j, "latitude"]
        lon2 <- coords[j, "longitude"]
        distances[i, j] <- haversine_distance(lat1, lon1, lat2, lon2)
      }
    }
  }
  
  # Apply Floyd's algorithm
  for (k in 1:num_nodes) {
    for (i in 1:num_nodes) {
      for (j in 1:num_nodes) {
        distances[i, j] <- min(distances[i, j], distances[i, k] + distances[k, j])
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
  distances <- floyd_haversine(geographic_coords)
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
