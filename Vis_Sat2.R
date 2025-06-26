## Get the LOS and dc function for the sataliets receivers. 
## The most important function is is_covered

# Load required packages
#install.packages("geosphere")  # Install if not already installed
#library(geosphere)
#library(parallel)
#library(data.table)

# # Example set of sensors (satellites)
# s <- data.table(
#   latitude = runif(80, -90, 90),  # Random latitudes for 80 sensors
#   longitude = runif(80, -180, 180),  # Random longitudes for 80 sensors
#   height = runif(80, 400, 600)  # Random altitudes between 400 and 600 km
# )
# s[, sensor_id := .I]
# # Example set of points in the search space (replace with real data)
# p <- data.table(
#   latitude = runif(12000, -90, 90),
#   longitude = runif(12000, -180, 180),
#   height = rep(c(600, 1800, 3000), each = 400)  # Altitude at ground level
# )

is_covered <- function(sensor, point) {
  # Earth's radius in km
  earth_radius <- 6371
  
  # Convert the point's height from meters to kilometers
  point_height_km <- point$height / 1000
  
  # Convert latitudes and longitudes to radians for trigonometric calculations
  point_lat_rad <- point$latitude * pi / 180
  point_lon_rad <- point$longitude * pi / 180
  sensor_lat_rad <- sensor$latitude * pi / 180
  sensor_lon_rad <- sensor$longitude * pi / 180
  
  # Calculate the (x, y, z) coordinates of the point and sensor in ECEF (Earth-Centered, Earth-Fixed)
  point_x <- (earth_radius + point_height_km) * cos(point_lat_rad) * cos(point_lon_rad)
  point_y <- (earth_radius + point_height_km) * cos(point_lat_rad) * sin(point_lon_rad)
  point_z <- (earth_radius + point_height_km) * sin(point_lat_rad)
  
  sensor_x <- (earth_radius + sensor$height) * cos(sensor_lat_rad) * cos(sensor_lon_rad)
  sensor_y <- (earth_radius + sensor$height) * cos(sensor_lat_rad) * sin(sensor_lon_rad)
  sensor_z <- (earth_radius + sensor$height) * sin(sensor_lat_rad)
  
  # Calculate the Euclidean distance between the point and the sensor
  surface_distance <- sqrt((point_x - sensor_x)^2 + (point_y - sensor_y)^2 + (point_z - sensor_z)^2)
  
  # Calculate the horizon distance based on the sensor's height
  horizon_distance <- sqrt(2 * earth_radius * sensor$height + sensor$height^2)
  
  # Debug print statements to verify calculations
  cat(sprintf("Sensor Height (km): %.2f, Surface Distance (km): %.2f, Horizon Distance (km): %.2f\n",
              sensor$height, surface_distance, horizon_distance))
  
  # Check if the point is within the coverage area of the sensor
  return(surface_distance <= horizon_distance)
}



get_visible_sensors <- function(point) {
  # Apply the `is_covered` function to create a logical vector of visible sensors
  visibility_vector <- sapply(1:nrow(s), function(j) is_covered(s[j], point))
  
  # Debugging: Print the logical vector
  cat("Visibility Vector:", visibility_vector, "\n")
  
  # Use the logical vector to filter and return the actual sensor IDs
  visible <- s[visibility_vector, .(sensor_id)]
  
  return(visible$sensor_id)
}




# Parallel setup
cl <- makeCluster(detectCores() - 1)  # Adjust number of cores as needed
clusterExport(cl, c("s", "p", "is_covered", "get_visible_sensors"))  # Export 'p' and other necessary objects
clusterEvalQ(cl, {
  library(data.table)
  library(geosphere)
})





# Apply coverage computation to each point using parallel processing
results <- parLapply(cl, 1:nrow(p), function(i) {
  point <- p[i]
  visible_sensors <- get_visible_sensors(point)
  list(point_id = i, visible_sensors = visible_sensors)
})
stopCluster(cl)



# Organize results into a data table
visible_results <- data.table(point_id = sapply(results, `[[`, "point_id"),
                              visible_sensors = sapply(results, `[[`, "visible_sensors"))

# Print the results (or analyze further)
print(head(visible_results))


####
visible_results[, coverage_count := lengths(visible_sensors)]
p=as.data.frame(p)
p=as.data.table(p)
setnames(p, c("V1", "V2", "V3"), c("latitude", "longitude", "height"))
p[, point_id := .I]
visible_results <- merge(visible_results, p[, .(point_id, latitude, longitude, height)], by = "point_id")
ggplot(visible_results, aes(x = height, y = coverage_count)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Altitude Level (m)", y = "Coverage Count", title = "Coverage Distribution by Altitude") +
  theme_minimal()