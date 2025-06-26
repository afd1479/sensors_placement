# Get how many sensors covers each point from Space and Ground receivers
# First Plots the K-coverage by Ground,Space and both togothor - Where the points scale from low altitude to high altitude. 
# Second plots the heat-map of coverage where x-axis is the lon and y-axis is lat for the three sets as figure one

library(ggplot2)
library(reshape2)

total_sensors <- 101
ground_sensors <- 21
space_sensors <- total_sensors - ground_sensors

# Initialize result storage
result <- data.frame(Point = integer(), Space_Sensors = integer(), Ground_Sensors = integer())

# Iterate over each point in dc
for (i in seq_along(dc)) {
  point_data <- dc[[i]]
  
  # Determine if sensors are space or ground and count non-NA entries per sensor
  space_sensors_seen <- sum(rowSums(!is.na(point_data[1:space_sensors, ])) == 3)
  ground_sensors_seen <- sum(rowSums(!is.na(point_data[(space_sensors+1):total_sensors, ])) == 3)
  
  # Store the results
  result <- rbind(result, data.frame(Point = i, Space_Sensors = space_sensors_seen, Ground_Sensors = ground_sensors_seen))
}


# Create combined column
result$Total_Sensors <- result$Space_Sensors + result$Ground_Sensors
###########


# Melt the data frame for ggplot
melted_result <- melt(result, id.vars = "Point")

# Plot heat maps
ggplot(data = melted_result, aes(x = Point, y = variable, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heat Maps of Sensor Coverage", x = "Point", y = "Sensor Type") +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal()

#############

# Convert point array to data frame and add point index
point_locations <- as.data.frame(p)
colnames(point_locations) <- c("v1", "v2", "v3")
point_locations$Point <- 1:nrow(point_locations)

# Merge result with point location data
merged_result <- merge(point_locations, result, by = "Point")

# Melt the data frame for ggplot
melted_result <- melt(merged_result, id.vars = c("Point", "v1", "v2", "v3"))

# Plot heat maps
ggplot(data = melted_result, aes(x = v2, y = v1, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heat Maps of Sensor Coverage", x = "Longitude", y = "Latitude") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()