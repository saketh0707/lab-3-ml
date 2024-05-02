# Load necessary libraries
library(ggplot2)
library(cluster)

# Define the function to simulate student features
simulate_student_features <- function(n = 100) {
  # Set the random seed
  set.seed(260923)
  
  # Generate unique student IDs
  student_ids <- seq(1, n)
  
  # Simulate student engagement
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  
  # Simulate student performance
  student_performance <- rnorm(n, mean = 60, sd = 15)
  
  # Combine the data into a data frame
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  
  # Return the data frame
  return(student_features)
}

# Simulate the data
student_data <- simulate_student_features(n = 100)

# Perform dimensionality reduction using PCA
pca_result <- prcomp(student_data[, -1], scale. = TRUE)


# Perform clustering using KMeans
kmeans_clusters <- kmeans(student_data[, -1], centers = 3)

# Add cluster labels to the original data
student_data$cluster <- as.factor(kmeans_clusters$cluster)

# Plot the clusters
ggplot(student_data, aes(x = student_engagement, y = student_performance, color = cluster)) +
  geom_point() +
  labs(title = "KMeans Clustering of Student Features",
       x = "Student Engagement",
       y = "Student Performance") +
  theme_minimal()
