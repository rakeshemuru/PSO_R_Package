library(ANTs)

# Define fitness function
fitness <- function(par, iris_data){
  phantom <- par[1]
  w <- par[-1]
  
  iris_phantom <- cbind(iris_data, phantom)
  
  # Calculate distances between each point and the centroid of its class
  distances <- iris_phantom %>%
    group_by(Species) %>%
    summarise_all(mean) %>%
    select(-phantom) %>%
    unlist() %>%
    matrix(ncol = 4, byrow = TRUE) %>%
    apply(2, function(x) {sqrt(rowSums((iris_phantom[, -5] - x)^2))})
  
  # Calculate fitness value
  fitness <- sum(distances * w)
  return(fitness)
}

# Define lower and upper bounds for the variables
lb <- c(-10, rep(-5, 4))
ub <- c(10, rep(5, 4))

# Run the Ant Colony Optimization algorithm
set.seed(123)
result <- ants(n = 20, f = fitness, lb = lb, ub = ub, control = list(maxit = 100, trace = TRUE))

# Print the final fitness value and the values of the phantom variable and weights
cat("Final fitness value: ", result$bestobj, "\n")
cat("Phantom variable: ", result$bestmem[1], "\n")
cat("Weights: ", result$bestmem[-1], "\n")