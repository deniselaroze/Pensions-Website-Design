


# Updated power calculation function using OLS model
power <- function(rep, esize, N) {
  pv <- rep(NA, rep)  # Initialize p-values storage
  for (i in 1:rep) {
    # Generate the data
    mydata <- data.frame(samegroup = rep(c(TRUE, FALSE), each = N / 2))
    # Add normally distributed errors with sigma = 0.2
    mydata$given <- 4.6 + mydata$samegroup * esize + rnorm(N, mean = 0, sd = 1)
    
    # Fit the linear model
    model <- lm(given ~ samegroup, data = mydata)
    
    # Extract p-value for 'samegroup' coefficient
    p <- summary(model)$coefficients["samegroupTRUE", "Pr(>|t|)"]
    
    # Store p-value
    pv[i] <- p
  }
  # Calculate power
  power <- sum(pv < 0.05) / length(pv)
  return(power)
}

# Single calculation with a fixed N
power(rep = 100, esize = 0.8, N = 30)

# Loop over multiple N values
N <- seq(10, 200, 2)
M <- length(N)
N.power <- rep(NA, M)

for (i in 1:M) {
  N.power[i] <- power(rep = 100, esize = 0.8, N = N[i])
}

# Plot the power calculations
plot(N.power, N / 2, main = "Power calculations", ylab = "Number of obs in each group", xlab = "Power")



###############################
############# Multiple betas
###############################

# Power calculation function using OLS model
# Define the updated power function with OLS model
power <- function(rep, esize, N) {
  pv <- rep(NA, rep)
  for (i in 1:rep) {
    mydata <- data.frame(samegroup = rep(c(TRUE, FALSE), each = N / 2))
    mydata$given <- 4.6 + mydata$samegroup * esize + rnorm(N, mean = 0, sd = 1)
    model <- lm(given ~ samegroup, data = mydata)
    p <- summary(model)$coefficients["samegroupTRUE", "Pr(>|t|)"]
    pv[i] <- p
  }
  power <- sum(pv < 0.05) / length(pv)
  return(power)
}

# Set up parameters for the simulation
N <- seq(10, 100, 2)  # Sample sizes to test
effects <- seq(0.6, 0.9, by = 0.1)  # Different effect sizes to test
M <- length(N)
K <- length(effects)

# Initialize a matrix to store power calculations for each (N, effect size) pair
ne.power <- matrix(NA, nrow = M, ncol = K)

# Calculate power for each effect size and sample size
for (i in 1:M) {
  for (j in 1:K) {
    ne.power[i, j] <- power(rep = 500, esize = effects[j], N = N[i])
  }
}

# Plot the results
par(mfrow = c(3, 2))  # Set up a 3x2 plot layout
colors <- c("red", "blue", "green", "purple", "springgreen4", "grey30")  # Define colors for each plot

for (j in 1:K) {
  plot(ne.power[, j], N / 2, col = colors[j], main = paste("Effect =", effects[j]), 
       xlim = c(0, 1), ylab = "Number of obs per group", xlab = "Power")
}







