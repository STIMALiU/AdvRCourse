library(parallel)

# Example function
euclidian <- function(a, b){
  while(b != 0){
    r <- b
    b <- a %% b
    a <- r
  }
  a
}

# The values to apply the calculation to:
a <- 1:1000

# Serial calculation:
res1 <- lapply(a, euclidian, b=33)
print(unlist(res))

# Check the number of cores on the computer
cores <- parallel::detectCores()
cores

# Using mcapply()
# Run the function in parallel
res2 <- parallel::mclapply(a, euclidian, b=33, mc.cores = cores)


# Using parLapply()
# Set up the ’cluster’
cl <- makeCluster(cores, type = "PSOCK")
# Parallel calculation (parLapply):
res3 <- parLapply(cl, a, euclidian, b=33)
# Shut down cluster
stopCluster(cl)
