x <- 1:100
class(x) <- "my_numeric"
print.my_numeric <- function(x, ...){
    cat("This is my numeric vector.\n")
}
print(x)
