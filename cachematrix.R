##############################################################################
# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store the matrix and a cached value of the inverse of the 
# matrix. 

makeCacheMatrix <- function(x = matrix()) {
  # Set the inv_cach variable to empty initially
  inv_cache <- NULL
  # Store the matrix
  
  set_matrix <- function(y) {
    x <<- y
    # Initially set inv_cache to empty 
    inv_cache <<- NULL
  }
  
  #get value of the matrix
  get_matrix <- function() {
    x
  }
  
  # set the cached value of the inverse of the matrix
  set_inverse <- function(solve) inv_cache <<- solve
  
  # get the cached value of the inverse of the matrix
  get_inverse <- function() inv_cache
  
  # list with named elements which are the functions
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

##############################################################################
# Function to find the inverse of the matrix created by the 
# makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  # get the cached inverse of the matrix
  inverse <- x$get_inverse()
  
  # if a cached inverse exists then return it and print message
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if cached inverse does not exist then calculate it with the solve function
  data <- x$get_matrix()
  inverse <- solve(data)
  x$set_inverse(inverse)
  
  # return the inverse matrix
  inverse
}


##############################################################################
# Not part of the assignment!
# Example matrix to confirm that makeCacheMatrix and cacheSolve both work
matrix_1 <- matrix( c(4, 2, 7, 6), nrow=2, ncol=2) 

matrix_2 <- makeCacheMatrix(matrix_1)
cacheSolve(matrix_2)
cacheSolve(matrix_2)
