## Here are the R functions to implement a caching mechanism for matrix inversion
## functions do: makeCacheMatrix and cacheSolve 

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# Initialize the inverse matrix cache
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to get the inverse of the matrix, either from the cache or by calculating it
  getInverse <- function() {
    if (!is.null(inv)) {
      # If the inverse is already cached, return it
      message("getting cached data")
      return(inv)
    }
    # Otherwise, calculate the inverse and cache it
    message("calculating data")
    inv <- solve(x)
    return(inv)
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

          # Get the cached inverse, if available
  inv <- x$getInverse()
  
  # If the inverse is not cached, calculate it and cache it
  if (!is.null(inv)) {
    x$inv <- inv
  }
  
  # Return the inverse
  inv

}
