## This function is written for the R programming class on Coursera by Timofey Arapov

## This file defines two functions. One performs an inversion of a matrix but f
## first checks to see if this object already exists. The second second solves the matrix
## Passing the outcome of the first function as the argument in the second

makeCacheMatrix <- function(x = matrix()) {
  it_v1 <- NULL
  setter_1 <- function(y){
         x <<- y
         it_v1 <<- NULL
  }
  getter_1 <- function() x
  setinv_matrix <- function(inv1) 
    it_v1 <<- inv1
  getinv_matrix <- function()
    it_v1
  list(setter_1 = setter_1, getter_1 = getter_1,
       setinv_matrix = setinv_matrix, 
       getinv_matrix = getinv_matrix)
}


# Checks to see if the inverse is already cached! If it is gives that value, if not solves it.
# Also saves the cached value.

cacheSolve <- function(x, ...) {
  it_v1 <- x$getinv_matrix()
  if(!is.null(it_v1)){
    message("getting cached data")
    return(it_v1)
  }
  data <- x$getter_1()
  it_v1 <- solve(data, ...)
  x$setinv_matrix(it_v1)
  it_v1
  ## Return a matrix that is the inverse of 'x'
}
