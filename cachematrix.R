## These two functions are used for storing some matrix and calculating it's inverse matrix
## with caching - so, no need in calculating each time if data is the same (has no changes).
## This approach helps to decrease computation time (especially in a loop).

## This function makes a special matrix; in fact, this is a list of functions,
## that allows to set/to get the value of matrix and to set/to get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## This function is a version of solve function, that has a cache.
## Input arg is the matrix, made by "makeCacheMatrix" function.
## If the inverse matrix of the matrix x has already been calculated (and cached),
## The function will return the cached result. Otherwise the function will do the calculations first,
## then cache the result and then return result.

cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
