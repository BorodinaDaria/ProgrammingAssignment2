##  This functions cache the inverse of a matrix.

## This function creates a special 'matrix' object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## 'makeCacheMatrix' contains 4 functions: 'set', 'get', 'set_inv' and 'get_inv'.
  inv <- NULL

  ## 'set' stores the input value 'y' in the matrix 'x' into the 'makeCacheMatrix' function 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 'get' returns the matrix 'x' stored in the 'makeCacheMatrix' function 
  get <- function() x
  
  ## 'set_inv' stores the input value 'inverse' in the inverse matrix 'inv' into the 'makeCacheMatrix' function 
  set_inv <- function(inverse) inv <<- inverse
  
  ## 'get_inv' returns the inverse matrix 'inv' stored in the 'makeCacheMatrix' function 
  get_inv <- function() inv
  
  ## save all functions as a list
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)    
}

## This function computes the inverse of the special 'matrix'.
cacheSolve <- function(x, ...) {
  ## try to find inverse matrix in the cache
  inv <- x$get_inv()
  
  ## if it exists in the cache, return the message and the value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if it don't exist, get the matrix stored with 'makeCacheMatrix'
  data <- x$get()
  
  ## calculate the inverse matrix and store it in the cache
  inv <- solve(data, ...)
  x$set_inv(inv)
  
  ## return the inverse matrix
  inv
}
