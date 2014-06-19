## 'cacheSolve' computes and caches the inverse of a given matrix. 
## It must be used with 'makeCacheMatrix' which creates a list 
## containing all functions needed for caching.



## 'makeCacheMatrix' creates a list containing functions for 
## caching the inverse of a matrix. 
##Use 'set' to set the matrix which should be inverted.
##Use 'get' to get the matrix
##Use 'setinv' to cache the inverted matrix
##Use 'getinv' to get the cached invreted matrix (or NULL in case of empty cache)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve' checks if the given input is already processed and cached.
## if yes it returns the cached value, else it computes the value and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
