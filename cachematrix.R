## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # create a function to cache the inverse of x
  cached_inv <- NULL
  set <- function(y) {
    x <<- y
    cached_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) cached_inv <<- inverse
  getinv <- function() cached_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function caches the inverse of a matrix

cacheSolve <- function(x, ...) {
  # get the cached inverse if it exists
  cached_inv <- x$getinv()
  if (!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  # if it doesn't exist, calculate the inverse and cache it
  data <- x$get()
  cached_inv <- solve(data, ...)
  x$setinv(cached_inv)
  cached_inv
}

