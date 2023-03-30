## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  make_inv <- NULL
  set <- function(y) {
    x <<- y
    make_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) make_inv <<- inverse
  getinverse <- function() make_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheinverse <- function(x, ...) {
  make_inv <- x$getinverse()
  if(!is.null(make_inv)) {
    message("getting cached data")
    return(inv)
  }
  invert_mat <- x$get()
  make_inv <- solve(invert_mat, ...)
  x$setinverse(make_inv)
  make_inv
}
