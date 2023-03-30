## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # check if x is a matrix
  if (!is.matrix(x)) {
    stop("Input is not a matrix.")
  }
  
  inverse_it <- NULL
  set <- function(y) {
    x <<- y
    inverse_it <<- NULL
  }
  get <- function() x
  setinverse_iterse <- function(inverse_iterse) inverse_it <<- inverse_iterse
  getinverse_iterse <- function() inverse_it
  list(set = set, get = get,
       setinverse_iterse = setinverse_iterse,
       getinverse_iterse = getinverse_iterse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse_iterse of 'x'
  inverse_it <- x$getinverse_iterse()
  if(!is.null(inverse_it)) {
    message("getting cached matrix inverse_iterse")
    return(inverse_it)
  }
  element <- x$get()
  inverse_it <- solve(element, ...)
  x$setinverse_iterse(inverse_it)
  inverse_it
}
