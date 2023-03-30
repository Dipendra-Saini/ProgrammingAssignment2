## Create a special "matrix" object that can cache its inverse
## 'matrix' is a square invertible matrix
make_cache_matrix <- function(matrix = matrix()) {
  inv <- NULL
  
  set_matrix <- function(new_matrix) {
    matrix <<- new_matrix
    inv <<- NULL
  }
  
  get_matrix <- function() {
    matrix
  }
  
  set_inv <- function(new_inv) {
    inv <<- new_inv
  }
  
  get_inv <- function() {
    inv
  }
  
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Compute the inverse of the special "matrix" returned by make_cache_matrix
## If the inverse has already been calculated (and the matrix has not changed), then retrieve the inverse from the cache
cache_solve <- function(cache_matrix, ...) {
  inv <- cache_matrix$get_inv()
  
  if (!is.null(inv)) {
    message("Retrieving cached data.")
    return(inv)
  }
  
  matrix <- cache_matrix$get_matrix()
  inv <- solve(matrix, ...)
  cache_matrix$set_inv(inv)
  inv
}
