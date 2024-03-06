
## Input "matrix" is the matrix
## The inverse that will be solved for is set as null
## Everything is structured similarly to the example
## except with "inverse" instead of "mean"
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  
  set <- function(matrix_input) {
    matrix <<- matrix_input
    inverse <<- NULL
  }
  
  get <- function() {
    matrix
  }
  
  set_inverse <- function(inverse_matrix) {
    inverse <<- inverse_matrix
  }
  
  get_inverse <- function() {
    inverse
  }
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This is the same idea with the swtich from the example to inverse
## This is the cacheSolve formula to actually retrieve the inverse of the matrix
cacheSolve <- function(matrix, ...) {
  inverse <- matrix$get_inverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  
  data <- matrix$get()
  inverse <- solve(data, ...)
  matrix$set_inverse(inverse)
  inverse
}




