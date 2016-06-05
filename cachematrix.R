## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <-NULL
  get <- function() x
  set_inv_matrix <- function(matrix) inv_matrix <<-matrix
  get_inv_matrix <- function() inv_matrix
  
  list(get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_matrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data<- x$get()
  m <- solve(data, ...)
  x$set_inv_matrix(m)
  m
}
