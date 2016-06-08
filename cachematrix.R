#Matrix inversion is usually a costly computation 
#both function below work together to reduce the calculation toll of inverting a matrix.

# makeCacheMatrix creates a speacial "matrix" object by whose properties include:
# - Get- this is used to return the result of the matrix that user inputs.
# - set_inv_matrix - this is the critical function that caches our inverted matrix.
# - get_inv_matrix - this is used to return the inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <-NULL
  get <- function() x
  set_inv_matrix <- function(matrix) inv_matrix <<-matrix
  get_inv_matrix <- function() inv_matrix
  
  list(get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}

#CacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not been changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inv_matrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m) #returns the cached result
  }
  
  data<- x$get()
  m <- solve(data, ...) #inverts matrix
  x$set_inv_matrix(m) #cahces the result
  m #returns the inverted matrix to user
}
