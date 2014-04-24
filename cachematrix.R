## Put comments here that give an overall description of what your
## functions do
## There are two funcions:
## makeCacheMatrix() caches the inverse of the given matrix. The inverse matrix is
## exposed via get, set .. functions

## cacheSolve() computes the inverse of the given matrix

## Write a short comment describing this function

## Testing commit

makeCacheMatrix <- function(x = matrix()) {
  
  cached_inv_mat <- NULL

  set <- function(y) {
    x <<- y
    cached_inv_mat <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse_matrix) cached_inv_mat <<- inverse_matrix
  
  getinv <- function() cached_inv_mat
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  
  inv <- solve(matrix)
  
  x$setinv(inv)
  
  inv
  
}
