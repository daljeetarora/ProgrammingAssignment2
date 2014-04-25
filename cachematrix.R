## There are two funcions:
## makeCacheMatrix() caches the inverse of the given matrix. The inverse matrix is
## exposed via get, set .. functions



## makeCacheMatrix creates (returns) a list that contains functions to set and/or 
## retrieve the given matrix and its inverse
## It takes a matrix as its argument

makeCacheMatrix <- function(x = matrix()) {
  
    #initialize the vector for caching inverse matrix
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


## cacheSolve computes the inverse of the given matrix returned by makeCacheMatrix
## In case the inverse has already been calculated, it fetches the inverse
## from the cache


cacheSolve <- function(x, ...) {
      
    inv <- x$getinv()
    
    ##Check if cache is available
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## cache not available - compute the inverse, cache it and return the inverse
    matrix <- x$get()
    
    inv <- solve(matrix)
    
    x$setinv(inv)
    
    inv
  
}
