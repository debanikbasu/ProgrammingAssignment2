##The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {              
      inv <- NULL
      set <- function(y) {                                      ## Create set function to store the matrix passed in the call as x and NULL as inv, both in cache
            x <<- y
            inv <<- NULL
      }
      get <- function() x                                       ## Create function to get/return the matrix passed in the command line call to '$set
      setinverse <- function(inv_matrix) inv <<- inv_matrix     ## Create function to set the value of inv in cache to the value of inv_matrix passed in the call to '$setinverse
      getinverse <- function() inv                              ## Create function to retrieve value of inv from cache and return inv to the caller so we can check it for NULL
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()                                     ## Get the value for inv in the cache environment and put it in a local inv
      if(!is.null(inv)) {
            message("getting cached data")                      ## If inv not NULL, return the value of inv with a message
            return(inv)
      }
      my_matrix <- x$get()                                      ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to my_matrix
      inv <- solve(my_matrix, ...)                              ## Use solve() to invert the my_gmatrix.  Assign the result to inv
      x$setinverse(inv)                                         ## Call nested function x$setinverse() in makeCacheMatrix to set inv in the cache environment to the local non-NULL inverted result in inv
      inv
}
