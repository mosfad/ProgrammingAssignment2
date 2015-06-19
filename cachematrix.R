## The two functions below cache the inverse of a square matrix to prevent 
##costly computations.

## makeCacheMatrix creates an invertible square matrix object that can
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(my_inverse) inv <<- my_inverse
  getinverse <- function( ) inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve computes the inverse of a square invertible matrix or
##retrieves the already computed inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  my_matrix <- x$get()
  inv <- solve(my_matrix, ...)        ##why is ... reqd after my_matrix?
  x$setinverse(inv)
  inv
  
}
