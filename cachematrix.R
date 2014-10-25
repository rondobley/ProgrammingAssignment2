## These two function allow you to store a matrix and then use
## that stored matrix to get the inverse of the matrix. If the 
## inverse has been previously calculated it will return the inverse
## that has been cached. If not, it will solve the inverse and cache
## the result so that the calculation does not have to be repeated

## makeCacheMatrix takes an invertible matrix and stores it.
## This stored invertible matrix can be used with cacheSolve to
## solve the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ## inv stores the inverse of the matrix
  inv <- NULL
  
  ## set the matrix to x and clear the inverse
  set <- function(y) {
    # x is the matrix
    x <<- y
    # clear the inverse of the matrix
    inv <<- NULL
  }
  
  ## return the martix stored in x
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(matrixInverse) {
    inv <<- matrixInverse
  }
  
  ## return the inverse of the matrix
  getinverse <- function() inv
  
  ## set the list of functions that can be called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## cacheSolve takes an invertible matrix that has been stored with
## makeCacheMatrix. When called with an invertible matrix that has been stored
## with makeCacheMatrix it will solve the inverse of the matrix and cache the 
## results so that further calls do not have to do the repeat the calculation
cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix stored with makeCacheMatrix
  inv <- x$getinverse()
  ## if the returned value is not null, we have a cached result, so return that
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if cached data was not returned above we need get the data and compute the inverse and set it  
  data <- x$get()
  ## solve the inverse of the matrix
  inv <- solve(data)
  ## set the result so it will be cached
  x$setinverse(inv)
  ## return the result
  inv
}
