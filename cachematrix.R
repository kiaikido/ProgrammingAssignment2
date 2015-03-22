##This file contains two functions that can help dealing with the matrix and its
##inverse it efficiently by caching the inverse of a matrix rather than computing it 
##repeatedly.


## makeCacheMatrix takes one argument x being a matrix and create a special object that
##can cache a matrix and its inverse. The function returnes a list of functions to
#set and get the matrix and to set and get its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inversem) inverse <<- inversem
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve: returnes the inverse 
#of the special "matrix" object returned by makeCacheMatrix. 
#In the case when the inverse is already computed and the matrix has not changed,
#then the inverse matrix from the cash is returned. 

cacheSolve <- function(x, ...) {
        
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached the inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
