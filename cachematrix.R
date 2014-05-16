## R Programming Assignment #2
##
## Creates a special matrix with functions to set and get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  invertedX <- NULL
  set <- function(y) {
    x <<- y
    invertedX <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invertedX <<- inv
  getinverse <- function() invertedX
  #make set* / get* functions accessible
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Creates and/or returns the cached inverted matrix
cacheSolve <- function(x, ...) {
  invertedX <- x$getinverse()
## Check if inverse is already cached and, if so, return it
  if(!is.null(invertedX)) {
    message("Retrieving cached inverse...")
    return(invertedX)
## Else invert the matrix, cache and return it
  } else { 
    invertedX <- solve(x$get())
    x$setinverse(invertedX, ...)
    return(invertedX) 
  }
}


