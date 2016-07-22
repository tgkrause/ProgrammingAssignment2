## These functions cache the inverse of a matrix so that it doesn't have to be computed
## multiple times because that can be a costly computation.

## Makes a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
      x <<- y
      a <<- NULL
  }

  get <- function() x
  setinverse <- function(solve) a <<- solve
  getinverse <- function() a

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates inverse of matrix returned by MakeCacheMatrix

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)) {
    message("Getting cached data")
    return(a)
  }
  data <- x$get()
  
  a <- solve(data, ...)
  x$setinverse(a)
  a
}