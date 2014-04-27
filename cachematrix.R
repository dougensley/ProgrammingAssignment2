## The functions below use a special "cached matrix" to avoid repeating 
## onerous matrix calculations associated with finding the inverse
## of the matrix. In particular, when using a "cached matrix", before computing
## the inverse (when asked), we will return a saved answer if this request
## has ever been asked before, and we will only compute an inverse for a
## matrix that we are being asked for the first time.

## The function makeCacheMatrix creates a special "cached matrix", 
## as a list containing functions to
## (1) set/get the value of the matrix, and
## (2) set/get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special
## "cached matrix" created with makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. 
## If so, cacheSolve gets the inverse from the cache and skips the computation. 
## Otherwise, it cacheSolve computes the inverse of the data and 
## caches the value of the inverse in the "cached matrix."

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}