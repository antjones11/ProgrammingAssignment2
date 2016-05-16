## These functions create a special matrix object that stores a matrix
## and caches its inverse. This only works for square n x n matrices

## makeCacheMatrix creates a special matrix, which is a list containing
## four functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list (set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## cacheSolve calculates the inverse of the special matrix. It first
## checks to see if the inverse has already been calculated, in which case
## it skips the solve function and returns the inverse from the cache.
## If the inverse is null, it computes it using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
}
