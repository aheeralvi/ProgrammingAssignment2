## Put comments here that give an overall description of what your
## functions do

## set and get functions for matrix and its inverse, returned in a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invarg) inv <<- invarg
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to either calculate and store inverse in cache, or retrieve cached
#  inverses from previous calculations

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(x, ...)
        x$setinv(inv)
        inv
}
