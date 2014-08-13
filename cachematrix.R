## This is a system for caching the solution for inversing matrices.
## The solution is cached by creating a special cache matrix that
## contains the source matrix as well as the cached solution.
## If the cache exists, that solution will be used, otherwise
## it will create the cache and store it for future use.

## makeCacheMatrix creates a special cache matrix "object" out
## of the given matrix.  This cache matrix will contain the source
## matrix as well as the cached solution.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function will first try to find the cached solution to the
## inversion. If that fails it will then calculate the solution and cache it.
## It returns the solution to the inversion.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
