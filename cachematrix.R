# This function creates CacheMatrix Object
# For instance to create a random CacheMatrix of 10 rows and 10 columns just type:
# myMatrix <- makeCacheMatrix(matrix(rexp(100, rate=.1), ncol=10))
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

# This function computes, and stores the solve of an object of type makeCacheMatrix
# For instance to solve your previously created matrix just type
# cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
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