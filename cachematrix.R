## makeCacheMatrix is used to cache the inverse of a matrix

## we are going to follow the same procedure as in the example, gonna create get, set, getInverse, setInverse function

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inverse <<- inverse
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function first gonna check if the inverse is null, the inverse is calculated
## using solve(), otherwise the cached value is returned as the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)
  Inverse
}
