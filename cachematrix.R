## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return the inverse of x
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
