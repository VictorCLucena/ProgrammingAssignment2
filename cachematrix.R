## The function receaves a matrix as parameter and cache it

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
  m <- x$getInverse()
  
  ## if there is no change in the matrix, then the cached data is returned
  if(!is.null(m)) {
    message("Getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

## Testing the functions

#matriz <- makeCacheMatrix(matrix(1:4, 2, 2))
#matriz$get()
#matriz$getInverse()
#cacheSolve(matriz)
#cacheSolve(matriz)
#matriz$getInverse()
