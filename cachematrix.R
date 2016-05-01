## Functions to create a matrix which can cache its inverse

## Generates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.x <- NULL
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv.x <<- inv
  getinv <- function() inv.x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of a matrix produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv.x <- x$getinv()
  if(!is.null(inv.x)) {
    message("Getting cached data...")
    return(inv.x)
  }
  mat <- x$get()
  inv.x <- solve(mat)
  x$setinv(inv.x)
  inv.x 
}
