## Creates a matrix object that can store its inverse input
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInv <- function(solve) INV <<- solve
  getInv <- function() INV
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
## Returns a matrix that is an inverse of matrix 'x' if it is cached or Null
cacheSolve <- function(x, ...) {
  INV <- x$getInv()
  if (!is.null(INV)) {
    message("function is getting the cached data")
    return(INV)
  }
  mat <- x$get()
  INV <- solve(mat, ...)
  x$setInv(INV)
  INV
}