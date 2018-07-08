## Functions 'makeCacheMatrix' & 'cacheSolve' cache the inverse of an matrix
## that is assumed to be inverseable.  'cacheSolve calls the 'makeCacheMatrix'
## function to check if the matrix has been inversed already.  If not, matrix
## can be inversed by using the 'solve' function.

## Creates matrix that can cache its inverse
##
## makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInv <- function(inv) i <<- inv
      getInv <- function() i
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## Returns inverse of 'x'
## 
## cacheSolve()

cacheSolve <- function(x, ...) {
      i <- x$getInv()
      if(!is.null(i)) {
            message("Getting the cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInv(i)
      i
}