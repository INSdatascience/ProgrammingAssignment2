## Essential idea is to create in "makeCacheMatrix" a matrix of functions 
## that will be used to calculate matrix inverses and remember them for later use.

## key elements is that the set function resets m to NULL, so anytime a new matrix is inputted, the cache is emptied.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInv <- function(Inv) m <<- Inv
      getInv <- function() m
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## if conditional checks if there is something in m, and if so, print it. If not, then calculate it, cache it then print it.

cacheSolve <- function(x, ...) {
      m <- x$getInv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInv(m)
      m
}