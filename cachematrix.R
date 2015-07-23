## Functions that are used to create a special object that stores a matrix and cache's its inverse

## Creates a special "matrix", which is really a list containing a function to set/get the value of the matrix and set/get its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Calculates the inverse of the special "matrix" or retrieves it in case it has been cached
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinv(i)
      i
}
