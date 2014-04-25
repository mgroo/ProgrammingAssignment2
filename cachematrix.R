## makeCacheMatrix and cacheSolve


## makeCacheMatrix is a getter and setter of the cache and the matrix itself

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinv <- function(mean) m <<- mean
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve will check if the result is cached and return it, if so. 
## If not it it will perform the solve function to inverse the matrix and put it in the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      ##m <- mean(data, ...)
      m <- solve(data, ...)
      x$setinv(m)
      m 
}
