## Matrix Inversion Caching by Lovi Raj Gupta
## There are two functions makeCacheMatrix and cacheSolve

## creates a special “matrix” object that can cache its inverse using set, get, setmean, getmean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## m is assigned a NULL value
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Checks the cache: If inverse available - prints the cached value, otherwise computes inverse afresh

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        ## checks if is already available in cache or not, if available returns the cached data
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
        ## if the data is NOT available in chache calculates the inverse afresh
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m 
}
