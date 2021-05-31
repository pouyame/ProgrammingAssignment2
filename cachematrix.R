## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
      invr <- NULL
      set <- function(y) {
            mat <<- y
            invr <<- NULL
      }
      get <- function() mat
      setinv <- function(inverse) invr <<- inverse
      getinv <- function() invr
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invr <- x$getinv()
      if(!is.null(invr)) {
            message("getting cached inverse")
            return(invr)
      }
      mat <- x$get()
      invr <- solve(mat)
      x$setinv(invr)
      invr
}
