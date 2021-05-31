## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(mat = matrix()) {
      invr <- NULL    #inerse matrix
      set <- function(y) {
            mat <<- y     #Cached Matrix
            invr <<- NULL #Cached inverse Matrix 'empty'
      }
      #recall matrix
      get <- function() mat     
      # register inverse matrix to cach
      setinv <- function(inverse) invr <<- inverse  
      # recall cached inverse matrix
      getinv <- function() invr
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invr <- x$getinv()  #recall hached matrix
      if(!is.null(invr)) {
            message("getting cached inverse")
            return(invr)
      }
      mat <- x$get()  #recall hached matrix
      #calculate inverse
      invr <- solve(mat)
      # record inverse natrix in hach
      x$setinv(invr)
      invr
}
      
      