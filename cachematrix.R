## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    M <<- solve(inverse)
  } 
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
        ## Return a matrix that is the inverse of 'x'
}

# Example usage
A <- matrix(c(5, 2, 8, 3), nrow = 2)
cacheA <- makeCacheMatrix(A)
cacheSolve(cacheA)
