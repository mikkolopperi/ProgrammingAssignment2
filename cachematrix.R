## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  set <- function(newmat) {
    mat <<- newmat
    inverse <<- NULL  # Invalidate the cached inverse when the matrix changes
  }
  
  get <- function() mat
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Compute the inverse of a cached matrix
cacheSolve <- function(cacheMatrix) {
  mat <- cacheMatrix$get()
  cachedInverse <- cacheMatrix$getInverse()
  
  if (!is.null(cachedInverse)) {
    message("Getting cached inverse.")
    return(cachedInverse)
  }
  
  inverse <- solve(mat)
  cacheMatrix$setInverse(inverse)
  inverse
}

# Example usage
A <- matrix(c(4, 2, 2, 3), nrow = 2)
cacheA <- makeCacheMatrix(A)

# Calculate and cache the inverse
cacheSolve(cacheA)

# Retrieve the cached inverse
cacheSolve(cacheA)

