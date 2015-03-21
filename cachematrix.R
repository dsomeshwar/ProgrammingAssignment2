## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.
## Assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) inv <<- inverse

  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  inv <- x$getinverse()

  ## If inverse already computed then return it from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## Compute inverse and return  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)

  inv  
}
