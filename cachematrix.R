## Functions for creating and using inverted matrices which caching ability


## Creates cacheable matrix for inputting to cacheSolve() function which
## sets and gets the cached values

makeCacheMatrix <- function(x = matrix()) {
  
  #Check if we have a valid matrix
  if (!is.matrix(x)) {
    stop("Please give a matrix")
  }
  
  inv.x <- NULL
  
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) inv.x <<- solve
  getinverse <- function() inv.x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.x <- x$getinverse()
  if(!is.null(inv.x)) {
    message("getting cached data")
    return(inv.x)
  }
  data <- x$get()
  inv.x <- solve(data, ...)
  x$setinverse(inv.x)
  inv.x
}
