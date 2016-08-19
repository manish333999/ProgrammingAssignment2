## Put comments here that give an overall description of what your
## functions doatrixc an
## The following set of functions achieve computing the inverse of a matrix and caching it 
## and retrieving the cached value without resorting to re-computation.

## Write a short comment describing this function
## This function creates a special 'matrix' whihc is really a list 
## containing a function to set and get the value of the matrix and set and get the 
## value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  
  im <- NULL ## im is the object holding the inverse of the matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) im <<- inverseMatrix
  getInverse <- function() im
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## This function returns the inverse of the matrix supplied to it. It uses 
## caching to retrieve the inverse of the matrix in case it is already computed previously.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached matrix data")
    return(im)
  }
  data <- x$get()
  im <- solve(data,...)
  x$setInverse(im)
  im
}
