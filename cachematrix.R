## This file contains two functions: makeCacheMatrix and 
## cacheSolve. These function can be used to cache and then retrieve
## the resuts of a solve on a matrix.

## makeCacheMatrix takes an input matrix and creates a variable
## m to hold the inverse of x. It then sets the scope on there outside
## of this function and returns a list of functions that can be used
## to set and get x and to set and get the inverse of x.

## These functions are used together. Call makeCacheMatrix with the 
## input matrix and assign the result to a variable used to reference
## the input in future computations. Passing this to cacheSolve will
## then use a cached value if it exists instead of rerunning the
## calculation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) m <<- inverse_matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks to see if the inverse of x has already
## been computed using the getinverse function created in makeCacheMatrix.
## If it has, the function returns the inverse and prints a message
## indicating that the value is being returned from cache. Otherwise
## the function computes the inverse matrix using solve, puts it in
## the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if (!is.null(inverse_matrix)) {
    message("getting chached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
