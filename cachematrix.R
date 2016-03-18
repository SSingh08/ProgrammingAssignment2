## While running computations which are time consuming, it's good to cache 
## the results so that you can reuse later instead of re-computing. Example:-
## matrix inversion is usually time consuming when running in loop. The following
## function will compute and cache the inverse of a matrix.

## The first function makeCacheMatrix creates a special "matrix" object that can
## cache its inverse.
## X is a square invertible matrix; function returns list containing functions to
##        1. set the matrix
##        2. get the matrix
##        3. set the inverse
##        4. get the inverse
## this list is used as input to second function cacheSOlve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The second function will compute inverse of the orginal matrix. However,it will
## first check if the inverse has already been computed. If it is already computed
## it will get from the cache and skip the computation

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
}