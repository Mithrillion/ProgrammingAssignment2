## The following two functions create and manipulate a special "matrix" object
## that caches its inverse

## the MakeCacheMatrix function creates a list that serves as a matrix object
## it includes functions to set or get the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## the cacheSolve function calculates the inverse of a special matrix object if it is not cached,
## or returns the cached inverse if it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  else{
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    return(inv)
  }
}
