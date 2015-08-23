
## There are two main functions - makeCacheMatrix: creates a matrix 
## and cacheSolve: computes inverse of matrix

## returns a list with these functions set, get, setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { 
    x }
  setinv <- function(inverse) { 
    inv <<- inverse }
  getinv <- function() { 
    inv }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## computes the inverse of matrix returned by makeCacheMatrix.if already calculated, 
## result is returned from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
