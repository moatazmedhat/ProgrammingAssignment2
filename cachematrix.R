## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that has methods to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse_in) inverse <<- inverse_in
    getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Gets the inverse of a matrix created by makeCacheMatrix from cache if calcualted before or
## else it calculates the inverse and updates the matrix inverse property.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
