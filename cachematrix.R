## These functions were created for programming assignment 2. 
## It calculates the inverse of a matrix and returns the value.
## If the attempt is made to take the inverse of the same matrix,
## a cached inverse is returned instead of it being computed again.

## Creates a vector of functions that 
## sets the value of the vector,
## gets the value of the vector, 
## sets the value of the inverset (setinverse) by using the solve function
## gets the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse using the funcitons from makeCacheMatrix
## If the inverse was already calculated, return the cached inverse
## Otherwise, compute it using setinverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
