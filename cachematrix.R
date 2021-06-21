## The following two functions will create a special cache matrix that
## stores the value of the matrix and caches its inverse

## This first function will  create a special matrix that is a list that 
## contains the value set as the matrix, getting the value of the matrix, 
## setting the value of the inverse, and then getting the value of the 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) inv <<- solveMatrix
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This next function will calculate the inverse of the cache matrix created
## in the previous function unless it has already been done. This first
## checks to see if the inverse was previously calculated by using 'get' and
## gets the inverse from the cache. If not, it calculates the inverse of the
## data and sets it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("get cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
