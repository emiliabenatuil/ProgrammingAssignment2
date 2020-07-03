## This function is the one that creates the special matrix, one that will then be cached.
## We will create a list of the functions that have to be performed:
## Setting & Getting the matrix 
## Setting & Getting the inverse of the matrix
## It is also where we will store the cached inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## By using x$getinv() we are trying to retrieve an inverse matrix if it was already calculated
## If there is no previous cache value then we proceed to calculate it
## First we get the data then we use 'solve' to create an inverse matrix
## After that we use x$setinv so that next time we wont have to calculate it, R will retrieve this inverse matrix 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

