## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## function to cache matrix inverse
  
  i <- NULL
  
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) i <<- solve
  
  getinv <- function() i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  ## function to check is matrix inverse exists
  ## if yes, then print it
  ## if no, then set the value and print it
  
  i <- x$getinv()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinv(i)
  
  i
  
}
