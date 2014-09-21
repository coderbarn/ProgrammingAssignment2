## The two functions below (makeCacheMatrix and cachemean) are the two functions that are to be
## implemented for Assignment 2 of the R Programming course offered by Coursera

## The makeCacheMatrix function creates a special "Matrix" which is really a list containing a function to
##     1) set the value of the matrix
##     2) get the value of the matrix
##     3) set the value of the inverse of the matrix
##     4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) invM <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() invM
  
  #returns a labeled vector of functions set, get, setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special matrix created with the makeCacheMatrix, it first checks to see
## if the inverse has already been created, if so it gets the inverse from the cache and returns it without
## calculating the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getinverse()
  
  # attempts to get the mean from x (if it was calculated previously)
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  else{
    message("Not in cache")
  }
  
  # The matrix was not cached so get it and return set and return inverse
  data <- x$get()
  
  # Calculate the inverse
  invM <- solve(data, ...)
  
  # Set the inverse
  x$setinverse(invM)
  
  # Return the inverse
  invM
  
}
