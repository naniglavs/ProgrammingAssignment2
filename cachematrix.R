## The following two functions makeCacheMatrix and cacheSolve, cache the inverse of a matrix


## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Set the inverse to NULL
  inv <- NULL
  
  # This function sets the matrix and sets inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # This function gets the matrix
  get <- function() x
  
  # This function sets the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # This function gets the inverse of the matrix
  getinverse <- function() inv
  
  # Return the list with the set, get, setinverse,
  # getinverse functions that are to be used by cacheSolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix which is returned by the function 
## makeCachematrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  # Get the inverse from x
  inv <- x$getinverse()
  
  # Check if the inverse exists already. If so, return the inverse.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)    
  }
  
  # Following code runs if no previous inverse exists
  
  # Get the matrix
  data <- x$get()
  
  # Calculate the inverse of the matrix (assuming matrix is convertible)
  inv <- solve(data, ...)
  message("calculating the inverse")
  
  # Set the inverse
  x$setinverse(inv)
  
  ## Return the inverse matrix
  inv
    
}
