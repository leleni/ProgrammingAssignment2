## The objective of this script is to calculate the inverse of a matrix

## makeCacheMatrix initialises objects and then defines behaviours for objects that are getting and setting values
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##get value
  get <- function() x
  
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve calls the makeCacheMatrix to get the values and decided whether they are stored in cache
##if yes then retrieves the cached values if not the calculates the new inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  
  ##check if the inverse matrix is cached
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  ##calculate inverse matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
