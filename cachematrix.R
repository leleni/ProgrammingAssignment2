## The objective of this script is to calculate the inverse of a matrix

## makeCacheMatrix initialises objects and then defines behaviours for objects that are getting and setting values
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## the cacheSolve calls the makeCacheMatrix to get the values and decided whether they are stored in cache
##if yes then retrieves the cached values if not the calculates the new inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
  
}
