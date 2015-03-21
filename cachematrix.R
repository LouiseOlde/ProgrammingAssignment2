## These functions are used to cache results of a function, then look it up again.  

## Need to do this first, to access the MASS package to do get "ginv", 'generalized inverse of a matrix' function.
library(MASS)


## This function creates a special matrix, which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the values of the inverse of the matrix
# get the values of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {       
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setginv <- function(ginv) m <<- ginv
  getginv <- function() m
  list(set = set, get = get,
       setginv = setginv,
       getginv = getginv)
}


## First need to run above code on matrix, and assign it to a new object.
# Then this function will first check if the inverse of the matrix has already been 
# calculated. If so, it retrieves the cached inverse. If not, it calculates the inverse.


cacheSolve <- function(x, ...) {
  m <- x$getginv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setginv(m)
  m
}

