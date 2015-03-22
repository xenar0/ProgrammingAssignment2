## A set of functions to be able to 
## - create special "matrix"
## - compute the inverse of the special "matrix"
## - get value from cache, if it was already calculated before

## A function to create special "matrix" with access to its cache
makeCacheMatrix <- function(x = matrix()) {  
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solved) s <<- solved
  getsolved <- function() s
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## A function that computes inverse of the special "matrix"
## For performace purposes it is enhanced with caching calculated value
cacheSolve <- function(x, ...) {
  s <- x$getsolved()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
