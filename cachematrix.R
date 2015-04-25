## Overall goal: 
## Avoid unnecessary computations by caching matrix 
## solving results for a given matrix object and check
## for already existing results before starting calculation 

## Part 1: 
## a) initialize comparison objects from function call (parent environment)
## and from cache (current enviromnent)
## b) define methods of this functions
##   get: display input 
##   setInverse: update cache
##   getInverse: get cached result)  
## c) define output as a list 

makeCacheMatrix <- function(x = matrix()) {
## initialize comparison objects
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Part 2:
## check for given matrix x whether cache result exists
## if yes: display cache info and return cache results
## if no:  calculate matrix inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
