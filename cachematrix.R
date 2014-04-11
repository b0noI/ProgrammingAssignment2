
## functions calculating solve for matrix with caching. 
## If solve already calculated for matrix it will be returned from cache
## If user set's new matrix, cache will be dropped


## makeCacheMatrix - create object with 4 fumctions:
## set - set matrix and delete cached solve
## get - get matrix
## setsolve - save solve to cache
## getsolve - load solve from cache
## input of function - matrix()
makeCacheMatrix <- function(x = matrix()) {
  solve <- NULL
  set <- function(y) {
    matrix <<- y
    solve <<- NULL
  }
  get <- function() x
  setsolve <- function(y) solve <<- y
  getsolve <- function() solve
  list(set = set, 
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## cacheSolve will try to read solve for matrix from cache and if fails it will calculate solve and save it to cache
## if it's read solve from cach it's print: "getting cached data"
## input - tuple that can be created with makeCacheMatrix function
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
