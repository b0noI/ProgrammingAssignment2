## functions calculating solve for matrix (with caching). 
## If solve already calculated for matrix it will be returned from cache
## If user set's new matrix, cache will be dropped


## makeCacheMatrix - create object with 4 functions:
## set - set matrix (also deleting cached solve)
## get - get matrix (getting matrix)
## setsolve - save solve to cache
## getsolve - load solve from cache
## default input of function - matrix()
makeCacheMatrix <- function(matrix = matrix()) {
  solve <- NULL
  ## set new matrix and clear cahce
  set <- function(new_matrix) {
    matrix <<- new_matrix
    solve <<- NULL
  }
  
  ## get matrix
  get <- function() matrix
  
  ## saving solve to cache
  setsolve <- function(new_solve) solve <<- new_solve
  
  ## getting solve from cache
  getsolve <- function() solve
  list(set = set, 
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}


## cacheSolve will try to read matrix solve from cache and if fails it will calculate solve and save it to cache
## input - tuple that can be created with makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## reading solve from cache
  solve <- x$getsolve()
  ## checking solve from cache
  if(!is.null(solve)) {
    return(solve)
  }
  matrix <- x$get()
  solve <- solve(matrix, ...)
  ## setting solve to cache
  x$setsolve(solve)
  solve
}
