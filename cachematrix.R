## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix 
##
## Create a matrix whose inverse can be cached when calculated with cacheSolve
## i stores the inverse when if the calculation is performed
## defines 4 methods: set, get, setSolve, getSolve 
##
## Parameters:
## x: matrix to be created
##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setSolve <- function(x) i <<- x
  getSolve <- function() i
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve
##
## Calculate an inverse matrix using R "solve" and cache the result, 
## i.e. don't recaclulate the inverse if it has already been caclulated
## The matrix must have been created by "makeCacheMatrix"
## use methods provided by makeCacheMatrix (get, getSolve, setSolve)
##
## Parameters:
## x: the matrix fro which the inverse has to be calculated
## ... other parameters passed to the solve function
## Returns the inverse matrix of x 
##  
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  return(m)
}
