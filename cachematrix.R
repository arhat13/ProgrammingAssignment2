## OVERALL DESCRIPTION OF FUNCTIONS:
## Since matrix inversion computations can be costly there can be a benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions cache the inverse of the matrix.

## The makeCacheMatrix function creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL ## create empty matrix for storing cache
  
  ## set the value of the matrix
  set <- function(y) { 
    x <<- y
    inverseMatrix <<- NULL
  }

  ## get the value of the matrix
  get <- function () {  
    x
  }
  
  ## set the value of the inverse of the matrix
  setInverse <- function(inverse) {    
    inverseMatrix <<- inverse
  }
  
  ## get the value of the inverse of the matrix
  getInverse <- function() {   
    inverseMatrix
  }
  
  ## return the list of functions so that they can be used in the global scope
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function computes inverse of the special "matrix"
## NOTE: If inverse has already been calculated (and matrix hasn't changed) then 
## function should retrieve the inverse from the cache instead of recalculating
cacheSolve <- function(x, ...) {
  
  ## gets current inverse matrix value of input matrix and stores
  inverseMatrix <- x$getInverse() 
  
  ##check to see if the inverse has already been calculated
  ## if it has, return the cached matrix, instead of executing additional code
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## if the inverse hasn't been calculated, put the matrix data in variable 'data'
  data <- x$get()
  
  ## compute the inverse of the data using the solve() function and store
  inverseMatrix <-solve(data)
  
  ## call the function to cache the inverse
  x$setInverse(inverseMatrix)
  
  ## return the inverse
  inverseMatrix
}
