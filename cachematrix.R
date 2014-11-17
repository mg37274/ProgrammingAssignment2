## These function return an inverse matrix for an input matrix.

## makeCacheMatrix: Returns a list of functions to set and get the value of 
## a matrix as well set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  setOriginal <- function(newMat) {
     x <<- newMat
     inverse <<- NULL
  }
  getOriginal <- function() { x }
  setInverse <- function(newInverse) {inverse <<- newInverse}
  getInverse <- function() {inverse}
  list(setOriginal = setOriginal, getOriginal = getOriginal,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Returns an inverse matrix of x, obtained by caching or 
## by taking the inverse.

cacheSolve <- function(x, ...) {
  print(x)
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
      message("Loading cached inverse...")
      return(inverse)
  }
  originalMat <- x$getOriginal()
  inverse <- solve(originalMat, ...)
  x$setInverse(inverse)
  inverse
}
