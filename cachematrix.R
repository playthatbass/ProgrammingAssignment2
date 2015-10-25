## The purpose of this R program is to make a list that stores
## the matrix pass into the program and to inverse the matrix,
## assuming the matrix is inversable.

## makeCacheMatrix allows the user to pass a matrix and store it as
## a list. If the user decides to call the cachesolve function,
## makeCacheMatrix will store the inversed matrix into the list.
## To get the original or the inversed matrix, the user would
## simply call variableName$get or variableName$getInverse.

makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inve) i <<- inve
  getInverse <- function() i
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## The cacheSolve function allows the user to inverse the matrix
## store in the list created by makeCacheMatrix. If there already
## is a inversed matrix, the "if" clause will print out a message
## and return the stored inverse matrix, namely variableName$getInverse()

cacheSolve <- function(x, ...){
  
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("getting cached inverse matrix")
    return(inve)
  }
  
  data <- x$get()
  inve <- solve(data, ...)
  x$setInverse(inve)
  inve
  
}
