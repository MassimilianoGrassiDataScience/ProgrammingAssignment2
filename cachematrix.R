## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize
  i <- NULL
  
  ## set the value of the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setInverse <- function(inverse) i <<- inverse
  
  ## get the value of the inverse
  getInverse <- function() i
  
  ## Return the list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   
}


## Calculates the inverse of the special "matrix" created 
## with the above function makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## Get the inverse Matrix
  i <- x$getInverse()
  
  ## Return the inverse in case it is already been set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  i <- solve(data) 
  
  ## Set the inverse to the object
  x$setInverse(i)
  
  ## Return the matrix
  i
}

