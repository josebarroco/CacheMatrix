## Creation of 2 functions that cache the inverse of a matrix

## creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL
  
  setmat <- function(y){
  
        x <<- y
        z <- NULL
  
  }
  
  getmat <- function() x
  
  setinv <- function(w) z <<- w
  
  getinv <- function() z
  
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)

}


## Computes the inverse of the special matrix returned by the makeCacheMatrix fucntion.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  z <- x$getinv()
  
  if(!is.null(z)) {
    
    message("getting cached data")
    return(z)
    
  }
  
  data <- x$getmat()
  
  z <- solve(data, ...)
  
  x$setinv(z)
  
  z

}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


